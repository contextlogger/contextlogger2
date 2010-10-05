#lang scheme

;; Here we deal with cross-cutting concerns related to our sensor
;; array, by generating some or all of the cross-cutting code. Join
;; points are defined simply as C function or macro invocations.

(require (lib "usual-4.ss" "common"))
(require (lib "ast-util.scm" "wg"))
(require (lib "file-util.scm" "wg"))
(require (lib "compact.scm" "wg"))
(require (lib "cxx-syntax.ss" "wg"))
(require (lib "settings.scm" "wg")) ;; for in-c-mode
(require (lib "string-util.scm" "wg"))
(require (lib "local-util.scm" "codegen"))
(require "sa_sensor_list_dsl.ss")

(define* (generate sensor-spec dump? gen?)
  (define sensor-list
    (begin
      (unless (sensor-list? sensor-spec)
        (error "sensor list syntax error"))
      (cdr sensor-spec)))
  
  (define (get-sensor-list)
    sensor-list)

  (define active-sensors
    (filter
     (lambda (sensor) (not (fget-opt-nlist-elem-1 sensor 'inactive)))
     sensor-list))
  
  ;;(pretty-nl sensors) (exit)
  
  ;;(define program-name (find-system-path 'run-file))
  
  ;;(write-nl program-name)
  ;;(write-nl (path-drop-extension (path-basename program-name) ".scm"))

  (define (capture-output f)
    (let ((output (open-output-string)))
      (parameterize ((current-output-port output))
        (f))
      (get-output-string output)))

  (define (get-sensor-name sensor)
    (fget-reqd-nlist-elem-1 sensor 'name))

  (define (get-sensor-cpp-condition sensor)
    (fget-opt-nlist-elem-1 sensor 'cpp-condition))

  (define (for-each-statement f)
    (for-each
     (lambda (sensor)
       (let* ((sensor-name (fget-reqd-nlist-elem-1 sensor 'name))
              (cpp-condition (fget-opt-nlist-elem-1 sensor 'cpp-condition))
              (stmt-list (fget-opt-nlist-elem-1up-e sensor 'sql-statements)))
         (for-each
          (lambda (stmt)
            (let* ((stmt-name (if (string? stmt) "" (first stmt)))
                   (stmt-sql (if (string? stmt) stmt (second stmt))))
              (f sensor-name cpp-condition stmt-name stmt-sql)))
          stmt-list)))
     (get-sensor-list)))

  (define (with-cpp-condition-harness cpp-condition f)
    (if cpp-condition
        (begin
          (display "#if ") (display-nl cpp-condition)
          (f)
          (display-nl "#endif"))
        (f)))

  (define create-tables-sql-def
    (capture-output
     (thunk
      (display-nl "static const char create_tables_sql[] =")
      (for-each
       (lambda (sensor)
         ;; During database initialization, we do output code for
         ;; creating tables for all the sensors, even those ones not
         ;; in the particular build, as this will make it easier to
         ;; change the configuration.
         (let* ((cpp-condition #f) ;;(fget-opt-nlist-elem-1 sensor 'cpp-condition)
                (schema (fget-opt-nlist-elem-1up-e sensor 'sql-schema))
                (write-schema (thunk (for-each write-nl schema))))
           (when schema
             (if cpp-condition
                 (begin
                   (display "#if ") (display-nl cpp-condition)
                   (write-schema)
                   (display-nl "#endif"))
                 (write-schema)))))
       (get-sensor-list))
      (display "\"\";"))))

  (define prepared-statements-def
    (capture-output
     (thunk
      (display-nl "struct _PreparedStmts {")
      (for-each
       (lambda (sensor)
         (let* ((sensor-name (fget-reqd-nlist-elem-1 sensor 'name))
                (cpp-condition (fget-opt-nlist-elem-1 sensor 'cpp-condition))
                (stmt-list (fget-opt-nlist-elem-1up-e sensor 'sql-statements))
                (write-them
                 (thunk
                  (for-each
                   (lambda (stmt)
                     (display-nl (format "sqlite3_stmt* ~a~aStmt;"
                                         sensor-name
                                         (if (string? stmt) "" (first stmt)))))
                   stmt-list))))
           (when stmt-list
             (if cpp-condition
                 (begin
                   (display "#if ") (display-nl cpp-condition)
                   (write-them)
                   (display-nl "#endif"))
                 (write-them)))))
       (get-sensor-list))
      (display-nl "};")
      (display "typedef struct _PreparedStmts PreparedStmts;"))))

  (define sql-statement-preparation
    (capture-output
     (thunk
      (for-each-statement
       (lambda (sensor-name cpp-condition stmt-name stmt-sql)
         (alet write-them (thunk (display-nl (format "if (sqlite_prepare(self->db, ~s, -1, &(self->stmts.~a~aStmt), 0)) { goto fail; }" stmt-sql sensor-name stmt-name)))
               (with-cpp-condition-harness cpp-condition write-them))))
      (display-nl "return TRUE;")
      (display-nl "fail:")
      (display-nl "if (error) *error = gx_error_new(domain_cl2app, code_database_state_init, \"error preparing statements for database '%s': %s (%d)\", LOGDB_FILE, sqlite3_errmsg(self->db), sqlite3_errcode(self->db));")
      (display "return FALSE;"))))

  (define sql-statement-destruction
    (capture-output
     (thunk
      (for-each-statement
       (lambda (sensor-name cpp-condition stmt-name stmt-sql)
         (alet write-them (thunk (display-nl (format "if (self->stmts.~a~aStmt) { sqlite3_finalize(self->stmts.~a~aStmt); self->stmts.~a~aStmt = NULL; }" sensor-name stmt-name sensor-name stmt-name sensor-name stmt-name)))
               (with-cpp-condition-harness cpp-condition write-them))))
      (display "return;"))))

  (define (ast-with-cpp-condition-harness cpp-condition ast)
    (if cpp-condition
        ;; Some trickery here, relying on ordering being preserved.
        (sc 
         (cpp-if cpp-condition)
         ast
         (cpp-end)
         )
        ast))

  (define (bind-func-by-type type)
    (case-eq type
             ('int 'sqlite3_bind_int)
             ('int?-neqz 'sqlite3_bind_int_neqz)
             ('int?-ltez 'sqlite3_bind_int_ltez)
             ('int64 'sqlite3_bind_int64)
             ('text 'sqlite3_bind_text)
             ('text? 'sqlite3_bind_text_or_null)
             ('double 'sqlite3_bind_double)
             (else (error "bind-func-by-type unsupported type" type))))

  ;; When using 'static rather than 'transient, the string "has to
  ;; remain valid during all subsequent calls to sqlite3_step() on the
  ;; statement handle. Or until you bind a different value to the same
  ;; parameter."
  (define (maybe-dispose type dispose)
    (if dispose
        (string-append ", "
                       (case-eq dispose
                                ('static "SQLITE_STATIC")
                                ('transient "SQLITE_TRANSIENT")
                                (else dispose)))
        ""))

  (define log-db-insert-functions
    (sc-list
     (compact
      (map
       (lambda (sensor)
         (aand*
          api (fget-opt-nlist-elem sensor 'log-insert-api)
          (let* ((sensor-name (fget-reqd-nlist-elem-1 sensor 'name))
                 (cpp-condition (fget-opt-nlist-elem-1 sensor 'cpp-condition))
                 (func-name (format "log_db_log_~a" sensor-name))
                 (specific-args (fget-opt-nlist-elem-1up-e api 'args))
                 (stmt-id (fget-opt-nlist-elem-1 api 'statement))
                 (stmt-var (format "self->stmts.~a~aStmt" sensor-name (or stmt-id "")))
                 (binding-list (fget-opt-nlist-elem-1up-e api 'bindings))
                 (body-text
                  (capture-output
                   (thunk
                    (display-nl "gboolean rval = TRUE;")
                    (display-nl "time_t now = time(NULL); if (now == -1) { goto posix_fail; }")
                    (display-nl (format "if (sqlite3_bind_int(~a, 1, now)) { goto sql_fail; }" stmt-var))
                    (for-each
                     (lambda (binding)
                       (let* ((index (fget-reqd-nlist-elem-1 binding 'index))
                              (type (fget-reqd-nlist-elem-1 binding 'type))
                              (value (fget-reqd-nlist-elem-1 binding 'value))
                              (dispose (fget-opt-nlist-elem-1 binding 'dispose))
                              (func-name (bind-func-by-type type)))
                         (display-nl (format "if (~a(~a, ~a, ~a~a)) { goto sql_fail; }" func-name stmt-var index value (maybe-dispose type dispose)))))
                     binding-list)
                    (display-nl (format "if (sqlite3_step(~a) != SQLITE_DONE) goto sql_fail;" stmt-var))
                    (display-nl (format "if (sqlite3_reset(~a)) goto sql_fail;" stmt-var))
                    (display-nl "goto done;")
                    (display-nl "posix_fail: rval = FALSE; if (error) *error = gx_error_new(domain_cl2app, code_time_query, \"failed to access current time: %s (%d)\", strerror(errno), errno); goto done;")
                    (display-nl (format "sql_fail: rval = FALSE; if (error) *error = gx_error_new(domain_cl2app, code_database_command, \"failed to log ~a event: %s (%d)\", sqlite3_errmsg(self->db), sqlite3_errcode(self->db));" sensor-name))
                    (display "done: return rval;")
                    )))
                 (func-decl
                  (func (name func-name) cexport
                        (returns (type 'gboolean))
                        (apply args
                               (append
                                (list (arg (name 'self) (type (ptr-to 'LogDb))))
                                specific-args
                                (list (arg (name 'error) (type (ptr-to (ptr-to 'GError)))))))
                        (block
                         (cxx-line body-text)))))
            (ast-with-cpp-condition-harness cpp-condition func-decl))))
       (get-sensor-list)))))

  (define program-1
    (cunit
     (basename "sa_sensor_list_log_db")

     (includes
      (system-include "glib.h")
      (local-include "application_config.h")
      (local-include "ld_log_db.h")
      (local-include "sqlite_cl2.h")
      (local-include "common/utilities.h")
      )
     (body

      (cxx-internal-declarations
       "#include \"ld_private.h\""
       )
      
      (cxx-internal-declarations
       "#include \"ac_app_context.h\""
       "#include <errno.h>"
       "#include \"common/logging.h\""
       "#include \"common/assertions.h\""
       "#include \"common/threading.h\""
       "#include \"common/error_list.h\""
       )
      
      (cxx-exported-declarations "G_BEGIN_DECLS")

      ;;(cxx-exported-declarations "struct LogDb;")

      (cxx-internal-declarations
       create-tables-sql-def)

      (func (name "get_create_tables_sql") cexport
            ;;(verbatim-modifier "EXTERN_C")
            (returns (type (ptr-to (cconst 'char))))
            (block
             (return 'create_tables_sql)))
      
      (cxx-exported-declarations
       prepared-statements-def)

      (func (name "prepare_sql_statements") cexport
            ;;(verbatim-modifier "EXTERN_C")
            (returns (type 'gboolean))
            (args (arg (name 'self) (type (ptr-to 'LogDb)))
                  (arg (name 'error) (type (ptr-to (ptr-to 'GError)))))
            (block
             (cxx-line sql-statement-preparation)
             ))
      
      (func (name "destroy_sql_statements") cexport
            ;;(verbatim-modifier "EXTERN_C")
            (args (arg (name 'self) (type (ptr-to 'LogDb))))
            (block
             (cxx-line sql-statement-destruction)
             ))

      log-db-insert-functions
      
      (cxx-exported-declarations "G_END_DECLS")

      ))) ;; end program-1

  (define (symbol-upcase s)
    (string->symbol (string-upcase (symbol->string s))))

  (define (for-each/sep elem-f sep-f lst)
    (unless (null? lst)
      (elem-f (car lst))
      (for-each (lambda (x) (sep-f) (elem-f x)) (cdr lst))))
  
  (define (make-supported-sensor-definitions)
    (define (g)
      (map
       (lambda (sensor)
         (let* ((sensor-name (fget-reqd-nlist-elem-1 sensor 'name))
                (cpp-condition (fget-opt-nlist-elem-1 sensor 'cpp-condition)))
           (capture-output
            (thunk
             (display-nl (format "#if ~a" cpp-condition))
             (display-nl (format "#define SENSOR_~a_IS_SUPPORTED 1" (symbol-upcase sensor-name)))
             (display-nl (format "#define WHEN_SENSOR_~a_SUPPORTED_BLOCK(x) x" (symbol-upcase sensor-name)))
             (display-nl (format "#define WHEN_SENSOR_~a_SUPPORTED_NOTHING(x) x" (symbol-upcase sensor-name)))
             (display-nl "#else")
             (display-nl (format "#define SENSOR_~a_IS_SUPPORTED 0" (symbol-upcase sensor-name)))
             (display-nl (format "#define WHEN_SENSOR_~a_SUPPORTED_BLOCK(x) {}" (symbol-upcase sensor-name)))
             (display-nl (format "#define WHEN_SENSOR_~a_SUPPORTED_NOTHING(x) " (symbol-upcase sensor-name)))
             (display "#endif")))))
       active-sensors))
    
    (define (f)
      (display "#define RETURN_WHETHER_NAMED_SENSOR_IS_SUPPORTED(name) {")
      (for-each
       (lambda (sensor)
         (let* ((sensor-name (fget-reqd-nlist-elem-1 sensor 'name)))
           (display-nl " \\")
           (display "  if (strcmp(name,")
           (write (symbol->string sensor-name))
           (display ") == 0) { ")
           (display (format "return SENSOR_~a_IS_SUPPORTED;" (symbol-upcase sensor-name)))
           (display " }")))
       active-sensors)
      (display-nl " \\")
      (display "}"))

    (define (h)
      (display-nl (format "#define NUM_ALL_SENSORS ~a" (length active-sensors)))
      (display "#define NUM_SUPPORTED_SENSORS (0")
      (for-each
       (lambda (sensor)
         (let* ((sensor-name (fget-reqd-nlist-elem-1 sensor 'name)))
           (display " + ")
           (display (format "SENSOR_~a_IS_SUPPORTED" (symbol-upcase sensor-name)))))
       active-sensors)
      (display ")"))
    
    (define (hh)
      (display "#define ALL_SENSOR_NAMES_LITERAL_LIST {")
      (for-each/sep
       (lambda (sensor)
         (let* ((sensor-name (fget-reqd-nlist-elem-1 sensor 'name)))
           (write (symbol->string sensor-name))))
       (thunk (display ", "))
       active-sensors)
      (display ", NULL}"))
    
    (sc
     (apply cxx-exported-declarations (g))
     (cxx-exported-declarations (capture-output h))
     (cxx-exported-declarations (capture-output hh))
     (cxx-exported-declarations (capture-output f))
     ))

  (define (make-stop-named-sensor)
    (define (f)
      (display "#define STOP_NAMED_SENSOR(name) {")
      (for-each
       (lambda (sensor)
         (let* ((sensor-name (fget-reqd-nlist-elem-1 sensor 'name))
                (upcase-name (symbol-upcase sensor-name)))
           (display-nl " \\")
           (display (format "  WHEN_SENSOR_~a_SUPPORTED_NOTHING({" (symbol-upcase sensor-name)))
           (display "if (strcmp(name,")
           (write (symbol->string sensor-name))
           (display ") == 0) { ")
           (display (format "if (SENSOR_~a_IS_RUNNING) { SENSOR_~a_STOP; } return;" upcase-name upcase-name))
           (display " }") ;; end if
           (display "})") ;; end when supported
           ))
       active-sensors)
      (display-nl " \\")
      (display "}"))
    (cxx-exported-declarations (capture-output f)))

  (define (make-start-named-sensor)
    (define (f)
      (display "#define START_NAMED_SENSOR_OR_FAIL(name) {")
      (for-each
       (lambda (sensor)
         (let* ((sensor-name (fget-reqd-nlist-elem-1 sensor 'name))
                (upcase-name (symbol-upcase sensor-name)))
           (display-nl " \\")
           (display (format "  WHEN_SENSOR_~a_SUPPORTED_NOTHING({" (symbol-upcase sensor-name)))
           (display "if (strcmp(name,")
           (write (symbol->string sensor-name))
           (display ") == 0) { ")
           (display (format "if (SENSOR_~a_IS_RUNNING) return TRUE; SENSOR_~a_START; return success;" upcase-name upcase-name))
           (display " }") ;; end if
           (display "})") ;; end when supported
           ))
       active-sensors)
      (display-nl " \\")
      (display "}"))
    (cxx-exported-declarations (capture-output f)))

  (define (make-named-sensor-running)
    (define (f)
      (display "#define RETURN_WHETHER_NAMED_SENSOR_IS_RUNNING(name) {")
      (for-each
       (lambda (sensor)
         (let* ((sensor-name (fget-reqd-nlist-elem-1 sensor 'name))
                (upcase-name (symbol-upcase sensor-name)))
           (display-nl " \\")
           (display (format "  WHEN_SENSOR_~a_SUPPORTED_NOTHING({" (symbol-upcase sensor-name)))
           (display "if (strcmp(name,")
           (write (symbol->string sensor-name))
           (display ") == 0) { ")
           (display (format "return (SENSOR_~a_IS_RUNNING);" upcase-name))
           (display " }") ;; end if
           (display "})") ;; end when supported
           ))
       active-sensors)
      (display-nl " \\")
      (display "}"))
    (cxx-exported-declarations (capture-output f)))

  (define (make-reconfigure-matching-sensor)
    (define (f)
      (display "#define RECONFIGURE_MATCHING_SENSOR(key,value) {")
      (for-each
       (lambda (sensor)
         (let* ((sensor-name (fget-reqd-nlist-elem-1 sensor 'name))
                (upcase-name (symbol-upcase sensor-name)))
           (display-nl " \\")
           (display (format "  WHEN_SENSOR_~a_SUPPORTED_NOTHING({" (symbol-upcase sensor-name)))
           (display (format "if (strncmp(key, \"sensor.~a.\", ~a) == 0) { " sensor-name (+ (string-length (symbol->string sensor-name)) 8)))
           (display (format "(SENSOR_~a_RECONFIGURE(key, value)); return success;" upcase-name))
           (display " }") ;; end if
           (display "})") ;; end when supported
           ))
       active-sensors)
      (display-nl " \\")
      (display "}"))
    (cxx-exported-declarations (capture-output f)))
  
  (define (make-stop-all-sensors)
    (define (f)
      (display "#define STOP_ALL_SUPPORTED_SENSORS {")
      (for-each
       (lambda (sensor)
         (let* ((sensor-name (fget-reqd-nlist-elem-1 sensor 'name))
                (upcase-name (symbol-upcase sensor-name)))
           (display-nl " \\")
           (display (format "WHEN_SENSOR_~a_SUPPORTED_NOTHING({if (SENSOR_~a_IS_RUNNING) { SENSOR_~a_STOP }})"
                            (symbol-upcase sensor-name) upcase-name upcase-name))
           ))
       active-sensors)
      (display-nl " \\")
      (display "}"))
    (cxx-exported-declarations (capture-output f)))

  (define (make-start-all-sensors)
    (define (f)
      (display "#define TRY_START_ALL_SUPPORTED_SENSORS {")
      (for-each
       (lambda (sensor)
         (let* ((sensor-name (fget-reqd-nlist-elem-1 sensor 'name))
                (upcase-name (symbol-upcase sensor-name)))
           (display-nl " \\")
           (display (format "WHEN_SENSOR_~a_SUPPORTED_NOTHING({if (!(SENSOR_~a_IS_RUNNING) && SENSOR_AUTOSTART_IS_ALLOWED(~a)) { SENSOR_~a_START; handle_any_sensor_start_failure; }})" (symbol-upcase sensor-name) upcase-name sensor-name upcase-name))
           ))
       active-sensors)
      (display-nl " \\")
      (display "}"))
    (cxx-exported-declarations (capture-output f)))

  (define (make-create-all-sensors)
    (define (f)
      (display "#define CREATE_ALL_SENSORS_OR_FAIL {")
      (for-each
       (lambda (sensor)
         (let* ((sensor-name (fget-reqd-nlist-elem-1 sensor 'name))
                (upcase-name (symbol-upcase sensor-name)))
           (display-nl " \\")
           (display (format "WHEN_SENSOR_~a_SUPPORTED_NOTHING({SENSOR_~a_CREATE; if (!success) { goto fail; }})"
                            (symbol-upcase sensor-name) upcase-name))
           ))
       active-sensors)
      (display-nl " \\")
      (display "}"))
    (cxx-exported-declarations (capture-output f)))

  (define (make-destroy-all-sensors)
    (define (f)
      (display "#define DESTROY_ALL_SENSORS {")
      (for-each
       (lambda (sensor)
         (let* ((sensor-name (fget-reqd-nlist-elem-1 sensor 'name))
                (upcase-name (symbol-upcase sensor-name)))
           (display-nl " \\")
           (display (format "WHEN_SENSOR_~a_SUPPORTED_NOTHING(SENSOR_~a_DESTROY)"
                            (symbol-upcase sensor-name) upcase-name))
           ))
       active-sensors)
      (display-nl " \\")
      (display "}"))
    (cxx-exported-declarations (capture-output f)))

  ;; Here we generate definitions for ensuring that all of the listed
  ;; sensors are indeed included in the sensor array. There are no
  ;; public declarations, the .cpp file is directly included. This
  ;; means that there are no makefiles to adjust.
  (define program-2
    (cunit
     (basename "sa_sensor_list_integration")
     (includes
      (system-include "string.h"))
     (body

      (make-supported-sensor-definitions)
      (make-named-sensor-running)
      (make-stop-named-sensor)
      (make-start-named-sensor)
      (make-stop-all-sensors)
      (make-start-all-sensors)
      (make-create-all-sensors)
      (make-destroy-all-sensors)
      (make-reconfigure-matching-sensor)

      ))) ;; end program-2

  (let ((do-program
         (lambda (ast)
           ;;(pretty-nl ast)
           (when dump? (dump-h-and-cpp ast))
           (when gen? (generate-h-and-cpp ast))
           ;;(dump-analyzed-ast ast)
           ;;(dump-compiled-ast ast)
           )))
    (do-program program-2)
    (parameterize ((in-c-mode #t))
      (do-program program-1)))

  ;; To avoid the invocation of this function from having a value when
  ;; invoked in a REPL or with mzscheme --eval.
  (void)) ;; end generate function

#|

sa_sensor_list_make.ss

Copyright 2009 Helsinki Institute for Information Technology (HIIT)
and the authors. All rights reserved.

Authors: Tero Hasu <tero.hasu@hut.fi>

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation files
(the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software,
and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

|#
