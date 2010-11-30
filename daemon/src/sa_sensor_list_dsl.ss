#lang scheme

;; We have a DSL for defining sensor lists.
;; 
;; An important thing for the user of a DSL is to be notified of any
;; syntactic and semantic errors when they write something in the
;; language. This module implements syntax checking for the language.
;; 
;; As our language is sexp-based, here we must simply implement a
;; predicate that will "traverse" a sexp, and match it against a
;; grammar, looking for inconsistencies. The idea is the same as with
;; XML schema validation. (An alternative would be to provide an API
;; for building ASTs, and do argument checking in that API at AST
;; build time.)

(require (lib "usual-4.ss" "common"))
(require (lib "type-util.scm" "wg"))
(require (planet dvanhorn/grammar:1:3/grammar))

(provide/contract
 (sensor-list? (-> any/c boolean?)))

(define-grammar sensor-list?
  (grammar <sensors>

           (<sensors> (lst 'sensors (star <sensor>)))

           (<sensor>
            (report-if-bad 'sensor
                           (lst 'sensor (star <attr>))))

           (<attr>
            (report-if-bad 'attribute
                           (alt <inactive-attr>
                                <name-attr>
                                <essential-attr>
                                <cpp-attr>
                                <sql-schema>
                                <sql-stmts>
                                <log-insert-api>
                                )))

           (<inactive-attr> (lst 'inactive <boolean>))
           
           (<essential-attr> (lst 'essential <boolean>))

           ;; The name of the sensor. A lot of names are derived based
           ;; on this name. A unique identifier for this sensor.
           ;; Should be alphanumeric, all lowercase, and starting with
           ;; a letter.
           (<name-attr> (lst 'name <name>))

           ;; Indicates for the build condition for the sensor is
           ;; available. This is a CPP expression. There may be
           ;; different implementations in different builds, or none
           ;; at all. Without this attribute, the sensor is available
           ;; in all builds.
           (<cpp-attr> (lst 'cpp-condition <cpp-expr>))

           ;; The SQL statements required for creating tables for the
           ;; sensor data. You need not specify any if you are going
           ;; to do this manually.
           (<sql-schema> (lst 'sql-schema (star <sql-stmt>)))

           ;; The SQL statements that should be prepared for the
           ;; sensor in LogDb. You need not specify any if you do not
           ;; require prepared statements, or if you are going to deal
           ;; with such statements manually.
           (<sql-stmts> (lst 'sql-statements (star <sql-stmt>)))

           ;; Can either be an unnamed statement, or a named
           ;; statement. An example of the latter: '(Insert "insert
           ;; ...;"). You will want to use named statements when a
           ;; single sensor requires multiple statements.
           (<sql-stmt> (alt (predicate string?)
                            (lst (predicate symbol?) (predicate string?))))

           ;; Defines how to generate code for a log database
           ;; insertion API.
           (<log-insert-api>
            (lst 'log-insert-api
                 (opt (lst 'statement <name>))
                 (lst 'args (star <func-arg>))
                 (lst 'bindings (star <binding>))))
           (<binding>
            (lst
             'binding
             (lst 'index <integer>)
             (lst 'type <name>)
             (lst 'value <c-expr>)
             (opt (lst 'dispose
                       (alt 'static
                            'transient
                            <c-expr>)))))
           (<func-arg>
            (predicate (lambda (ast) (list-named? ast 'arg))))

           (<name> (predicate symbol?))
           (<boolean> (predicate boolean?))
           (<integer> (predicate integer?))
           (<cpp-expr> (predicate string?))
           (<c-expr> (predicate string?))
           
           )) ;; end define-grammar

#;
(write-nl
 (sensor-list?
  '(sensors
    (sensor (name foobar))
    (sensor (cpp-condition "defined(__EPOC32__)"))
    (sensor))))

#|

sa_sensor_list_dsl.ss

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
