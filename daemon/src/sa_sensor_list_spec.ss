#!/bin/sh
#| # -*- scheme -*-
exec mzscheme --name "$0" --eval "(require scheme (lib \"usual-4.ss\" \"common\") (file \"$0\")) (current-load-relative-directory (path-dirname (path->complete-path \"$0\"))) (sensor-list-main)"
|#

;; Note that this file can both be required as a module and executed as a program.

#lang scheme

(require (lib "usual-4.ss" "common"))
(require (lib "cxx-syntax.ss" "wg"))
;;(require (lib "node-ctors.scm" "wg"))
(require "sa_sensor_list_make.ss")

(define*
  all-sensors
  `(sensors

    ;; Not really a sensor.
    (sensor (name transaction) (inactive #t)
            (platforms "__TRANSACTION_ENABLED__")
            (sql-statements (Begin "begin transaction;")
                            (Commit "commit transaction;")
                            (Rollback "rollback transaction;")))
    ;; We might also want "select last_insert_rowid();", but for that
    ;; there is also the C API function sqlite3_int64
    ;; sqlite3_last_insert_rowid(sqlite3*) that can be used instead,
    ;; and more easily.
    
    ;; status
    (sensor (name status) (inactive #t)
            (platforms "__STATUS_ENABLED__")
            (sql-schema "create table status_log (unixtime INTEGER, message TEXT);")
            (sql-statements "insert into status_log (unixtime, message) values (?, ?);"))

    ;; mark
    (sensor (name mark)
            (platforms "__MARK_ENABLED__")
            (sql-schema "create table mark_log (unixtime INTEGER);")
            (sql-statements "insert into mark_log (unixtime) values (?);")
            (log-insert-api (args) (bindings)))
    
    ;; appmessage
    ;; 
    ;; For freeform messages received from other applications. This is
    ;; not an active sensor in the sense that any activity is
    ;; initiated by another application, via a Lua binding.
    (sensor (name appmessage) (inactive #t)
            (platforms "__APPMESSAGE_ENABLED__")
            (sql-schema "create table appmessage_log (unixtime INTEGER, message TEXT);")
            (sql-statements "insert into appmessage_log (unixtime, message) values (?, ?);")
            (log-insert-api
             (args ,(arg (type (ptr-to (cconst 'char))) (name 'msgText)))
             (bindings
              (binding (index 2) (type text) (value "msgText, strlen(msgText)") (dispose static))))
            )

    ;; timer
    (sensor (name timer)
            (platforms "__TIMER_ENABLED__")
            (sql-schema "create table timer_scan (unixtime INTEGER);")
            (sql-statements "insert into timer_scan (unixtime) values (?);")
            (log-insert-api (args) (bindings)))
    
    ;; flightmode
    ;; 
    ;; We have code for a dedicated "flightmode" sensor, but that is
    ;; rather redundant as the "callstatus" sensor can easily log
    ;; flightmode changes as well.
    (sensor (name flightmode) (inactive #t)
            ;;(platforms "__FLIGHTMODE_ENABLED__")
            (platforms "__CALLSTATUS_ENABLED__")
            (sql-schema "create table flightmode_scan (unixtime INTEGER, value INTEGER);")
            (sql-statements "insert into flightmode_scan (unixtime, value) values (?, ?);")
            (log-insert-api
             (args ,(arg (type 'gboolean) (name 'value)))
             (bindings
              (binding (index 2) (type int) (value "(value ? 1 : 0)")))))

    ;; profile (needs a variant targeting new extended plugin)
    (sensor (name profile)
            (platforms "__PROFILE_ENABLED__")
            (sql-schema "create table profile_scan (unixtime INTEGER, value INTEGER, name TEXT);")
            (sql-statements "insert into profile_scan (unixtime, value, name) values (?, ?, ?);")
            (log-insert-api
             (args ,(arg (type 'int) (name 'profileId))
                   ,(arg (type (ptr-to (cconst 'char))) (name 'profileName)))
             (bindings
              (binding (index 2) (type int) (value "profileId"))
              (binding (index 3) (type text) (value "profileName, strlen(profileName)") (dispose static))))
            ;;(scanner-object
            ;;(epoc-create-expr "CSensor_profile::NewL(iLogDb)"))
            )
    
    ;; cellid
    (sensor (name cellid)
            (platforms "__CELLID_ENABLED__")
            (sql-schema "create table cellid_scan (unixtime INTEGER, country_code TEXT, network_code TEXT, area_code INTEGER, cell_id INTEGER);")
            (sql-statements "insert into cellid_scan (unixtime, country_code, network_code, area_code, cell_id) values (?, ?, ?, ?, ?);")
            (log-insert-api
             (args
              ,(arg (type (ptr-to (cconst 'char))) (name 'countryCode))
              ,(arg (type (ptr-to (cconst 'char))) (name 'networkCode))
              ,(arg (type 'int) (name 'areaCode))
              ,(arg (type 'int) (name 'cellId))
              )
             (bindings
              (binding (index 2) (type text) (value "countryCode, strlen(countryCode)") (dispose static))
              (binding (index 3) (type text) (value "networkCode, strlen(networkCode)") (dispose static))
              (binding (index 4) (type int) (value "areaCode"))
              (binding (index 5) (type int) (value "cellId"))
              ))
            ;;(scanner-object
            ;;(epoc-create-expr "new (ELeave) CSensor_cellid(*iTelephony, iLogDb)"))
            )

    ;; btprox
    (sensor (name btprox)
            (platforms "__BTPROX_ENABLED__")
            (sql-schema "create table btprox_scan (unixtime INTEGER, scan_id INTEGER not null primary key AUTOINCREMENT);"
                        "create table btprox_item (scan_id INTEGER not null, address TEXT not null, name TEXT not null);")
            (sql-statements (Scan "insert into btprox_scan (unixtime) values (?);")
                            (Item "insert into btprox_item (scan_id, address, name) values (?, ?, ?);"))
            ;;(scanner-object
            ;;(epoc-create-expr "CSensor_btprox::NewL(iLogDb)"))
            )
    
    ;; gps
    (sensor (name gps)
            (platforms "__GPS_ENABLED__")
            (sql-schema "create table gps_scan (unixtime INTEGER, latitude REAL, longitude REAL, altitude REAL, vertical_accuracy REAL, horizontal_accuracy REAL, course TEXT, satellites TEXT);")
            (sql-statements "insert into gps_scan (unixtime, latitude, longitude, altitude, vertical_accuracy, horizontal_accuracy, course, satellites) values (?, ?, ?, ?, ?, ?, ?, ?);")
            (log-insert-api
             (args
              ,(arg (type 'double) (name 'latitude))
              ,(arg (type 'double) (name 'longitude))
              ,(arg (type 'double) (name 'altitude))
              ,(arg (type 'double) (name 'verticalAccuracy))
              ,(arg (type 'double) (name 'horizontalAccuracy))
              ,(arg (type (ptr-to (cconst 'char))) (name 'course))
              ,(arg (type (ptr-to (cconst 'char))) (name 'satellites))
              )
             (bindings
              (binding (index 2) (type double) (value "latitude"))
              (binding (index 3) (type double) (value "longitude"))
              (binding (index 4) (type double) (value "altitude"))
              (binding (index 5) (type double) (value "verticalAccuracy"))
              (binding (index 6) (type double) (value "horizontalAccuracy"))
              (binding (index 7) (type text) (value "course, strlen(course)") (dispose static))
              (binding (index 8) (type text) (value "satellites, strlen(satellites)") (dispose static))
              ))
            ;;(scanner-object
            ;;(epoc-create-expr "CSensor_gps::NewL(iLogDb)"))
            )
    
    ;; appfocus
    (sensor (name appfocus)
            (platforms "__APPFOCUS_ENABLED__")
            (sql-schema "create table appfocus_scan (unixtime INTEGER, uid INTEGER, caption TEXT);")
            (sql-statements "insert into appfocus_scan (unixtime, uid, caption) values (?, ?, ?);")
            (log-insert-api
             (args
              ,(arg (type 'sqlite3_int64) (name 'appUid)) ;; non-negative 32-value
              ,(arg (type (ptr-to (cconst 'char))) (name 'appCaption))
              )
             (bindings
              (binding (index 2) (type int64) (value "appUid"))
              (binding (index 3) (type text) (value "appCaption, strlen(appCaption)") (dispose static))
              ))
            ;;(scanner-object
            ;;(epoc-create-expr "CSensor_appfocus::NewL(iLogDb)"))
            )

    ;; keypress (needs a variant implemented in terms of Anim DLL)
    (sensor (name keypress)
            (platforms "__KEYPRESS_ENABLED__")
            (sql-schema "create table keypress_scan (unixtime INTEGER, presstimes TEXT);")
            (sql-statements "insert into keypress_scan (unixtime, presstimes) values (?, ?);")
            (log-insert-api
             (args
              ,(arg (type (ptr-to (cconst 'char))) (name 'aPressTimes))
              )
             (bindings
              (binding (index 2) (type text) (value "aPressTimes, strlen(aPressTimes)") (dispose static))
              ))
            ;;(scanner-object
            ;;(epoc-create-expr "CSensor_keypress::NewL(iLogDb)"))
            )
    
    ;; inactivity
    ;;
    ;; The value "1" indicates activity.
    (sensor (name inactivity)
            (platforms "__INACTIVITY_ENABLED__")
            (sql-schema "create table inactivity_scan (unixtime INTEGER, value INTEGER);")
            (sql-statements "insert into inactivity_scan (unixtime, value) values (?, ?);")
            (log-insert-api
             (args ,(arg (type 'gboolean) (name 'value)))
             (bindings
              (binding (index 2) (type int) (value "(value ? 1 : 0)"))))
            )

    ;; Symbian Standby screen indicators.
    (sensor (name indicator)
            (platforms "__INDICATOR_ENABLED__")
            (sql-schema "create table indicator_scan (unixtime INTEGER, value INTEGER, caps INTEGER);")
            (sql-statements "insert into indicator_scan (unixtime, value, caps) values (?, ?, ?);")
            (log-insert-api
             (args ,(arg (type 'guint32) (name 'value))
                   ,(arg (type 'guint32) (name 'caps)))
             (bindings
              ;; Casting to an int is hopefully okay. Should be at
              ;; least if the high bit is not used.
              (binding (index 2) (type int) (value "(int)value"))
              (binding (index 3) (type int) (value "(int)caps"))
            )))
    
    (sensor (name callstatus)
            (platforms "__CALLSTATUS_ENABLED__")
            (sql-schema "create table callstatus_scan (unixtime INTEGER, value INTEGER, number TEXT);")
            (sql-statements "insert into callstatus_scan (unixtime, value, number) values (?, ?, ?);")
            (log-insert-api
             (args
              ,(arg (type 'int) (name 'value))
              ,(arg (type (ptr-to (cconst 'char))) (name 'aNumber))
              )
             (bindings 
              (binding (index 2) (type int) (value "value"))
              (binding (index 3) (type text?) (value "aNumber, strlen(aNumber)") (dispose static))
              )
             ))
    
    (sensor (name smsevent)
            (platforms "__SMSEVENT_ENABLED__")
            (sql-schema "create table smsevent_scan (unixtime INTEGER, evtype TEXT, number TEXT);")
            (sql-statements "insert into smsevent_scan (unixtime, evtype, number) values (?, ?, ?);")
            (log-insert-api
             (args
              ,(arg (type (ptr-to (cconst 'char))) (name 'aEvType))
              ,(arg (type (ptr-to (cconst 'char))) (name 'aNumber))
              )
             (bindings 
              (binding (index 2) (type text) (value "aEvType, strlen(aEvType)") (dispose static))
              (binding (index 3) (type text) (value "aNumber, strlen(aNumber)") (dispose static))
              )
            ))
    
    ))

(define* (sensor-list-main)
  (let ((dump? #f)
        (gen? #t))
    (generate all-sensors dump? gen?)))

