#!/bin/sh
#| # -*- scheme -*-
exec mzscheme --name "$0" --eval "(require scheme (lib \"usual-4.ss\" \"common\") (file \"$0\")) (current-load-relative-directory (path-dirname (path->complete-path \"$0\"))) (sensor-list-main)"
|#

;; Note that this file can both be required as a module and executed as a program.

#lang scheme

(require (lib "usual-4.ss" "common"))
(require (lib "ast-util.scm" "wg"))
(require (lib "cxx-syntax.ss" "wg"))
(require "sa_sensor_list_make.ss")

(define*
  SENSORS-SPEC
  `(sensors

    ;; Not really a sensor.
    (sensor (name transaction) (inactive #t) (essential #t)
            (sql-statements (Begin "begin transaction;")
                            (Commit "commit transaction;")
                            (Rollback "rollback transaction;")))
    ;; We might also want "select last_insert_rowid();", but for that
    ;; there is also the C API function sqlite3_int64
    ;; sqlite3_last_insert_rowid(sqlite3*) that can be used instead,
    ;; and more easily.
    
    ;; status
    (sensor (name status) (inactive #t) (essential #t)
            (sql-schema "create table status_log (unixtime INTEGER, message TEXT);")
            (sql-statements "insert into status_log (unixtime, message) values (?, ?);"))

    ;; mark
    (sensor (name mark)
            (cpp-condition "__MARK_ENABLED__")
            (sql-schema "create table mark_log (unixtime INTEGER, message TEXT);")
            (sql-statements "insert into mark_log (unixtime, message) values (?, ?);")
            (log-insert-api
             (args ,(arg (type (ptr-to (cconst 'char))) (name 'msgText)))
             (bindings
              (binding (index 2) (type text) (value "msgText, strlen(msgText)") (dispose static))))
            )
    
    ;; appmessage
    ;; 
    ;; For freeform messages received from other applications. This is
    ;; not an active sensor in the sense that any activity is
    ;; initiated by another application, via a Lua binding.
    (sensor (name appmessage) (inactive #t)
            (cpp-condition "__APPMESSAGE_ENABLED__")
            (sql-schema "create table appmessage_log (unixtime INTEGER, message TEXT);")
            (sql-statements "insert into appmessage_log (unixtime, message) values (?, ?);")
            (log-insert-api
             (args ,(arg (type (ptr-to (cconst 'char))) (name 'msgText)))
             (bindings
              (binding (index 2) (type text) (value "msgText, strlen(msgText)") (dispose static))))
            )

    ;; timer
    (sensor (name timer)
            (cpp-condition "__TIMER_ENABLED__")
            (sql-schema "create table timer_scan (unixtime INTEGER);")
            (sql-statements "insert into timer_scan (unixtime) values (?);")
            (log-insert-api (args) (bindings)))
    
    ;; flightmode
    ;; 
    ;; Symbian-specific app context code observes (and logs) this.
    (sensor (name flightmode) (inactive #t)
            (cpp-condition "defined(__EPOC32__)")
            (sql-schema "create table flightmode_scan (unixtime INTEGER, value INTEGER);")
            (sql-statements "insert into flightmode_scan (unixtime, value) values (?, ?);")
            (log-insert-api
             (args ,(arg (type 'gboolean) (name 'value)))
             (bindings
              (binding (index 2) (type int) (value "(value ? 1 : 0)")))))

    ;; battery
    (sensor (name battery) (inactive #t)
            (cpp-condition "defined(__EPOC32__)")
            (sql-schema "create table battery_scan (unixtime INTEGER, status INTEGER, level INTEGER);")
            (sql-statements "insert into battery_scan (unixtime, status, level) values (?, ?, ?);")
            (log-insert-api
             (args
              ,(arg (type 'int) (name 'status))
              ,(arg (type 'int) (name 'level))
              )
             (bindings
              (binding (index 2) (type int) (value "status"))
              (binding (index 3) (type int) (value "level"))
              )))
    
    ;; registration
    (sensor (name registration) (inactive #t)
            (cpp-condition "defined(__EPOC32__)")
            (sql-schema "create table registration_scan (unixtime INTEGER, status INTEGER);")
            (sql-statements "insert into registration_scan (unixtime, status) values (?, ?);")
            (log-insert-api
             (args
              ,(arg (type 'int) (name 'status))
              )
             (bindings
              (binding (index 2) (type int) (value "status"))
              )))
    
    ;; signal
    (sensor (name signal) (inactive #t)
            (cpp-condition "defined(__EPOC32__)")
            (sql-schema "create table signal_scan (unixtime INTEGER, dbm INTEGER, bars INTEGER);")
            (sql-statements "insert into signal_scan (unixtime, dbm, bars) values (?, ?, ?);")
            (log-insert-api
             (args
              ,(arg (type 'int) (name 'dbm))
              ,(arg (type 'int) (name 'bars))
              )
             (bindings
              (binding (index 2) (type int) (value "dbm"))
              (binding (index 3) (type int) (value "bars"))
              )))
    
    ;; operator
    (sensor (name operator) (inactive #t)
            (cpp-condition "defined(__EPOC32__)")
            (sql-schema "create table operator_scan (unixtime INTEGER, name TEXT);")
            (sql-statements "insert into operator_scan (unixtime, name) values (?, ?);")
            (log-insert-api
             (args ,(arg (type (ptr-to (cconst 'char))) (name 'operatorName)))
             (bindings
              (binding (index 2) (type text) (value "operatorName, strlen(operatorName)") (dispose static)))))

    ;; ambient light (based on Qt Mobility)
    (sensor (name light)
            (cpp-condition "__LIGHT_ENABLED__")
            ;; enum LightLevel { Undefined, Dark, Twilight, Light, Bright, Sunny }
            (sql-schema "create table light_scan (unixtime INTEGER, level INTEGER);")
            (sql-statements "insert into light_scan (unixtime, level) values (?, ?);")
            (log-insert-api
             (args ,(arg (type 'int) (name 'level)))
             (bindings
              (binding (index 2) (type int) (value "level")))))
    
    ;; tap sensor (based on Qt Mobility)
    (sensor (name tap) (inactive #t)
            (cpp-condition "__WITH_TAP_SENSORS__")
            (sql-schema "create table tap_scan (unixtime INTEGER, direction INTEGER, is_dbl INTEGER);")
            (sql-statements "insert into tap_scan (unixtime, direction, is_dbl) values (?, ?, ?);")
            (log-insert-api
             (args ,(arg (type 'int) (name 'direction))
                   ,(arg (type 'gboolean) (name 'isDouble)))
             (bindings
              (binding (index 2) (type int) (value "direction"))
              (binding (index 3) (type int) (value "isDouble"))
              )))

    (sensor (name singletap)
            (cpp-condition "__SINGLETAP_ENABLED__"))
    
    (sensor (name doubletap)
            (cpp-condition "__DOUBLETAP_ENABLED__"))
    
    ;; proximity sensor (based on Qt Mobility)
    (sensor (name proximity)
            (cpp-condition "__PROXIMITY_ENABLED__")
            (sql-schema "create table proximity_scan (unixtime INTEGER, close INTEGER);")
            (sql-statements "insert into proximity_scan (unixtime, close) values (?, ?);")
            (log-insert-api
             (args ,(arg (type 'gboolean) (name 'isClose)))
             (bindings
              (binding (index 2) (type int) (value "isClose")))))

    ;; An alternative data representation for "proximity", with multiple events per log entry, as JSON.
    (sensor (name proximityset) (inactive #t)
            (cpp-condition "__PROXIMITY_ENABLED__")
            (sql-schema "create table proximityset_scan (unixtime INTEGER, json TEXT);")
            (sql-statements "insert into proximityset_scan (unixtime, json) values (?, ?);")
            (log-insert-api
             (args ,(arg (type (ptr-to (cconst 'char))) (name 'json)))
             (bindings
              (binding (index 2) (type text) (value "json, strlen(json)") (dispose static)))))
    
    (sensor (name music)
            (cpp-condition "__MUSIC_ENABLED__"))

    (sensor (name musicplayer) (inactive #t)
            (cpp-condition "__MUSIC_ENABLED__")
            (sql-schema "create table musicplayer_scan (unixtime INTEGER, event INTEGER, detail TEXT);")
            (sql-statements "insert into musicplayer_scan (unixtime, event, detail) values (?, ?, ?);")
            (log-insert-api
             (args
              ,(arg (type 'int) (name 'eventId))
              ,(arg (type (ptr-to (cconst 'char))) (name 'detail))
              )
             (bindings
              (binding (index 2) (type int) (value "eventId"))
              (binding (index 3) (type text?) (value "detail, strlen(detail)") (dispose static))
              )))
            
    (sensor (name musictrack) (inactive #t)
            (cpp-condition "__MUSIC_ENABLED__")
            (sql-schema "create table musictrack_scan (unixtime INTEGER, url TEXT, title TEXT, artist TEXT, album TEXT);")
            (sql-statements "insert into musictrack_scan (unixtime, url, title, artist, album) values (?, ?, ?, ?, ?);")
            (log-insert-api
             (args
              ,(arg (type (ptr-to (cconst 'char))) (name 'url))
              ,(arg (type (ptr-to (cconst 'char))) (name 'title))
              ,(arg (type (ptr-to (cconst 'char))) (name 'artist))
              ,(arg (type (ptr-to (cconst 'char))) (name 'album))
              )
             (bindings
              (binding (index 2) (type text?) (value "url, strlen(url)") (dispose static))
              (binding (index 3) (type text?) (value "title, strlen(title)") (dispose static))
              (binding (index 4) (type text?) (value "artist, strlen(artist)") (dispose static))
              (binding (index 5) (type text?) (value "album, strlen(album)") (dispose static))
              )))
            
    ;; httpurl
    (sensor (name httpurl)
            (cpp-condition "__HTTPURL_ENABLED__")
            (sql-schema "create table httpurl_scan (unixtime INTEGER, url TEXT);")
            (sql-statements "insert into httpurl_scan (unixtime, url) values (?, ?);")
            (log-insert-api
             (args
              ,(arg (type (ptr-to (cconst 'char))) (name 'url))
              )
             (bindings
              (binding (index 2) (type text) (value "url, strlen(url)") (dispose static))
              )))
    
    ;; weburl
    (sensor (name weburl)
            (cpp-condition "__WEBURL_ENABLED__")
            (sql-schema "create table weburl_scan (unixtime INTEGER, name TEXT, url TEXT);")
            (sql-statements "insert into weburl_scan (unixtime, name, url) values (?, ?, ?);")
            (log-insert-api
             (args
              ,(arg (type (ptr-to (cconst 'char))) (name 'name))
              ,(arg (type (ptr-to (cconst 'char))) (name 'url))
              )
             (bindings
              (binding (index 2) (type text) (value "name, strlen(name)") (dispose static))
              (binding (index 3) (type text) (value "url, strlen(url)") (dispose static))
              )))
    
    ;; profile (needs a variant targeting new extended plugin)
    (sensor (name profile)
            (cpp-condition "__PROFILE_ENABLED__")
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
            (cpp-condition "__CELLID_ENABLED__")
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
            (cpp-condition "__BTPROX_ENABLED__")
            (sql-schema "create table btprox_scan (unixtime INTEGER, scan_id INTEGER not null primary key AUTOINCREMENT);"
                        "create table btprox_item (scan_id INTEGER not null, address TEXT not null, name TEXT not null);")
            (sql-statements (Scan "insert into btprox_scan (unixtime) values (?);")
                            (Item "insert into btprox_item (scan_id, address, name) values (?, ?, ?);"))
            ;;(scanner-object
            ;;(epoc-create-expr "CSensor_btprox::NewL(iLogDb)"))
            )
    
    ;; position
    (sensor (name position) (inactive #t)
            (cpp-condition "__POSITION_ENABLED__")
            (sql-schema "create table position_scan (unixtime INTEGER, latitude REAL, longitude REAL, altitude REAL, vertical_accuracy REAL, horizontal_accuracy REAL, course TEXT, satellites TEXT);")
            (sql-statements "insert into position_scan (unixtime, latitude, longitude, altitude, vertical_accuracy, horizontal_accuracy, course, satellites) values (?, ?, ?, ?, ?, ?, ?, ?);")
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
            )
    
    ;; gps
    (sensor (name gps)
            (cpp-condition "__GPS_ENABLED__"))
    
    ;; cellpos
    (sensor (name cellpos)
            (cpp-condition "__CELLPOS_ENABLED__"))
    
    ;; appfocus
    (sensor (name appfocus)
            (cpp-condition "__APPFOCUS_ENABLED__")
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
            (cpp-condition "__KEYPRESS_ENABLED__")
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
            (cpp-condition "__INACTIVITY_ENABLED__")
            (sql-schema "create table inactivity_scan (unixtime INTEGER, value INTEGER);")
            (sql-statements "insert into inactivity_scan (unixtime, value) values (?, ?);")
            (log-insert-api
             (args ,(arg (type 'gboolean) (name 'value)))
             (bindings
              (binding (index 2) (type int) (value "(value ? 1 : 0)"))))
            )

    ;; Symbian Standby screen indicators.
    (sensor (name indicator)
            (cpp-condition "__INDICATOR_ENABLED__")
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
            (cpp-condition "__CALLSTATUS_ENABLED__")
            (sql-schema "create table callstatus_scan (unixtime INTEGER not null, value INTEGER not null, number TEXT, contact_name TEXT, starttime INTEGER, osterm INTEGER, netterm INTEGER);")
            (sql-statements "insert into callstatus_scan (unixtime, value, number, contact_name, starttime, osterm, netterm) values (?, ?, ?, ?, ?, ?, ?);")
            (log-insert-api
             (args
              ,(arg (type 'int) (name 'value))
              ,(arg (type (ptr-to (cconst 'char))) (name 'aNumber))
              ,(arg (type (ptr-to (cconst 'char))) (name 'aContactName))
              ,(arg (type 'int) (name 'aStartTime))
              ,(arg (type 'int) (name 'aOsTerm))
              ,(arg (type 'int) (name 'aNetTerm)))
             (bindings 
              (binding (index 2) (type int) (value "value"))
              (binding (index 3) (type text?) (value "aNumber, strlen(aNumber)") (dispose static))
              (binding (index 4) (type text?) (value "aContactName, strlen(aContactName)") (dispose static))
              (binding (index 5) (type int?-neqz) (value "aStartTime"))
              (binding (index 6) (type int?-ltez) (value "aOsTerm"))
              (binding (index 7) (type int?-ltez) (value "aNetTerm")))))
    
    (sensor (name smsevent)
            (cpp-condition "__SMSEVENT_ENABLED__")
            (sql-schema "create table smsevent_scan (unixtime INTEGER not null, evtype TEXT not null, number TEXT, contact_name TEXT, body TEXT);")
            (sql-statements "insert into smsevent_scan (unixtime, evtype, number, contact_name, body) values (?, ?, ?, ?, ?);")
            (log-insert-api
             (args
              ,(arg (type (ptr-to (cconst 'char))) (name 'aEvType))
              ,(arg (type (ptr-to (cconst 'char))) (name 'aNumber))
              ,(arg (type (ptr-to (cconst 'char))) (name 'aContactName))
              ,(arg (type (ptr-to (cconst 'char))) (name 'aBodyText))
              )
             (bindings 
              (binding (index 2) (type text) (value "aEvType, strlen(aEvType)") (dispose static))
              (binding (index 3) (type text) (value "aNumber, strlen(aNumber)") (dispose static))
              (binding (index 4) (type text?) (value "aContactName, strlen(aContactName)") (dispose static))
              (binding (index 5) (type text?) (value "aBodyText, strlen(aBodyText)") (dispose static))
              )
            ))
    
    ))

(define* FULL-SENSOR-LIST (cdr SENSORS-SPEC))

(define* (get-sensor-name sensor)
  (fget-reqd-nlist-elem-1 sensor 'name))

(define* (sensor-essential? sensor)
  (true? (fget-opt-nlist-elem-1 sensor 'essential)))

(define* (sensor-inactive? sensor)
  (true? (fget-opt-nlist-elem-1 sensor 'inactive)))

(define* all-sensor-names
  (map get-sensor-name FULL-SENSOR-LIST))

(define* essential-sensor-names
  (map get-sensor-name (filter sensor-essential? FULL-SENSOR-LIST)))

(define* (sensor-enabled-symbol name)
  (string->symbol (format "~a-enabled" name)))

(define* (sensor-enabled-method-name name)
  (string->symbol (format "~a-enabled.attr" name)))

(define* (sensor-list-main)
  (let ((dump? #f)
        (gen? #t))
    (generate SENSORS-SPEC dump? gen?)))

#|

sa_sensor_list_spec.ss

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
