echo AMBIENT LIGHT
sqlite3 $1 "select datetime(unixtime, 'unixepoch'), level from light_scan;" 

echo APP MESSAGES
sqlite3 $1 "select datetime(unixtime, 'unixepoch'), message from appmessage_log;"

echo APPLICATION FOCUS
sqlite3 $1 "select datetime(unixtime, 'unixepoch'), uid, caption from appfocus_scan;"

echo BATTERY STATUS
sqlite3 $1 "select datetime(unixtime, 'unixepoch'), status, level from battery_scan;"

echo BLUETOOTH PROXIMITY DEVICES
sqlite3 $1 "select datetime(unixtime, 'unixepoch'), address, name from btprox_item, btprox_scan where btprox_item.scan_id = btprox_scan.scan_id order by unixtime, address;"
echo BLUETOOTH PROXIMITY SCAN TIMES
sqlite3 $1 "select datetime(unixtime, 'unixepoch'), scan_id from btprox_scan order by unixtime;"

echo CALL STATUS
sqlite3 $1 "select datetime(unixtime, 'unixepoch'), value, number, contact_name, datetime(starttime, 'unixepoch'), osterm, netterm from callstatus_scan;"

echo FLIGHT MODE
sqlite3 $1 "select datetime(unixtime, 'unixepoch'), value from flightmode_scan;"

echo GSM CELL ID
sqlite3 $1 "select datetime(unixtime, 'unixepoch'), country_code, network_code, area_code, cell_id from cellid_scan;"

echo HTTP URL
sqlite3 $1 "select datetime(unixtime, 'unixepoch'), url from httpurl_scan;" 

echo INACTIVITY '(active=1)'
sqlite3 $1 "select datetime(unixtime, 'unixepoch'), value from inactivity_scan;"

echo KEYPRESS
sqlite3 $1 "select datetime(unixtime, 'unixepoch'), presstimes from keypress_scan;" 

echo MUSIC PLAYER STATE
sqlite3 $1 "select datetime(unixtime, 'unixepoch'), event, detail from musicplayer_scan;" 

echo MUSIC TRACK INFO
sqlite3 $1 "select datetime(unixtime, 'unixepoch'), url, title, artist, album from musictrack_scan;" 

echo NETWORK REGISTRATION STATUS
sqlite3 $1 "select datetime(unixtime, 'unixepoch'), status from registration_scan;"

echo NETWORK SIGNAL STRENGTH
sqlite3 $1 "select datetime(unixtime, 'unixepoch'), dbm, bars from signal_scan;"

echo OPERATOR
sqlite3 $1 "select datetime(unixtime, 'unixepoch'), name from operator_scan;"

echo POSITION
sqlite3 $1 "select datetime(unixtime, 'unixepoch'), latitude, longitude, altitude, vertical_accuracy, horizontal_accuracy, course, satellites from position_scan;"

echo PROFILE CHANGE
sqlite3 $1 "select datetime(unixtime, 'unixepoch'), value, name from profile_scan;"

echo PROXIMITY
sqlite3 $1 "select datetime(unixtime, 'unixepoch'), close from proximity_scan;"

echo SMS EVENT
sqlite3 $1 "select datetime(unixtime, 'unixepoch'), evtype, number, contact_name from smsevent_scan;"

echo STATUS INDICATORS
sqlite3 $1 "select datetime(unixtime, 'unixepoch'), value, caps from indicator_scan;"

echo TAPPING
sqlite3 $1 "select datetime(unixtime, 'unixepoch'), direction, is_dbl from tap_scan;"

echo WEB APP URL
sqlite3 $1 "select datetime(unixtime, 'unixepoch'), name, url from weburl_scan;" 

echo MARKS
sqlite3 $1 "select datetime(unixtime, 'unixepoch'), message from mark_log;"

echo STATUS MESSAGES
sqlite3 $1 "select datetime(unixtime, 'unixepoch'), message from status_log;"

echo Notes:
echo Text is encoded as UTF-8.
echo Application UIDs are hexadecimal.
echo Some non-primary, variable data is in JSON format.
