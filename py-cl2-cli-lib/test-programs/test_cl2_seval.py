import cl2_client
session = cl2_client.Session()

def test(expr):
    print(expr)
    print(repr(session.eval(expr)))

try:
    test("do x = 555; return x end")
    test("do function f() local x = 555; return x end; return f() end")
    test("return type(os.time())")
    test("return os.time()")
    test("""os.time(); os.time(); return os.time()""")
    test("return math['pi']")
    test("return cl2['version']")
    test("return type(cl2.shutdown)")
    test("return type(cl2.iap_id_by_name)")
    # Note that we must be sure that everything evaluates to a string coercable value.
    test("do x = cl2.iap_id_by_name(\"Saunalahti Internet\"); if x then return x else return \"no such IAP\" end end")
    test("do x = cl2.iap_id_by_name(\"Elisa Internet\"); if x then return x else return \"no such IAP\" end end")
    test("return type(cl2.config_set)")
    test("return type(cl2.config_get)")
    #test(""" return os.time(1,2) """) # causes a crash, who is to blame, most functions do not appear to check number of arguments, or course should not crash our server
    test(""" cl2.config_set("foo", " return 555 "); return "done" """)
    test("""do x = cl2.config_get("foo"); if x then return x else return "no such config key" end end""")
    #test(""" cl2.config_set("foo", " this does not parse "); return "done" """)
    test(""" if cl2.is_sensor_supported("flightmode") then return "yes" else return "no" end """)
    test(""" if cl2.is_sensor_supported("flightmode") then 
               if cl2.is_sensor_running("flightmode") then
	         return "is running"
	       else
	         return "not running"
               end
             else return "not supported" end """)
    #test(""" cl2.sensor_stop("flightmode"); return "stopped" """)
    #test(""" cl2.sensor_start("flightmode"); return "started" """)
    #test(""" cl2.sensor_stop("flightmode"); cl2.sensor_start("flightmode"); return "restarted" """)
    test(""" cl2.sensor_start("timer"); return "started" """)
finally:
    session.close()
print "all done"
