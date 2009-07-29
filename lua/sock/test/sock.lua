package.loadlib("active_sock.dll", "1")()
package.loadlib("active.dll", "1")()

function getURI (uri)
	s = socket.open()
	socket.connect(s, uri, 80)
	socket.write(s, "GET / HTTP/1.1\n\n")

	repeat 
		data = socket.read(s)
		print(data)
		active.wait(1000)
	until data == ""

	socket.close(s)
	active.stop()
end

getter = active.create(getURI)
active.resume(getter, "www.symbian.com")

active.start()

