op = package.loadlib("active.dll", "1")
op()

function activeHello()
	print "Hello\n"
	active.wait(3000)
end

ac1 = active.create(activeHello)
--ac2 = active.create(activeHello)

active.resume(ac1, "One")
--active.resume(ac2, "Two")

