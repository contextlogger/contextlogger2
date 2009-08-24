CURRENT_CONFIG := ../daemon/src/current_config.mk

include $(CURRENT_CONFIG)

s60_30_self30 :
	cd ../daemon && make
	cd ../cxx-cl2-cli-lib && sake cert=self30 kits=s60_30
	cd ../py-cl2-cli-lib && sake cert=self30 kits=s60_30
	cd ../launcher && make CERT=self30 sis

s60_30_dev_sub :
	cd ../daemon && make
	cd ../cxx-cl2-cli-lib && sake cert=dev kits=s60_30
	cd ../py-cl2-cli-lib && sake cert=dev kits=s60_30
	cd ../keyevents && sake cert=dev kits=s60_30
	cd ../watchdog && make

s60_30_dev : s60_30_dev_sub
	cd ../launcher && make CERT=dev sis

s60_30_unsigned : s60_30_dev_sub
	cd ../launcher && make CERT=unsigned sis

s60_30_self32 :
	cd ../daemon && make
	cd ../cxx-cl2-cli-lib && sake cert=self32 kits=s60_30
	cd ../py-cl2-cli-lib && sake cert=self32 kits=s60_30
	cd ../launcher && make CERT=self32 sis
