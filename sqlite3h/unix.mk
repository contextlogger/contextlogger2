HEADERS := \
btree.h \
btreeInt.h \
hash.h \
hwtime.h \
os.h \
os_common.h \
mutex.h \
pager.h \
pcache.h \
sqlite3ext.h \
sqliteInt.h \
sqliteLimit.h \
vdbe.h \
vdbeInt.h \
keywordhash.h \
opcodes.h \
parse.h \
sqlite3.h

SOURCES := \
alter.c \
analyze.c \
attach.c \
auth.c \
bitvec.c \
btmutex.c \
btree.c \
build.c \
callback.c \
complete.c \
date.c \
delete.c \
expr.c \
fault.c \
func.c \
global.c \
hash.c \
insert.c \
journal.c \
legacy.c \
loadext.c \
main.c \
malloc.c \
mem0.c \
mem1.c \
mem2.c \
mem3.c \
mem5.c \
memjournal.c \
mutex.c \
mutex_noop.c \
mutex_os2.c \
mutex_unix.c \
mutex_w32.c \
os.c \
os_symbian.c \
os_win.c \
os_os2.c \
pager.c \
pcache.c \
pragma.c \
prepare.c \
printf.c \
random.c \
resolve.c \
select.c \
status.c \
table.c \
tokenize.c \
trigger.c \
utf.c \
update.c \
util.c \
vacuum.c \
vdbe.c \
vdbeapi.c \
vdbeaux.c \
vdbeblob.c \
vdbefifo.c \
vdbemem.c \
vtab.c \
walker.c \
where.c \
opcodes.c \
parse.c

H_FILES := $(patsubst %, src/sqlite3/%, $(HEADERS) sqlite3_config.h)
C_FILES := $(patsubst %, src/sqlite3/%, $(SOURCES))

test : main
	./main

# platform_config.h from ../shared
main : $(C_FILES) $(H_FILES) src/unix_test.c
	gcc -o $@ -D_HAVE_SQLITE_CONFIG_H -Isrc -Isrc/sqlite3 -I../shared src/unix_test.c $(C_FILES)

clean :
	-rm main file.db *~
