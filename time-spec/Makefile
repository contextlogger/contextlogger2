MACHINE := moment_parser
MACHINE := interval_parser
RL_INCLUDES := time_common.rl
COMMON_CFLAGS := -Wall
GNOME_CFLAGS := `pkg-config --cflags --libs glib-2.0` -DG_DISABLE_DEPRECATED=1
RUBY_CFLAGS := -fPIC -I/usr/lib/ruby/1.8/i486-linux
RUBY_LDFLAGS := -shared -lruby1.8

test : moment_parser interval_parser ruby
	./moment_parser
	./interval_parser
	ruby test_time_parser.rb

moment_parser : test_moment_parser.c moment_parser.c moment_parser.h time_utils.h time_utils.c
	gcc -o $@ $(COMMON_CFLAGS) $(GNOME_CFLAGS) test_moment_parser.c moment_parser.c time_utils.c

moment_parser.c : moment_parser.rl $(RL_INCLUDES)
	ragel -C -o $@ $<

interval_parser : test_interval_parser.c interval_parser.c interval_parser.h time_utils.h time_utils.c
	gcc -o $@ $(COMMON_CFLAGS) $(GNOME_CFLAGS) test_interval_parser.c interval_parser.c time_utils.c

interval_parser.c : interval_parser.rl $(RL_INCLUDES)
	ragel -C -o $@ $<

ruby : time_parser_rb.so

time_parser_rb.so : time_parser_rb.c interval_parser.c interval_parser.h moment_parser.c moment_parser.h time_utils.h time_utils.c
	gcc -o $@ $(COMMON_CFLAGS) $(GNOME_CFLAGS) $(RUBY_CFLAGS) $(RUBY_LDFLAGS) time_parser_rb.c interval_parser.c moment_parser.c time_utils.c 
