#include <stdio.h>
#include <ruby.h>
#include <alloca.h>
#include "moment_parser.h"
#include "interval_parser.h"

#define Check_Time(t) \
  if (!rb_obj_is_instance_of(t, rb_cTime)) { \
    rb_raise(rb_eTypeError, "%s not a Time", #t); \
  }

/*
gboolean parse_moment(const char* s, time_t ctx, time_t now,
		      time_t* result, GError** error)
gboolean parse_interval(const char* s, time_t ctx, time_t now,
			time_t* beg, time_t* end, GError** error)
*/

// Returns a result Time Range or :always or nil. Throws an
// exception on a parse error.
static VALUE rb_parse_interval(VALUE self, VALUE s,
			       VALUE ctx, VALUE now)
{
  Check_Time(ctx);
  Check_Time(now);

  char *c_str = StringValuePtr(s);
  time_t c_ctx = NUM2LONG(rb_funcall(ctx, rb_intern("to_i"), 0));
  time_t c_now = NUM2LONG(rb_funcall(now, rb_intern("to_i"), 0));
  time_t c_result_b = 0;
  time_t c_result_e = 0;
  GError* error = NULL;
  if (!parse_interval(c_str, c_ctx, c_now, 
		      &c_result_b, &c_result_e, &error)) {
    char* msg = alloca(strlen(error->message) + 1);
    strcpy(msg, error->message);
    g_error_free(error);
    rb_raise(rb_eSyntaxError, msg);
  }

  if (!c_result_b && !c_result_e) {
    return Qnil;
  } else if (!c_result_e) {
    return ID2SYM(rb_intern("always"));
  } else {
    // We probably have to do GC registrations here. Suppose, when
    // allocating the second time object, the first one is GCd as the
    // runtime is not aware of any references to it.
    VALUE tms = rb_funcall(rb_cTime, rb_intern("at"), 1, LONG2NUM(c_result_b));
    rb_gc_register_address(&tms);
    VALUE tme = rb_funcall(rb_cTime, rb_intern("at"), 1, LONG2NUM(c_result_e));
    rb_gc_register_address(&tme);
    VALUE range = rb_funcall(rb_cRange, rb_intern("new"), 2, tms, tme);
    rb_gc_unregister_address(&tms);
    rb_gc_unregister_address(&tme);
    return range;
  }
}

// Returns the result Time or nil (indicating no such moment
// upcoming). Throws an exception on a parse error.
static VALUE rb_parse_moment(VALUE self, VALUE s,
			     VALUE ctx, VALUE now)
{
  Check_Time(ctx);
  Check_Time(now);

  char *c_str = StringValuePtr(s);
  time_t c_ctx = NUM2LONG(rb_funcall(ctx, rb_intern("to_i"), 0));
  time_t c_now = NUM2LONG(rb_funcall(now, rb_intern("to_i"), 0));
  time_t c_result = 0;
  GError* error = NULL;
  if (!parse_moment(c_str, c_ctx, c_now, &c_result, &error)) {
    // A problem here is that we must pass a string to rb_raise, one
    // which really has to be freed at some point, but rb_raise does
    // not give us a chance to do cleanup before causing a return from
    // this function. Also, wrapping the string inside a Ruby object
    // does not really work either, since Ruby would not know about
    // there being a reference to the string, and hence might GC it
    // too early, unless we used rb_gc_register_address, but if we
    // used that we would also have to later use
    // rb_gc_unregister_address, and there we are back to square one.
    // The only solution is to copy the string to the stack, either
    // using a fixed size buffer or "alloca".
    // 
    // Another alternative would be to construct the exception
    // instance first, and only then throw it.
    char* msg = alloca(strlen(error->message) + 1);
    strcpy(msg, error->message);
    g_error_free(error);
    rb_raise(rb_eSyntaxError, msg);
  }

  if (c_result == 0)
    return Qnil;
  else
    // This should be okay. The "at" method will have the number
    // object on its stack, and hence we can trust the number not to
    // get GCd too early. Besides, the number probably is not even a
    // real object, but rather just a labeled value.
    return rb_funcall(rb_cTime, rb_intern("at"), 1, LONG2NUM(c_result));
}

void Init_time_parser_rb()
{
  VALUE topModule = rb_define_module("TimeParser");
  rb_define_module_function(topModule, "parse_moment", rb_parse_moment, 3);
  rb_define_module_function(topModule, "parse_interval", rb_parse_interval, 3);
}
