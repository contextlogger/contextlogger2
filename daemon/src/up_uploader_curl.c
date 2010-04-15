// This is a Curl-based implementation of Uploader. To reduce
// maintenance effort, and to add to the number of upload methods one
// might want to switch to this implementation on Symbian as well, but
// the present toolchain will simply produce a corrupt binary if we
// try to compile this. If there ever is a fixed toolchain we can
// revisit the idea of using this implementation on Symbian.
// 
// With a proper toolchain this should build, but is in much less of a
// good shape than the _epoc variant.

#include "up_private.h"

#if __FEATURE_UPLOADER__

#include "er_errors.h"
#include "utils_cl2.h"

#include "common/logging.h"
#include "common/logging-stack.h"
#include "common/assertions.h"
#include "common/sh_utils.h"

#include <errno.h>
#include <stdlib.h> // abort
#include <string.h> // strerror
#include <sys/select.h>
#include <unistd.h> // close

// The Symbian provided document http://developer.symbian.com/main/documentation/books/books_files/pdf/P.I.P.S..pdf is a good reference when messing with threads and pipes and such, as we do here and in libcurl.
//
// Some interesting threading code in http://ting.googlecode.com/svn-history/r21/trunk/src/ting.cpp

// --------------------------------------------------
// configuration
// --------------------------------------------------

// By default libcurl uses HTTP/1.1, and not just any server is fully
// compliant. Our server.scm is fine with Expect, and so must be
// Apache.
#define ALLOW_EXPECT 1

// Upload retry time.      xxx might want to use a gradually increasing timeout if there should be multiple failing attempts
#define RETRY_TIME_IN_SECS 60

/// xxx in the symbian case the access point name should be configurable, and we may have to hack curl to make it support such access point setting -- that might even be a patch to Curl proper

// --------------------------------------------------
// logging
// --------------------------------------------------

// We use a separate file, since with two threads attempting to write
// to the same file at the same time might cause (silent) failures,
// and we like to know for sure what we have logged ended up in the
// log(s).

#define LOGGING_FILENAME "uploader.txt"
#undef logt
#undef logf
#undef logc
#define logf(f...) log_fmt(LOGGING_FILENAME, f)

// --------------------------------------------------
// prototypes
// --------------------------------------------------

#include <curl/curl.h>
#include <pthread.h>
#include <time.h>

struct _up_Uploader {
  int nudge_w; // notification pipe FD (for writing)
  int nudge_r; // notification pipe FD (for reading)
  pthread_t worker;
  pthread_mutex_t mutex; // protects shared resource access
  gboolean inited; // whether initialized, boolean
  gboolean keep_running; // whether worker should keep running
  GQueue* config; // listof(up_ConfigItem), owned
  gchar* pathname; // pathname of file to upload
  time_t next_time;
  gchar* time_expr; // expresses next snapshot time, may be NULL
  gboolean no_next_file;
  gboolean next_time_computed;
  gboolean no_next_time;
  gboolean snapshot_requested;
  gboolean wait_while;
}; 

gboolean up_Uploader_init(up_Uploader* object, 
			  GError** error);

// Stops any and all activity.
void up_Uploader_cleanup(up_Uploader* object);

static void workerReadNudge(up_Uploader* object);

// --------------------------------------------------
// errors
// --------------------------------------------------

#define up_error_new(f...) gx_error_new(up_DOMAIN, up_ERR_GENERAL, f)

static GError* up_error_new_CURLcode(CURLcode errCode)
{
  // Yes, it is not documented, but looking at the curllib source we
  // can see that invoking curl_easy_strerror is safe at any time, and
  // the result need not be freed.
  return gx_error_new(up_DOMAIN, up_ERR_GENERAL, "Curl error: %s (%d)", curl_easy_strerror(errCode), errCode);
}

static GError* up_error_new_CURLMcode(CURLMcode errCode)
{
  // Not documented, hopefully result needs no freeing.
  return gx_error_new(up_DOMAIN, up_ERR_GENERAL, "Curl error: %s (%d)", curl_multi_strerror(errCode), errCode);
}

static GError* up_error_new_posix(int errCode)
{
  return gx_error_new(up_DOMAIN, up_ERR_GENERAL, "POSIX error: %s (%d)", strerror(errCode), errCode);
}

// --------------------------------------------------
// global system state
// --------------------------------------------------

gboolean up_global_init(GError** error)
{
  log_clear(LOGGING_FILENAME);

  CURLcode errCode = curl_global_init(CURL_GLOBAL_ALL);
  if (errCode) {
    if (error)
      *error = up_error_new_CURLcode(errCode);
    return FALSE;
  }

  logf("using Curl version '%s'", curl_version());

  return TRUE;
}

void up_global_cleanup()
{
  curl_global_cleanup();
}

// --------------------------------------------------
// Curl
// --------------------------------------------------

#define MIN(a,b) (((a) < (b)) ? (a) : (b))
#define MAX(a,b) (((a) > (b)) ? (a) : (b))

typedef struct {
  size_t numRead;
  int complete;
  int gotOk;
} PostStatus;

static const char okReply[] = "OK";

static size_t curlRead(void *ptr, size_t size, size_t nmemb, 
		       void *stream)
{
  size_t total = size * nmemb;
  PostStatus* st = (PostStatus*)stream;
  if (!st->complete) {
    size_t okLen = strlen(okReply);
    size_t numCmp = MIN(okLen - st->numRead, total);
    int cmpRes = memcmp(&okReply[st->numRead], ptr, numCmp);
    if (cmpRes) {
      st->complete = 1;
      st->gotOk = 0;
    } else {
      st->numRead += total;
      if (st->numRead >= okLen) {
	st->complete = 1;
	st->gotOk = 1;
      }
    }
  }
  return total;
}

#define EASY_OR_GOTO(target,expr) \
  eerrCode = (expr);		  \
  if (eerrCode) { \
    easyRet = FALSE; \
    if (error) \
      *error = up_error_new_CURLcode(eerrCode); \
    goto target; \
  }    

#define MULTI_OR_RETURN(expr) \
  merrCode = (expr);	      \
  if (merrCode) {	      \
    if (error)					\
      *error = up_error_new_CURLMcode(merrCode); \
    return FALSE; \
  }

static gboolean driveCurl(CURLM* multihandle, up_Uploader* object, GError** error)
{
  int running_handles = 0;
  CURLMcode merrCode;
  fd_set read_fd_set, write_fd_set, exc_fd_set;
  int max_fd;
  struct timeval timeout_tv;

  for (;;) {
  again:
    merrCode = curl_multi_perform(multihandle, &running_handles);
    if (merrCode == CURLM_CALL_MULTI_PERFORM) {
      continue;
    } else if (merrCode != CURLM_OK) {
      // An actual Curl driving error.
      if (error)
	*error = up_error_new_CURLMcode(merrCode);
      return FALSE;
    } else /* (merrCode == CURLM_OK) */ {
      if (running_handles == 0)
	return TRUE; // success

      FD_ZERO(&read_fd_set);
      FD_SET(object->nudge_r, &read_fd_set);
      FD_ZERO(&write_fd_set);
      FD_ZERO(&exc_fd_set);
      MULTI_OR_RETURN(curl_multi_fdset(multihandle,
				       &read_fd_set,
				       &write_fd_set,
				       &exc_fd_set,
				       &max_fd));
      assert(max_fd != -1);
      max_fd = MAX(max_fd, object->nudge_r);
      logf("max_fd is %d", max_fd);

      long timeout;
#if LIBCURL_VERSION_NUM > 0x071002
      MULTI_OR_RETURN(curl_multi_timeout(multihandle, &timeout));
#else
      timeout = -1; // v7.16.2 gives insanely high values
#endif
      logf("timeout is %ld ms", timeout);
      if (timeout == -1) {
	// No stored timeout value.
	timeout_tv.tv_sec = 1;
	timeout_tv.tv_usec = 0;
      } else {
	timeout_tv.tv_sec = timeout / 1000;
	timeout_tv.tv_usec = (timeout % 1000) * 1000;
      }

      logf("invoking select %d", max_fd + 1);
      int sret = select(max_fd + 1, 
			&read_fd_set,
			&write_fd_set,
			&exc_fd_set,
			&timeout_tv);
      logf("select (%d)", sret);
      // sret == 0 means timeout, presumably, and sret > 0 means that
      // there has been some socket activity, presumably. sret < 0
      // (sret == -1, actually) then would indicate an error of some
      // kind, some more severe than others. See errno-base.h for some
      // common error codes on Linux.
      if (sret < 0) {
	switch(errno) {
	  // Not too severe, should try again.
	case EINTR:
	case EAGAIN:
	  {
	    logf("will retry select");
	    goto again;
	  }

	  // Severe, no point trying again.
	case ECONNRESET:
	case ENOTCONN:
	case EPIPE: // non-standard on Linux, may get this instead of ENOTCONN
	  // Not quite sure, let us err on the side of caution.
	default:
	  {
	    if (error)
	      *error = up_error_new_posix(errno);
	    return FALSE;
	  }
	} // end switch
      } else if (sret > 0) {
	if (FD_ISSET(object->nudge_r, &read_fd_set)) { 
	  workerReadNudge(object);
	  if (!object->keep_running) {
	    if (error)
	      *error = up_error_new("upload interrupted");
	    return FALSE;
	  }
	}
      }
    } // end merrCode check
  } // end for

  assert(0);
}

static gboolean doEasy(CURLM* multihandle, up_Uploader* object, GError** error)
{
  assert(multihandle && object);
  assert_error_unset(error);
  logf("doEasy");
#if 0
  if (error)
    *error = up_error_new("curl doEasy dummy error");
  return FALSE;
#endif

  CURLcode eerrCode;
  CURLMcode merrCode;
  gboolean easyRet = TRUE; // if set to false must already set "error"

  logf("doing curl_easy_init");
  CURL* easyhandle = curl_easy_init();
  if (!easyhandle) {
    if (error)
      *error = up_error_new("curl easy session init failure");
    return FALSE;
  }

  // With this done, do not go invoking curl_easy_perform on
  // easyhandle.
  logf("doing curl_multi_add_handle");
  merrCode = curl_multi_add_handle(multihandle, easyhandle);
  if (merrCode) {
    easyRet = FALSE;
    if (error)
      *error = up_error_new_CURLMcode(merrCode);
    goto cleanup_easy;
  }

  struct curl_slist *slist = NULL;

#if !ALLOW_EXPECT
  {
    struct curl_slist *new_slist = curl_slist_append(slist, "Expect: ");
    if (!new_slist) {
      easyRet = FALSE;
      if (error)
	*error = up_error_new("curl header setup failure");
      goto remove_handle;
    }
    slist = new_slist;
    eerrCode = curl_easy_setopt(easyhandle, CURLOPT_HTTPHEADER, slist);
    if (eerrCode) {
      easyRet = FALSE;
      if (error)
	*error = up_error_new_CURLcode(eerrCode);
      goto free_slist;
    }
  }
#endif

  logf("setting url");
  eerrCode = curl_easy_setopt(easyhandle, CURLOPT_URL, "http://127.0.0.1:7778/upload");
  if (eerrCode) {
    easyRet = FALSE;
    if (error)
      *error = up_error_new_CURLcode(eerrCode);
    goto free_slist;
  }    

  struct curl_httppost *post = NULL;  
  struct curl_httppost *last = NULL;  

  logf("creating form");
  eerrCode = curl_formadd(&post, &last,
			  CURLFORM_COPYNAME, "logdata",
			  CURLFORM_FILE, object->pathname,
			  CURLFORM_CONTENTTYPE, "application/octet-stream",
			  CURLFORM_END);
  if (eerrCode) {
    easyRet = FALSE;
    if (error)
      *error = up_error_new_CURLcode(eerrCode);
    goto free_slist;
  }    

  logf("setting form");
  eerrCode = curl_easy_setopt(easyhandle, CURLOPT_HTTPPOST, post);
  if (eerrCode) {
    easyRet = FALSE;
    if (error)
      *error = up_error_new_CURLcode(eerrCode);
    goto free_form;
  }    

  PostStatus st;
  memzero(st);

  logf("setting write function and data");
  EASY_OR_GOTO(free_form, curl_easy_setopt(easyhandle, CURLOPT_WRITEFUNCTION, curlRead) || curl_easy_setopt(easyhandle, CURLOPT_WRITEDATA, &st));

  char curlError[CURLOPT_ERRORBUFFER];
  curlError[0] = 0;
  logf("setting error buffer");
  EASY_OR_GOTO(free_form, curl_easy_setopt(easyhandle, CURLOPT_ERRORBUFFER, curlError));

  logf("doing driveCurl");
  if (driveCurl(multihandle, object, error)) {
    if (!(st.complete && st.gotOk)) {
      easyRet = FALSE;
      char* curlMsg = ((curlError[0]) ? curlError : "unknown");
      if (error)
	*error = up_error_new("upload failure: %s", curlMsg);
      logf("upload failure: %s", curlMsg);
    }
  } else {
    assert_error_set(error);
    easyRet = FALSE;
  }
  logf("status: %s", easyRet ? "OK" : "FAIL");

 free_form:
  curl_formfree(post);

 free_slist:
  curl_slist_free_all(slist);

#if !ALLOW_EXPECT
 remove_handle:
#endif
  if (curl_multi_remove_handle(multihandle, easyhandle))
    assert(0 && "failure removing easy handle");

 cleanup_easy:
  curl_easy_cleanup(easyhandle);

  return easyRet;
}

static gboolean doMulti(up_Uploader* object, GError** error)
{
  assert_error_unset(error);

  logf("doing curl_multi_init");
#if 1
  CURLM* multihandle = curl_multi_init();
#else
  CURLM* multihandle = (CURLM*)1;
#endif
  logf("curl_multi_init (%d)", (int)multihandle);
  if (!multihandle) {
    if (error)
      *error = up_error_new("curl multi session init failure");
    return FALSE;
  }

  //log_stack_info(LOGGING_FILENAME);

#if 0
  // for testing...
  if (multihandle) {
    curl_multi_cleanup(multihandle);
    if (error)
      *error = up_error_new("curl multi session init dummy failure");
    return FALSE;
  }
#endif

  logf("invoking doEasy"); // Symbian crash happens right after this, nothing further in the logfile
  gboolean ret = doEasy(multihandle, object, error);
  logf("returned from doEasy");

  curl_multi_cleanup(multihandle);

  assert(ret || !error || *error);
  return ret;
}

// --------------------------------------------------
// Uploader object
// --------------------------------------------------

// Note that we do our uploads strictly one at a time. This way we
// reduce the amount of incompletely transferred data, and we hardly
// have any need for multiple simultaneous logfile transfers.
// 
// When uploading big logfiles, we better hope Curl does not read the
// whole thing into memory before doing the upload. Probably not, but
// we can override that if need be.

#define up_NUDGE_STOP 0 // stop request (for cleanup)
#define up_NUDGE_NOW 'n' // snapshot request
#define up_NUDGE_CONFIG 'c' // reconfigure request, config info in queue

typedef struct {
  const gchar* key;
  const gchar* value;
} up_ConfigItem;

// This processes all the config items in the queue.
static void workerReconfigure(up_Uploader* object)
{
  pthread_mutex_lock(&object->mutex);
  for (;;) {
    up_ConfigItem* item = (up_ConfigItem*)g_queue_pop_head(object->config);
    if (!item) break;
    if (strcmp(item->key, "uploader.snapshot.time") == 0) {
      //const char* time_expr = (const char*)item->value;
      assert(0 && "time expr reconfiguration to be supported"); //xxx
    } else {
      logf("unsupported config key for uploader '%s'", item->key);
    }
    g_slice_free1(sizeof(up_ConfigItem), item);
  }
  pthread_mutex_unlock(&object->mutex);
}

static void workerHandleNudge(up_Uploader* object, char nudge)
{
  switch (nudge)
    {
    case up_NUDGE_STOP:
      {
        object->keep_running = FALSE;
        break;
      }
    case up_NUDGE_NOW:
      {
        object->snapshot_requested = TRUE;
        break;
      }
    case up_NUDGE_CONFIG:
      {
	workerReconfigure(object);
        break;
      }
    default:
      {
        assert(0);
        break;
      }
    }
}

// Invoked when there is at least one byte available for reading in
// the nudge pipe. This function modifies Uploader state accordingly
// to any requests received. Note that after calling this
// object->keep_running might no longer be true, so the caller needs
// to check that if planning to do something more before returning
// control to the main worker loop.
static void workerReadNudge(up_Uploader* object)
{
  // We can assume here that a single read() will not block.
  char buf[1];
  ssize_t rret = read(object->nudge_r, buf, 1);
  if (rret > 0) {
    assert(rret == 1);
    workerHandleNudge(object, buf[0]);
  } else {
    if (rret == 0) /* EOF */ {
      goto severe;
    } else {
      assert(rret < 0); // errno will have been set
      switch (errno)
        {
        case EAGAIN:
        case EINTR:
          {
	    break;
          }
        default:
          {
	    goto severe;
          }
        }
    }
  }
  return;

 severe:
  {
    assert(0 && "reporting unimplemented"); ///xxx need error notification to Core
  }
}

static void workerWait(up_Uploader* object, struct timeval* timeout)
{
  fd_set read_fd_set, write_fd_set, exc_fd_set;
  int max_fd = object->nudge_r;
  FD_ZERO(&read_fd_set);
  FD_ZERO(&write_fd_set);
  FD_ZERO(&exc_fd_set);
  FD_SET(object->nudge_r, &read_fd_set);
  int sret = select(max_fd + 1, 
		    &read_fd_set,
		    &write_fd_set,
		    &exc_fd_set,
		    timeout);

  // sret == 0 means timeout, presumably, and sret > 0 means that
  // there has been some FD activity, presumably (sret in that case is
  // the number of FDs that are ready, in this case should be 1). sret
  // < 0 (sret == -1, actually) then would indicate an error of some
  // kind, some more severe than others. See errno-base.h for some
  // common error codes on Linux.
  if (sret == 0) return; // timeout
  if (sret > 0) {
    assert(sret == 1);
    workerReadNudge(object);
  } else { // sret < 0
    switch(errno) {
    case EINTR:
    case EAGAIN:
      // Not too severe, should try again, but timeout needs
      // recomputing. Assuming we will be called again.
      return;
    default:
      assert(0 && "reporting unimplemented"); ///xxx need error notification to Core
    }
  }
}

// This is the function here that drives Curl. If there is an
// non-severe error here we might consider a little wait and a retry.
static void workerUploadFile(up_Uploader* object)
{
  assert(object->pathname); // we assume this since called

  // Check that the file still exists. Kind of pointless, except to
  // get better error messages.
  logf("checking for existence of '%s'", object->pathname);
  if (!g_file_test(object->pathname, G_FILE_TEST_EXISTS)) {
    g_free(object->pathname);
    object->pathname = NULL;
    return;
  }
  logf("file still exists");

  GError* localError = NULL;
  if (doMulti(object, &localError)) {
    logf("removing uploaded file");
    if (!rm_file(object->pathname, &localError)) {
      abort(); // xxx severe error, report instead
    }
    g_free(object->pathname);
    object->pathname = NULL;
  } else {
    logf("uploading of '%s' failed: %s", object->pathname, localError->message);
    g_error_free(localError);
    object->wait_while = TRUE; // for typical recovery scenario
  }
}

// What the worker does is basically the following. If there are any
// old logfiles to upload, it starts uploading them one at a time. If
// "immediate" snapshot taking has been requested or it is past time
// next one be taken, then one is taken and uploaded. Otherwise the
// worker waits for the next snapshot time. Or if there is none of
// that, the worker just hangs around waiting for requests.
// 
// Our error handling scheme here is that the worker chugs along the
// best it can. If there is a fatal error, it should notify some
// outside power that can actually do something about the problem. xxx
// Presently we do not have such a notification mechanism here.
static void* worker_task(void* arg)
{
  logf("Uploader active"); // may not be so safe to have more than one thread writing to the same file -- should use a different one xxx
  up_Uploader* object = (up_Uploader*)arg;

  //log_stack_info(LOGGING_FILENAME);

  // We have lots of different state in "object" that drives this
  // loop.
  for (;;) {
    if (!object->keep_running)
      break;

    if (object->wait_while) {
      logf("waiting a while");
      // We do any retry waits here in the main worker loop to avoid
      // stack overflow due to repeated waits.
      object->wait_while = FALSE;
      struct timeval timeout_tv;
      timeout_tv.tv_sec = RETRY_TIME_IN_SECS;
      timeout_tv.tv_usec = 0;
      workerWait(object, &timeout_tv);
    } else if (object->pathname) {
      logf("uploading '%s'", object->pathname);
      // Upload the file we already know of. Having uploaded
      // something, do make sure to free and zero object->pathname.
      //
      // Note here that a snapshot timer cannot interrupt an
      // upload, but nudges will be observed. up_NUDGE_STOP will
      // even stop any ongoing upload.
      workerUploadFile(object);
    } else if (!object->no_next_file) {
      logf("looking for old files");
      // See if there are more old files.
      getNextOldLogFile(&object->pathname, NULL);
      if (!object->pathname)
	object->no_next_file = TRUE;
    } else if (object->snapshot_requested) {
      logf("taking a snapshot");
      assert(0 && "snapshot taking not supported"); //xxx
    } else if (!object->no_next_time) {
      logf("determining wait time");
      time_t now = time(NULL); // cannot really fail
      if (!object->next_time_computed) {
	if (object->time_expr) {
	  assert(0 && "time expressions not yet supported"); //xxx
	} else {
	  object->no_next_time = TRUE;
	}
	object->next_time_computed = TRUE;
	continue;
      }
      if (now >= object->next_time) {
	object->next_time_computed = FALSE;
	object->snapshot_requested = TRUE;
      } else {
	time_t timeout = object->next_time - now;
	struct timeval timeout_tv;
	timeout_tv.tv_sec = timeout / 1000;
        timeout_tv.tv_usec = (timeout % 1000) * 1000;
	workerWait(object, &timeout_tv);
      }
    } else {
      logf("waiting for a nudge");
      // Nothing to do but read the nudge stream. We would not
      // even necessarily require the use of select().
      workerWait(object, NULL);
    }
  }
  
  logf("Uploader inactive");
  return ((void*)0);
}

// This function needs to be coded so that it either fully succeeds,
// or cleans up the partial initialization before returning with an
// error.
gboolean up_Uploader_init(up_Uploader* object, 
			  GError** error)
{
  memzero(*object);

  if (!mkdir_p(LOG_UPLOADS_DIR, error))
    return FALSE;

  // Our worker thread will receive notifications from other threads
  // via this pipe.
  int pipefd[2];
  if (pipe(pipefd)) {
    if (error)
      *error = up_error_new_posix(errno);
    return FALSE;
  }
  object->nudge_w = pipefd[1];
  object->nudge_r = pipefd[0];

  pthread_mutex_init(&object->mutex, NULL); // always returns 0

  object->keep_running = TRUE;

  // Rather than using something like this we instead want to use a
  // pipe, which meshes well with our blocking waiting on file
  // descriptor(s).
  // 
  // pthread_cond_init(&object->running_cond, NULL); // always returns 0

  object->config = g_queue_new();

  // This starts the thread.
  // 
  // On Symbian, this is implemented to create a cleanup stack and to
  // run within a TRAP automatically, and many Open C APIs (especially
  // FD related ones) require those. The story would be different if
  // we did not use a pthread function for creating the thread.
  //
  //  xxx must check whether on symbian our stack is of suitable size -- the default with pthread_create is apparently 8K, and this can be changed with pthread_attr_setstacksize
  pthread_create(&object->worker, NULL, &worker_task, object); // xxx may return an error

  // Just to prevent up_Uploader_cleanup from being run multiple times.
  object->inited = TRUE;

  return TRUE;
}

// In theory, the pipe (kernel-side) buffer might become full, causing
// the write() here to block. That would be bad, but it really is more
// theoretical than anything else given that the worker reads stuff
// pretty much as soon as it gets there. Just do not let the worker
// die without closing the pipe.
static gboolean nudge_worker(up_Uploader* object, char msg, GError** error)
{
  ssize_t ret = write(object->nudge_w, &msg, 1);
  if (ret == 1) return TRUE; // 1 byte written
  if (error) {
    if (ret == 0) {
      *error = gx_error_new(up_DOMAIN, up_ERR_GENERAL, "write function failed to write anything");
    } else {
      *error = up_error_new_posix(errno);
    }
  }
  return FALSE;
}

// Overrides any timeout to cause a new LogDb snapshot to be taken
// immediately.
gboolean up_Uploader_upload_now(up_Uploader* object, GError** error)
{
  return nudge_worker(object, up_NUDGE_NOW, error);
}

// Modifies the up_Uploader runtime configuration. Any upload that is
// in progress is not interrupted.
gboolean up_Uploader_reconfigure(up_Uploader* object,
				 const gchar* key,
				 const gchar* value, 
				 GError** error)
{
  up_ConfigItem* item = g_slice_new0(up_ConfigItem);
  pthread_mutex_lock(&object->mutex); // xxx
  g_queue_push_tail(object->config, item);
  pthread_mutex_unlock(&object->mutex); // xxx
  return nudge_worker(object, up_NUDGE_CONFIG, error);
}

static void GFunc_ConfigItem_free(gpointer data, gpointer dummy)
{
  g_slice_free1(sizeof(up_ConfigItem), data);
}

// Not thread safe. Only creator should call, but may call multiple
// times.
void up_Uploader_cleanup(up_Uploader* object)
{
  if (!object->inited)
    // Already stopped.
    return;
  object->inited = FALSE;

  object->keep_running = FALSE;
  if (!nudge_worker(object, up_NUDGE_STOP, NULL)) {
    // Failed, but we cannot risk blocking forever in pthread_join. We
    // might at most attempt to kill the thread. But there really
    // should be no error since we have created the pipe, and have not
    // closed it yet.
    log_text(PRIMARY_LOG_FILENAME, "unexpectedly failed to write to an internal pipe");
    return;
  }

  void* exitValue;
  pthread_join(object->worker, &exitValue);
  log_fmt(PRIMARY_LOG_FILENAME, "worker exited with %d", (int)exitValue);

  close(object->nudge_r);
  close(object->nudge_w);

  //pthread_cond_destroy(&object->running_cond);
  pthread_mutex_destroy(&object->mutex);
  g_queue_foreach(object->config, &GFunc_ConfigItem_free, NULL);
  g_queue_free(object->config);

  if (object->pathname)
    g_free(object->pathname);
}

up_Uploader* up_Uploader_new(LogDb* logDb, GError** error)
{
  up_Uploader* object = (up_Uploader*)malloc(sizeof(up_Uploader));
  if (!object) {
    if (error)
      *error = gx_error_no_memory;
    return NULL;
  }
  if (!up_Uploader_init(object, error)) {
    free(object);
    return NULL;
  }
  return object;
}

void up_Uploader_destroy(up_Uploader* object)
{
  if (object) {
    up_Uploader_cleanup(object);
    free(object);
  }
}

#endif // __FEATURE_UPLOADER__

/**

up_uploader_curl.c

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

 **/
