/*
**
** The author disclaims copyright to this source code.  In place of
** a legal notice, here is a blessing:
**
**    May you do good and not evil.
**    May you find forgiveness for yourself and forgive others.
**    May you share freely, never taking more than you give.
**
******************************************************************************
**
** This file is a variant of os_unix.c, for Symbian OS / P.I.P.S.
** based systems. P.I.P.S. provides a fairly decent degree of Unix
** compatibility, with quirks.
** 
** os_unix.c might otherwise work on Symbian, but the usual
** slash/backslash issue is there, and the complicated-looking file
** locking is unlikely to work. os_symbian.c disables locking
** altogether. This is fine for applications with an
** application-specific database and only one instance running, and
** this actually is what a typical Symbian application/daemon is like,
** so it's not as bad as its sounds.
**
** To further simplify this file, we have also disabled support for
** SQLITE_THREADSAFE, but luckily a typical Symbian application is
** single-threaded.
**
** Also, we are not supporting directory syncing, as it did not appear
** to work on Symbian anyway. Do not know how important it is for
** database integrity.
**
** Ensure that you are only compiling in one of os_symbian.c and
** os_unix.c, not both. We want to keep them somewhat interchangeable
** to allow for testing the Symbian version on Linux.
**
*/
#include "sqliteInt.h"
#if SQLITE_OS_UNIX

/*
** standard include files.
*/
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <time.h>
#include <sys/time.h>
#include <errno.h>

//#include "logging.h"

/*
** If we are to be thread-safe, include the pthreads header and define
** the SQLITE_UNIX_THREADS macro.
*/
#if SQLITE_THREADSAFE
#error SQLITE_THREADSAFE unsupported
#endif

/*
** Default permissions when creating a new file
*/
#ifndef SQLITE_DEFAULT_FILE_PERMISSIONS
# define SQLITE_DEFAULT_FILE_PERMISSIONS 0644
#endif

/*
** Maximum supported path-length. In the Symbian case these actually
** should be UTF-8 encoded, and such that there are at most 256
** characters, meaning that the number of bytes can be greater
** (presuming that is okay). But we are assuming ASCII here, and this
** should be fine for any applications where the user does not get to
** name the file.
*/
#define MAX_PATHNAME 256


/*
** The unixFile structure is subclass of sqlite3_file specific for the unix
** protability layer.
*/
typedef struct unixFile unixFile;
struct unixFile {
  sqlite3_io_methods const *pMethod;  /* Always the first entry */
#ifdef SQLITE_TEST
  /* In test mode, increase the size of this structure a bit so that 
  ** it is larger than the struct CrashFile defined in test6.c.
  */
  char aPadding[32];
#endif
  int h;                    /* The file descriptor */
  int lastErrno;            /* The unix errno from the last I/O error */
};

/*
** Include code that is common to all os_*.c files
*/
#include "os_common.h"

/*
** Define various macros that are missing from some systems.
*/
#ifndef O_LARGEFILE
# define O_LARGEFILE 0
#endif
#ifdef SQLITE_DISABLE_LFS
# undef O_LARGEFILE
# define O_LARGEFILE 0
#endif
#ifndef O_NOFOLLOW
# define O_NOFOLLOW 0
#endif
#ifndef O_BINARY
# define O_BINARY 0
#endif

#if defined(__SYMBIAN32__)
#define SEP_CH '\\'
#define SEP_STR "\\"
#else
#define SEP_CH '/'
#define SEP_STR "/"
#endif

/*
** Seek to the offset passed as the second argument, then read cnt 
** bytes into pBuf. Return the number of bytes actually read.
**
** NB:  If you define USE_PREAD or USE_PREAD64, then it might also
** be necessary to define _XOPEN_SOURCE to be 500.  This varies from
** one system to another.  Since SQLite does not define USE_PREAD
** any any form by default, we will not attempt to define _XOPEN_SOURCE.
** See tickets #2741 and #2681.
*/
static int seekAndRead(unixFile *id, sqlite3_int64 offset, void *pBuf, int cnt){
  int got;
  i64 newOffset;
  TIMER_START;
#if defined(USE_PREAD)
  got = pread(id->h, pBuf, cnt, offset);
  SimulateIOError( got = -1 );
#elif defined(USE_PREAD64)
  got = pread64(id->h, pBuf, cnt, offset);
  SimulateIOError( got = -1 );
#else
  newOffset = lseek(id->h, offset, SEEK_SET);
  SimulateIOError( newOffset-- );
  if( newOffset!=offset ){
    return -1;
  }
  got = read(id->h, pBuf, cnt);
#endif
  TIMER_END;
  OSTRACE5("READ    %-3d %5d %7lld %llu\n", id->h, got, offset, TIMER_ELAPSED);
  return got;
}

/*
** Read data from a file into a buffer.  Return SQLITE_OK if all
** bytes were read successfully and SQLITE_IOERR if anything goes
** wrong.
*/
static int unixRead(
  sqlite3_file *id, 
  void *pBuf, 
  int amt,
  sqlite3_int64 offset
){
  int got;
  assert( id );
  got = seekAndRead((unixFile*)id, offset, pBuf, amt);
  if( got==amt ){
    return SQLITE_OK;
  }else if( got<0 ){
    return SQLITE_IOERR_READ;
  }else{
    /* Unread parts of the buffer must be zero-filled */
    memset(&((char*)pBuf)[got], 0, amt-got);
    return SQLITE_IOERR_SHORT_READ;
  }
}

/*
** Seek to the offset in id->offset then read cnt bytes into pBuf.
** Return the number of bytes actually read.  Update the offset.
*/
static int seekAndWrite(unixFile *id, i64 offset, const void *pBuf, int cnt){
  int got;
  i64 newOffset;
  TIMER_START;
#if defined(USE_PREAD)
  got = pwrite(id->h, pBuf, cnt, offset);
#elif defined(USE_PREAD64)
  got = pwrite64(id->h, pBuf, cnt, offset);
#else
  newOffset = lseek(id->h, offset, SEEK_SET);
  if( newOffset!=offset ){
    return -1;
  }
  got = write(id->h, pBuf, cnt);
#endif
  TIMER_END;
  OSTRACE5("WRITE   %-3d %5d %7lld %llu\n", id->h, got, offset, TIMER_ELAPSED);
  return got;
}


/*
** Write data from a buffer into a file.  Return SQLITE_OK on success
** or some other error code on failure.
*/
static int unixWrite(
  sqlite3_file *id, 
  const void *pBuf, 
  int amt,
  sqlite3_int64 offset 
){
  int wrote = 0;
  assert( id );
  assert( amt>0 );
  while( amt>0 && (wrote = seekAndWrite((unixFile*)id, offset, pBuf, amt))>0 ){
    amt -= wrote;
    offset += wrote;
    pBuf = &((char*)pBuf)[wrote];
  }
  SimulateIOError(( wrote=(-1), amt=1 ));
  SimulateDiskfullError(( wrote=0, amt=1 ));
  if( amt>0 ){
    if( wrote<0 ){
      return SQLITE_IOERR_WRITE;
    }else{
      return SQLITE_FULL;
    }
  }
  return SQLITE_OK;
}

#ifdef SQLITE_TEST
/*
** Count the number of fullsyncs and normal syncs.  This is used to test
** that syncs and fullsyncs are occuring at the right times.
*/
int sqlite3_sync_count = 0;
int sqlite3_fullsync_count = 0;
#endif

/*
** Use the fdatasync() API only if the HAVE_FDATASYNC macro is defined.
** Otherwise use fsync() in its place.
*/
#ifndef HAVE_FDATASYNC
# define fdatasync fsync
#endif

/*
** Define HAVE_FULLFSYNC to 0 or 1 depending on whether or not
** the F_FULLFSYNC macro is defined.  F_FULLFSYNC is currently
** only available on Mac OS X.  But that could change.
*/
#ifdef F_FULLFSYNC
# define HAVE_FULLFSYNC 1
#else
# define HAVE_FULLFSYNC 0
#endif


/*
** The fsync() system call does not work as advertised on many
** unix systems.  The following procedure is an attempt to make
** it work better.
**
** The SQLITE_NO_SYNC macro disables all fsync()s.  This is useful
** for testing when we want to run through the test suite quickly.
** You are strongly advised *not* to deploy with SQLITE_NO_SYNC
** enabled, however, since with SQLITE_NO_SYNC enabled, an OS crash
** or power failure will likely corrupt the database file.
*/
static int full_fsync(int fd, int fullSync, int dataOnly){
  int rc;

  /* Record the number of times that we do a normal fsync() and 
  ** FULLSYNC.  This is used during testing to verify that this procedure
  ** gets called with the correct arguments.
  */
#ifdef SQLITE_TEST
  if( fullSync ) sqlite3_fullsync_count++;
  sqlite3_sync_count++;
#endif

  /* If we compiled with the SQLITE_NO_SYNC flag, then syncing is a
  ** no-op
  */
#ifdef SQLITE_NO_SYNC
  rc = SQLITE_OK;
#else

#if HAVE_FULLFSYNC
  if( fullSync ){
    rc = fcntl(fd, F_FULLFSYNC, 0);
  }else{
    rc = 1;
  }
  /* If the FULLFSYNC failed, fall back to attempting an fsync().
   * It shouldn't be possible for fullfsync to fail on the local 
   * file system (on OSX), so failure indicates that FULLFSYNC
   * isn't supported for this file system. So, attempt an fsync 
   * and (for now) ignore the overhead of a superfluous fcntl call.  
   * It'd be better to detect fullfsync support once and avoid 
   * the fcntl call every time sync is called.
   */
  if( rc ) rc = fsync(fd);

#else 
  if( dataOnly ){
    rc = fdatasync(fd);
  }else{
    rc = fsync(fd);
  }
#endif /* HAVE_FULLFSYNC */
#endif /* defined(SQLITE_NO_SYNC) */

  return rc;
}

/*
** Make sure all writes to a particular file are committed to disk.
**
** If dataOnly==0 then both the file itself and its metadata (file
** size, access time, etc) are synced.  If dataOnly!=0 then only the
** file data is synced.
**
** Under Unix, also make sure that the directory entry for the file
** has been created by fsync-ing the directory that contains the file.
** If we do not do this and we encounter a power failure, the directory
** entry for the journal might not exist after we reboot.  The next
** SQLite to access the file will not know that the journal exists (because
** the directory entry for the journal was never created) and the transaction
** will not roll back - possibly leading to database corruption.
*/
static int unixSync(sqlite3_file *id, int flags){
  int rc;
  unixFile *pFile = (unixFile*)id;

  int isDataOnly = (flags&SQLITE_SYNC_DATAONLY);
  int isFullsync = (flags&0x0F)==SQLITE_SYNC_FULL;

  /* Check that one of SQLITE_SYNC_NORMAL or FULL was passed */
  assert((flags&0x0F)==SQLITE_SYNC_NORMAL
      || (flags&0x0F)==SQLITE_SYNC_FULL
  );

  /* Unix cannot, but some systems may return SQLITE_FULL from here. This
  ** line is to test that doing so does not cause any problems.
  */
  SimulateDiskfullError( return SQLITE_FULL );

  assert( pFile );
  OSTRACE2("SYNC    %-3d\n", pFile->h);
  rc = full_fsync(pFile->h, isFullsync, isDataOnly);
  SimulateIOError( rc=1 );
  if( rc ){
    return SQLITE_IOERR_FSYNC;
  }
  return SQLITE_OK;
}

/*
** Truncate an open file to a specified size
*/
static int unixTruncate(sqlite3_file *id, i64 nByte){
  int rc;
  assert( id );
  SimulateIOError( return SQLITE_IOERR_TRUNCATE );
  rc = ftruncate(((unixFile*)id)->h, (off_t)nByte);
  if( rc ){
    return SQLITE_IOERR_TRUNCATE;
  }else{
    return SQLITE_OK;
  }
}

/*
** Determine the current size of a file in bytes
*/
static int unixFileSize(sqlite3_file *id, i64 *pSize){
  int rc;
  struct stat buf;
  assert( id );
  rc = fstat(((unixFile*)id)->h, &buf);
  SimulateIOError( rc=1 );
  if( rc!=0 ){
    return SQLITE_IOERR_FSTAT;
  }
  *pSize = buf.st_size;

  /* When opening a zero-size database, the findLockInfo() procedure
  ** writes a single byte into that file in order to work around a bug
  ** in the OS-X msdos filesystem.  In order to avoid problems with upper
  ** layers, we need to report this file size as zero even though it is
  ** really 1.   Ticket #3260.
  */
  if( *pSize==1 ) *pSize = 0;


  return SQLITE_OK;
}

/*
** This function performs the parts of the "close file" operation 
** common to all locking schemes. It closes the directory and file
** handles, if they are valid, and sets all fields of the unixFile
** structure to 0.
*/
static int closeUnixFile(sqlite3_file *id){
  unixFile *pFile = (unixFile*)id;
  if( pFile ){
    if( pFile->h>=0 ){
      close(pFile->h);
    }
    OSTRACE2("CLOSE   %-3d\n", pFile->h);
    OpenCounter(-1);
    memset(pFile, 0, sizeof(unixFile));
  }
  return SQLITE_OK;
}

/*
** The nolockLockingContext is void
*/
typedef void nolockLockingContext;

static int nolockCheckReservedLock(sqlite3_file *id, int *pResOut) {
  *pResOut = 0;
  return SQLITE_OK;
}

static int nolockLock(sqlite3_file *id, int locktype) {
  return SQLITE_OK;
}

static int nolockUnlock(sqlite3_file *id, int locktype) {
  return SQLITE_OK;
}

/*
** Close a file.
*/
static int nolockClose(sqlite3_file *id) {
  return closeUnixFile(id);
}


/*
** Information and control of an open file handle.
*/
static int unixFileControl(sqlite3_file *id, int op, void *pArg){
  switch( op ){
    case SQLITE_FCNTL_LOCKSTATE: {
      *(int*)pArg = SQLITE_LOCK_NONE;
      return SQLITE_OK;
    }
  }
  return SQLITE_ERROR;
}

/*
** Return the sector size in bytes of the underlying block device for
** the specified file. This is almost always 512 bytes, but may be
** larger for some devices.
**
** SQLite code assumes this function cannot fail. It also assumes that
** if two files are created in the same file-system directory (i.e.
** a database and its journal file) that the sector size will be the
** same for both.
*/
static int unixSectorSize(sqlite3_file *id){
  return SQLITE_DEFAULT_SECTOR_SIZE;
}

/*
** Return the device characteristics for the file. This is always 0.
*/
static int unixDeviceCharacteristics(sqlite3_file *id){
  return 0;
}

/*
** Initialize the contents of the unixFile structure pointed to by pId.
**
** When locking extensions are enabled, the filepath and locking style 
** are needed to determine the unixFile pMethod to use for locking operations.
** The locking-style specific lockingContext data structure is created 
** and assigned here also.
*/
static int fillInUnixFile(
  sqlite3_vfs *pVfs,      /* Pointer to vfs object */
  int h,                  /* Open file descriptor of file being opened */
  sqlite3_file *pId,      /* Write to the unixFile structure here */
  const char *zFilename   /* Name of the file being opened */
){
  unixFile *pNew = (unixFile *)pId;
  int rc = SQLITE_OK;

  /* Macro to define the static contents of an sqlite3_io_methods 
  ** structure for a unix backend file. Different locking methods
  ** require different functions for the xClose, xLock, xUnlock and
  ** xCheckReservedLock methods.
  */
  #define IOMETHODS(xClose, xLock, xUnlock, xCheckReservedLock) {    \
    1,                          /* iVersion */                           \
    xClose,                     /* xClose */                             \
    unixRead,                   /* xRead */                              \
    unixWrite,                  /* xWrite */                             \
    unixTruncate,               /* xTruncate */                          \
    unixSync,                   /* xSync */                              \
    unixFileSize,               /* xFileSize */                          \
    xLock,                      /* xLock */                              \
    xUnlock,                    /* xUnlock */                            \
    xCheckReservedLock,         /* xCheckReservedLock */                 \
    unixFileControl,            /* xFileControl */                       \
    unixSectorSize,             /* xSectorSize */                        \
    unixDeviceCharacteristics   /* xDeviceCapabilities */                \
  }
  static sqlite3_io_methods aIoMethod[] = {
   IOMETHODS(nolockClose, nolockLock, nolockUnlock, nolockCheckReservedLock)
  };

  OSTRACE3("OPEN    %-3d %s\n", h, zFilename);    
  pNew->h = h;

  pNew->lastErrno = 0;
  if( rc!=SQLITE_OK ){
    close(h);
  }else{
    pNew->pMethod = &aIoMethod[0];
    OpenCounter(+1);
  }
  return rc;
}

/*
** Create a temporary file name in zBuf.  zBuf must be allocated
** by the calling process and must be big enough to hold at least
** pVfs->mxPathname bytes.
*/
static int getTempname(int nBuf, char *zBuf){
  static const char *azDirs[] = {
     0,
#ifdef __SYMBIAN32__
     "c:\\system\\temp\\sqlite3",
     "c:\\temp",
     "c:",
#else
     "/var/tmp",
     "/usr/tmp",
     "/tmp",
#endif
     ".",
  };
  static const unsigned char zChars[] =
    "abcdefghijklmnopqrstuvwxyz"
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    "0123456789";
  int i, j;
  struct stat buf;
  const char *zDir = ".";

  /* It's odd to simulate an io-error here, but really this is just
  ** using the io-error infrastructure to test that SQLite handles this
  ** function failing. 
  */
  SimulateIOError( return SQLITE_IOERR );

  azDirs[0] = sqlite3_temp_directory;
  for(i=0; i<sizeof(azDirs)/sizeof(azDirs[0]); i++){
    if( azDirs[i]==0 ) continue;
    if( stat(azDirs[i], &buf) ) continue;
    if( !S_ISDIR(buf.st_mode) ) continue;
    if( access(azDirs[i], 07) ) continue;
    zDir = azDirs[i];
    break;
  }

  /* Check that the output buffer is large enough for the temporary file 
  ** name. If it is not, return SQLITE_ERROR.
  */
  if( (strlen(zDir) + strlen(SQLITE_TEMP_FILE_PREFIX) + 17) >= nBuf ){
    return SQLITE_ERROR;
  }

  do{
    sqlite3_snprintf(nBuf-17, zBuf, "%s" SEP_STR SQLITE_TEMP_FILE_PREFIX, zDir);
    j = strlen(zBuf);
    sqlite3_randomness(15, &zBuf[j]);
    for(i=0; i<15; i++, j++){
      zBuf[j] = (char)zChars[ ((unsigned char)zBuf[j])%(sizeof(zChars)-1) ];
    }
    zBuf[j] = 0;
  }while( access(zBuf,0)==0 );
  return SQLITE_OK;
}


/*
** Open the file zPath.
** 
** Previously, the SQLite OS layer used three functions in place of this
** one:
**
**     sqlite3OsOpenReadWrite();
**     sqlite3OsOpenReadOnly();
**     sqlite3OsOpenExclusive();
**
** These calls correspond to the following combinations of flags:
**
**     ReadWrite() ->     (READWRITE | CREATE)
**     ReadOnly()  ->     (READONLY) 
**     OpenExclusive() -> (READWRITE | CREATE | EXCLUSIVE)
**
** The old OpenExclusive() accepted a boolean argument - "delFlag". If
** true, the file was configured to be automatically deleted when the
** file handle closed. To achieve the same effect using this new 
** interface, add the DELETEONCLOSE flag to those specified above for 
** OpenExclusive().
*/
static int unixOpen(
  sqlite3_vfs *pVfs, 
  const char *zPath, 
  sqlite3_file *pFile,
  int flags,
  int *pOutFlags
){
  int fd = 0;                    /* File descriptor returned by open() */
  int oflags = 0;                /* Flags to pass to open() */
#ifndef NDEBUG
  int eType = flags&0xFFFFFF00;  /* Type of file to open */
#endif

  int isExclusive  = (flags & SQLITE_OPEN_EXCLUSIVE);
  int isDelete     = (flags & SQLITE_OPEN_DELETEONCLOSE);
  int isCreate     = (flags & SQLITE_OPEN_CREATE);
  int isReadonly   = (flags & SQLITE_OPEN_READONLY);
  int isReadWrite  = (flags & SQLITE_OPEN_READWRITE);

  /* If argument zPath is a NULL pointer, this function is required to open
  ** a temporary file. Use this buffer to store the file name in.
  */
  char zTmpname[MAX_PATHNAME+1];
  const char *zName = zPath;

  /* Check the following statements are true: 
  **
  **   (a) Exactly one of the READWRITE and READONLY flags must be set, and 
  **   (b) if CREATE is set, then READWRITE must also be set, and
  **   (c) if EXCLUSIVE is set, then CREATE must also be set.
  **   (d) if DELETEONCLOSE is set, then CREATE must also be set.
  */
  assert((isReadonly==0 || isReadWrite==0) && (isReadWrite || isReadonly));
  assert(isCreate==0 || isReadWrite);
  assert(isExclusive==0 || isCreate);
  assert(isDelete==0 || isCreate);

  /* The main DB, main journal, and master journal are never automatically
  ** deleted
  */
  assert( eType!=SQLITE_OPEN_MAIN_DB || !isDelete );
  assert( eType!=SQLITE_OPEN_MAIN_JOURNAL || !isDelete );
  assert( eType!=SQLITE_OPEN_MASTER_JOURNAL || !isDelete );

  /* Assert that the upper layer has set one of the "file-type" flags. */
  assert( eType==SQLITE_OPEN_MAIN_DB      || eType==SQLITE_OPEN_TEMP_DB 
       || eType==SQLITE_OPEN_MAIN_JOURNAL || eType==SQLITE_OPEN_TEMP_JOURNAL 
       || eType==SQLITE_OPEN_SUBJOURNAL   || eType==SQLITE_OPEN_MASTER_JOURNAL 
       || eType==SQLITE_OPEN_TRANSIENT_DB
  );

  memset(pFile, 0, sizeof(unixFile));

  if( !zName ){
    int rc;
    assert(isDelete);
    rc = getTempname(MAX_PATHNAME+1, zTmpname);
    if( rc!=SQLITE_OK ){
      return rc;
    }
    zName = zTmpname;
  }

  if( isReadonly )  oflags |= O_RDONLY;
  if( isReadWrite ) oflags |= O_RDWR;
  if( isCreate )    oflags |= O_CREAT;
  if( isExclusive ) oflags |= (O_EXCL|O_NOFOLLOW);
  oflags |= (O_LARGEFILE|O_BINARY);

  fd = open(zName, oflags, isDelete?0600:SQLITE_DEFAULT_FILE_PERMISSIONS);
  if( fd<0 && errno!=EISDIR && isReadWrite && !isExclusive ){
    /* Failed to open the file for read/write access. Try read-only. */
    flags &= ~(SQLITE_OPEN_READWRITE|SQLITE_OPEN_CREATE);
    flags |= SQLITE_OPEN_READONLY;
    return unixOpen(pVfs, zPath, pFile, flags, pOutFlags);
  }
  if( fd<0 ){
    //logt("SQLITE_CANTOPEN in unixOpen");
    return SQLITE_CANTOPEN;
  }
  if( isDelete ){
    unlink(zName);
  }
  if( pOutFlags ){
    *pOutFlags = flags;
  }

  assert(fd!=0);

#ifdef FD_CLOEXEC
  fcntl(fd, F_SETFD, fcntl(fd, F_GETFD, 0) | FD_CLOEXEC);
#endif

  return fillInUnixFile(pVfs, fd, pFile, zPath);
}

/*
** Delete the file at zPath. If the dirSync argument is true, fsync()
** the directory after deleting the file.
*/
static int unixDelete(sqlite3_vfs *pVfs, const char *zPath, int dirSync){
  int rc = SQLITE_OK;
  SimulateIOError(return SQLITE_IOERR_DELETE);
  unlink(zPath);
  return rc;
}

/*
** Test the existance of or access permissions of file zPath. The
** test performed depends on the value of flags:
**
**     SQLITE_ACCESS_EXISTS: Return 1 if the file exists
**     SQLITE_ACCESS_READWRITE: Return 1 if the file is read and writable.
**     SQLITE_ACCESS_READONLY: Return 1 if the file is readable.
**
** Otherwise return 0.
*/
static int unixAccess(
  sqlite3_vfs *pVfs, 
  const char *zPath, 
  int flags, 
  int *pResOut
){
  int amode = 0;
  SimulateIOError( return SQLITE_IOERR_ACCESS; );
  switch( flags ){
    case SQLITE_ACCESS_EXISTS:
      amode = F_OK;
      break;
    case SQLITE_ACCESS_READWRITE:
      amode = W_OK|R_OK;
      break;
    case SQLITE_ACCESS_READ:
      amode = R_OK;
      break;

    default:
      assert(!"Invalid flags argument");
  }
  *pResOut = (access(zPath, amode)==0);
  return SQLITE_OK;
}


/*
** Turn a relative pathname into a full pathname. The relative path
** is stored as a nul-terminated string in the buffer pointed to by
** zPath. 
**
** zOut points to a buffer of at least sqlite3_vfs.mxPathname bytes 
** (in this case, MAX_PATHNAME bytes). The full-path is written to
** this buffer before returning.
*/
static int unixFullPathname(
  sqlite3_vfs *pVfs,            /* Pointer to vfs object */
  const char *zPath,            /* Possibly relative input path */
  int nOut,                     /* Size of output buffer in bytes */
  char *zOut                    /* Output buffer */
){

  /* It's odd to simulate an io-error here, but really this is just
  ** using the io-error infrastructure to test that SQLite handles this
  ** function failing. This function could fail if, for example, the
  ** current working directly has been unlinked.
  */
  SimulateIOError( return SQLITE_ERROR );

  assert( pVfs->mxPathname==MAX_PATHNAME );
  zOut[nOut-1] = '\0';
  // Must try to check for both with or without drive letter case.
  if( (zPath[0] == SEP_CH) ||
      (zPath[0] && (zPath[1] == ':') && zPath[2] == SEP_CH) ) {
    //logf("absolute path '%s'", zPath);
    sqlite3_snprintf(nOut, zOut, "%s", zPath);
  } else {
    int nCwd;
    if( getcwd(zOut, nOut-1)==0 ){    //logt("getcwd fail");
      return SQLITE_CANTOPEN;
    }
    nCwd = strlen(zOut);
    sqlite3_snprintf(nOut-nCwd, &zOut[nCwd], SEP_STR "%s", zPath);
  }
  return SQLITE_OK;
}


#ifndef SQLITE_OMIT_LOAD_EXTENSION
#error !SQLITE_OMIT_LOAD_EXTENSION unsupported
#else /* if SQLITE_OMIT_LOAD_EXTENSION is defined: */
  #define unixDlOpen  0
  #define unixDlError 0
  #define unixDlSym   0
  #define unixDlClose 0
#endif

/*
** Write nBuf bytes of random data to the supplied buffer zBuf.
*/
static int unixRandomness(sqlite3_vfs *pVfs, int nBuf, char *zBuf){

  assert(nBuf>=(sizeof(time_t)+sizeof(int)));

  /* We have to initialize zBuf to prevent valgrind from reporting
  ** errors.  The reports issued by valgrind are incorrect - we would
  ** prefer that the randomness be increased by making use of the
  ** uninitialized space in zBuf - but valgrind errors tend to worry
  ** some users.  Rather than argue, it seems easier just to initialize
  ** the whole array and silence valgrind, even if that means less randomness
  ** in the random seed.
  **
  ** When testing, initializing zBuf[] to zero is all we do.  That means
  ** that we always use the same random number sequence.  This makes the
  ** tests repeatable.
  */
  memset(zBuf, 0, nBuf);
#if !defined(SQLITE_TEST)
  {
    int pid;
    time_t t;
    time(&t);
    memcpy(zBuf, &t, sizeof(t));
    pid = getpid();
    memcpy(&zBuf[sizeof(t)], &pid, sizeof(pid));
    assert( sizeof(t)+sizeof(pid)<=nBuf );
    nBuf = sizeof(t) + sizeof(pid);
  }
#endif
  return nBuf;
}


/*
** Sleep for a little while.  Return the amount of time slept.
** The argument is the number of microseconds we want to sleep.
** The return value is the number of microseconds of sleep actually
** requested from the underlying operating system, a number which
** might be greater than or equal to the argument, but not less
** than the argument.
*/
static int unixSleep(sqlite3_vfs *pVfs, int microseconds){
#if defined(HAVE_USLEEP) && HAVE_USLEEP
  usleep(microseconds);
  return microseconds;
#else
  int seconds = (microseconds+999999)/1000000;
  sleep(seconds);
  return seconds*1000000;
#endif
}

/*
** The following variable, if set to a non-zero value, becomes the result
** returned from sqlite3OsCurrentTime().  This is used for testing.
*/
#ifdef SQLITE_TEST
int sqlite3_current_time = 0;
#endif

/*
** Find the current time (in Universal Coordinated Time).  Write the
** current time and date as a Julian Day number into *prNow and
** return 0.  Return 1 if the time and date cannot be found.
*/
static int unixCurrentTime(sqlite3_vfs *pVfs, double *prNow){
#ifdef NO_GETTOD
  time_t t;
  time(&t);
  *prNow = t/86400.0 + 2440587.5;
#else
  struct timeval sNow;
  gettimeofday(&sNow, 0);
  *prNow = 2440587.5 + sNow.tv_sec/86400.0 + sNow.tv_usec/86400000000.0;
#endif
#ifdef SQLITE_TEST
  if( sqlite3_current_time ){
    *prNow = sqlite3_current_time/86400.0 + 2440587.5;
  }
#endif
  return 0;
}

static int unixGetLastError(sqlite3_vfs *pVfs, int nBuf, char *zBuf){
  return 0;
}

/*
** Initialize the operating system interface.
*/
int sqlite3_os_init(void){ 
  /* Macro to define the static contents of an sqlite3_vfs structure for
  ** the unix backend. The two parameters are the values to use for
  ** the sqlite3_vfs.zName and sqlite3_vfs.pAppData fields, respectively.
  ** 
  */
  #define UNIXVFS(zVfsName, pVfsAppData) {                  \
    1,                    /* iVersion */                    \
    sizeof(unixFile),     /* szOsFile */                    \
    MAX_PATHNAME,         /* mxPathname */                  \
    0,                    /* pNext */                       \
    zVfsName,             /* zName */                       \
    (void *)pVfsAppData,  /* pAppData */                    \
    unixOpen,             /* xOpen */                       \
    unixDelete,           /* xDelete */                     \
    unixAccess,           /* xAccess */                     \
    unixFullPathname,     /* xFullPathname */               \
    unixDlOpen,           /* xDlOpen */                     \
    unixDlError,          /* xDlError */                    \
    unixDlSym,            /* xDlSym */                      \
    unixDlClose,          /* xDlClose */                    \
    unixRandomness,       /* xRandomness */                 \
    unixSleep,            /* xSleep */                      \
    unixCurrentTime,      /* xCurrentTime */                \
    unixGetLastError      /* xGetLastError */               \
  }

  static sqlite3_vfs unixVfs = UNIXVFS("unix", 0);
  sqlite3_vfs_register(&unixVfs, 1);
  return SQLITE_OK; 
}

/*
** Shutdown the operating system interface. This is a no-op for unix.
*/
int sqlite3_os_end(void){ 
  return SQLITE_OK; 
}
 
#endif /* SQLITE_OS_UNIX */
