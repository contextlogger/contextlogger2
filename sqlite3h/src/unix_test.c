#include <stdio.h>
#include "sqlite3.h"



/*
** {F12119} The [sqlite3_exec()] routine sets the 3rd parameter of its 
**          callback to be an array of pointers to strings holding the
**          values for each column in the current result set row as
**          obtained from [sqlite3_column_text()].
**
** {F12122} The [sqlite3_exec()] routine sets the 4th parameter of its
**          callback to be an array of pointers to strings holding the
**          names of result columns as obtained from [sqlite3_column_name()].
*/
static int callback(void* udata, int numcols, char* vals[], char* colnames[])
{
  int i;
  for (i=0; i<numcols; i++) {
    printf("%s: %s\n", colnames[i], vals[i]);
  }
  return 0; // no problem here
}



static void testOpenFailErrMsg()
{
  sqlite3 *db = 0;

  int error = sqlite3_open("/very/strange/file.db", &db);
  if (error) {
    fprintf(stderr, "Can't open database: %s\n", sqlite3_errmsg(db));
  }
  sqlite3_close(db);
}


static void testInMemDb()
{
  sqlite3 *db;
  int error = sqlite3_open(":memory:", &db);

  if (error == SQLITE_OK) {
    char* errmsg = NULL;
    error = sqlite3_exec(db, 
			 "create table notes (t text); insert into notes (t) values ('some text'); select * from notes; drop table notes",
			 &callback,
			 NULL,
			 &errmsg);
    if (errmsg) {
      printf(errmsg);
      free(errmsg);
    }
    
    sqlite3_close(db);
  }
}


static void testFileDb()
{
  sqlite3 *db;
  int error = sqlite3_open("file.db", &db);

  if (error == SQLITE_OK) {
    char* errmsg = NULL;
    error = sqlite3_exec(db, 
			 "create table notes (t text); insert into notes (t) values ('some text'); select * from notes; drop table notes",
			 &callback,
			 NULL,
			 &errmsg);
    if (errmsg) {
      printf(errmsg);
      free(errmsg);
    }
    
    sqlite3_close(db);
  }
}


int main()
{

  testInMemDb();

  testFileDb();

  testOpenFailErrMsg();

  return 0;
}
