/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "syslog.h"
#include "debug.h"
#include <stdarg.h>
#include <stdlib.h>

typedef struct _log_flags {
  char* name;
  char* optname;
  syslog_t** syslog;
} log_flag_t;

struct syslog_t {
  char* name;
  int level;
  FILE* output;
  int private_id;
};

static log_flag_t logFlags[] = {
   { "CONN", "conn", &LOG_CONN },
   { "RESTART", "restart", &LOG_RESTART },
   { "NOTIFIER", "not", &LOG_NOTIFIER },
   { "MPI", "mpi", &LOG_MPI },
   { "CALLBACK", "callback", &LOG_CALLBACK },
   { "DDT", "ddt", &LOG_DDT },
   { "IOVEC", "iovec", &LOG_IOVEC },
   { "SNIPE2", "snipe2", &LOG_SNIPE2 },
   { "SYS", "sys", &LOG_SYS }
};

syslog_t* conn_syslog    = NULL;  /* connection stuff */
syslog_t* restart_syslog = NULL;  /* restart messages */
syslog_t* mpi_syslog     = NULL;  /* high level messages */
syslog_t* notif_syslog   = NULL;  /* communications with the notifier */
syslog_t* cb_syslog      = NULL;  /* callback functions */
syslog_t* ddt_syslog     = NULL;  /* ddt stuff */
syslog_t* iovec_syslog   = NULL;  /* iovec informations */
syslog_t* snipe2_syslog  = NULL;  /* snipe2 information */
syslog_t* sys_syslog     = NULL;  /* system messages */

int ftmpi_syslog_init( int private_id )
{
  char envName[128];
  char *pEnv, *pFileEnv;
  int i, pos = 0;

  pEnv = getenv( "HARNESS_FTMPI_DEBUG" );
  if( pEnv == NULL ) return 0;

  while( 1 ) {
    if( pEnv[pos] == ' ' ) continue;
    for( i = 0; i < (sizeof(logFlags) / sizeof(log_flag_t)); i++ ) {
      if( strcmp( pEnv, logFlags[i].optname ) == 0 ) {
	/* one of the flags is present */
	sprintf( envName, "HARNESS_FTMPI_%s_FILE", logFlags[i].name );
	pFileEnv = getenv( envName );
	if( pFileEnv == NULL )
	  *(logFlags[i].syslog) = syslog_init( NULL, private_id, 100 );
	else {
	  FILE* pf = fopen( pFileEnv, "w" );
	  if( pf == NULL ) {
	    fprintf( stderr, "Unable to open the file %s\n", pFileEnv );
	  } else {
	    *(logFlags[i].syslog) = syslog_init( pf, private_id, 100 );
	  }
	}
	pos += strlen(logFlags[i].optname);
	break;
      }
    }
    while( 1 ) {
      if( pEnv[pos] == '\0' ) return 1;
      if( pEnv[pos] == ',' ) pos++;
      else if( pEnv[pos] == ' ' ) pos++;
      else break;
    }
  }
  return 1;
}

syslog_t* syslog_init( FILE* output, int private_id, int level )
{
  syslog_t* pSyslog = _MALLOC( sizeof(struct syslog_t) );

  if( output == NULL ) pSyslog->output = stderr;
  else pSyslog->output = output;
  pSyslog->level = level;
  pSyslog->private_id = private_id;
/* unsigned int activeFlags = LOG_RESTART|LOG_CONN|LOG_SYS|LOG_SNIPE2; */
  return pSyslog;
}

int syslog_close( syslog_t** pSyslog )
{
  if( (*pSyslog) == NULL ) return 0;
  fflush( (*pSyslog)->output );
  _FREE( (*pSyslog) );
  *pSyslog = NULL;
  return 0;
}

void syslog_hint( syslog_t* pSyslog, const unsigned int level, const char* file, int lineno, const char* fmt, ... )
{
   va_list list;
   
   if( pSyslog == NULL ) return;
   if( level < pSyslog->level ) return;
   
   fprintf( pSyslog->output, "%x-%s:%d [H-%s] ", pSyslog->private_id, file, lineno, pSyslog->name );
   va_start( list, fmt );
   vfprintf( pSyslog->output, fmt, list );
   va_end( list );
}

void syslog_warning( syslog_t* pSyslog, const char* file, int lineno, const char* fmt, ... )
{
   va_list list;
   FILE* output;
   int id = 0;
   char* name = "UNKN";

   if( pSyslog == NULL ) output = stderr;
   else {
     output = pSyslog->output;
     id = pSyslog->private_id;
     name = pSyslog->name;
   }

   fprintf( output, "%x-%s:%d [W-%s] ", id, file, lineno, name );
   va_start( list, fmt );
   vfprintf( output, fmt, list );
   va_end( list );
}

void syslog_fatal( syslog_t* pSyslog, const char* file, int lineno, const char* fmt, ... )
{
   va_list list;
   FILE* output;
   int id = 0;
   char* name = "UNKN";

   if( pSyslog == NULL ) output = stderr;
   else {
     output = pSyslog->output;
     id = pSyslog->private_id;
     name = pSyslog->name;
   }

   fprintf( output, "%x-%s:%d [F-%s] ", id, file, lineno, name );
   va_start( list, fmt );
   vfprintf( output, fmt, list );
   va_end( list );
#if !defined(NDEBUG)
   {
      int* l = NULL;
      *l = 0;  /* generate a SEGV could be trapped with a debugger */
   }
#endif  /* NDEBUG */
}

#if !defined(NDEBUG)
#  if !defined(__GNUC__) && !defined(ACCEPT_C99)
/* Simple case as the compiler thas not support macros with
 * a variable number of arguments.
 */
void HINT( syslog_t* psyslog, const unsigned int flags, const char* fmt, ... )
{
   va_list list;

   va_start( list, fmt );
   syslog_hint( flags, "unknown", -1, fmt, list );
   va_end( list );
}
#  endif  /* !defined(__GNUC__) && !defined(ACCEPT_C99) */
#endif  /* NDEBUG */

#if !defined(__GNUC__) && !defined(ACCEPT_C99)
void WARNING( syslog_t* psyslog, const unsigned int flags, const char* fmt, ... )
{
   va_list list;

   va_start( list, fmt );
   syslog_warning( flags, "unknown", -1, fmt, list );
   va_end( list );
}

void FATAL( syslog_t* psyslog, const unsigned int flags, const char* fmt, ... )
{
   va_list list;

   va_start( list, fmt );
   syslog_fatal( flags, "unknown", -1, fmt, list );
   va_end( list );
}
#endif  /* !defined(__GNUC__) && !defined(ACCEPT_C99) */

