/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef SYSLOG_H_HAS_BEEN_INCLUDED
#define SYSLOG_H_HAS_BEEN_INCLUDED

#include "debug.h"

typedef struct syslog_t syslog_t;

#define LOG_CONN     conn_syslog     /* connection stuff */
#define LOG_RESTART  restart_syslog  /* restart messages */
#define LOG_MPI      mpi_syslog      /* high level messages */
#define LOG_NOTIFIER notif_syslog    /* communications with the notifier */
#define LOG_CALLBACK cb_syslog       /* callback functions */
#define LOG_DDT      ddt_syslog      /* ddt stuff */
#define LOG_IOVEC    iovec_syslog    /* iovec informations */
#define LOG_SNIPE2   snipe2_syslog   /* snipe2 information */
#define LOG_SYS      sys_syslog      /* system messages */

extern syslog_t* conn_syslog;     /* connection stuff */
extern syslog_t* restart_syslog;  /* restart messages */
extern syslog_t* mpi_syslog;      /* high level messages */
extern syslog_t* notif_syslog;    /* communications with the notifier */
extern syslog_t* cb_syslog;       /* callback functions */
extern syslog_t* ddt_syslog;      /* ddt stuff */
extern syslog_t* iovec_syslog;    /* iovec informations */
extern syslog_t* snipe2_syslog;   /* snipe2 information */
extern syslog_t* sys_syslog;      /* system messages */

extern syslog_t* syslog_init( FILE* output, int private_id, int level );
extern void syslog_hint( syslog_t* syslog, const unsigned int level,
                         const char* file, int lineno,
                         const char* fmt, ... );
extern void syslog_warning( syslog_t* syslog,
                            const char* file, int lineno,
                            const char* fmt, ... );
extern void syslog_fatal( syslog_t* syslog,
                          const char* file, int lineno,
                          const char* fmt, ... );

#if defined(NDEBUG)
#  if defined(__GNUC__)
#    define DO_DEBUG(args...)
#    define HINT( SYSLOG, LEVEL, FMT...)
#    define WARNING( SYSLOG, FMT... ) syslog_warning( (SYSLOG), __FILE__, __LINE__, ##FMT )
#    define FATAL( SYSLOG, FMT... )   syslog_fatal( (SYSLOG), __FILE__, __LINE__, ##FMT )
#  else
#    if defined(ACCEPT_C99)
#      define DO_DEBUG( ... )
#      define HINT( SYSLOG, LEVEL, args... )
#      define WARNING( SYSLOG, FMT, args... ) syslog_warning( (SYSLOG), __FILE__, __LINE__, (FMT), __VA_ARGS__ )
#      define FATAL( SYSLOG, FMT, args... )   syslog_fatal( (SYSLOG), __FILE__, __LINE__, (FMT), __VA_ARGS__ )
#    else
#      define DO_DEBUG( Args )
       static void HINT( syslog_t* syslog, const unsigned int level, const char* fmt, ... ) { /* empty hopefully removed by the compiler */};
       extern void WARNING( syslog_t* syslog, const char* fmt, ... );
       extern void FATAL( syslog_t* syslog, const char* fmt, ... );
#    endif  /* ACCEPT_C99 */
#  endif  /* __GNUC__ */
#else  /* NDEBUG not defined */
#  if defined(__GNUC__)
#    define DO_DEBUG(args...)              args
#    define HINT( SYSLOG, LEVEL, FMT, args...)     if( (SYSLOG) != NULL ) syslog_hint( (SYSLOG), (LEVEL), __FILE__, (int)__LINE__, FMT, ##args )
#    define WARNING( SYSLOG, FMT, args... ) syslog_warning( (SYSLOG), __FILE__, __LINE__, (FMT), ##args )
#    define FATAL( SYSLOG, FMT, args... )   syslog_fatal( (SYSLOG), __FILE__, __LINE__, (FMT), ##args )
#  else
#    if defined(ACCEPT_C99)
#      define DO_DEBUG( ... )                __VA_ARGS__
#      define HINT( SYSLOG, LEVEL, args... )  if( (SYSLOG) != NULL ) syslog_hint( (SYSLOG), (LEVEL), __FILE__, (int)__LINE__, __VA_ARGS__ )
#      define WARNING( SYSLOG, FMT, args... ) syslog_warning( (SYSLOG), __FILE__, __LINE__, (FMT), __VA_ARGS__ )
#      define FATAL( SYSLOG, FMT, args... )   syslog_fatal( (SYSLOG), __FILE__, __LINE__, (FMT), __VA_ARGS__ )
#    else
#      define DO_DEBUG( Args )                      Args
       extern void HINT( syslog_t* syslog, const unsigned int level, const char* fmt, ... );
       extern void WARNING( syslog_t* syslog, const char* fmt, ... );
       extern void FATAL( syslog_t* syslog, const char* fmt, ... );
#    endif  /* ACCEPT_C99 */
#  endif  /* __GNUC__ */
#endif  /* NDEBUG */

#include <sys/socket.h>
#include <string.h>
#include <stdio.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <errno.h>
extern int errno;
#define DUMP_SOCKET_ERROR(SOCK, ERRNO) \
do { \
  struct sockaddr_in sockname; \
  socklen_t length = sizeof(struct sockaddr); \
 \
  if( getpeername( (SOCK), (struct sockaddr*)&sockname, &length ) == 0 ) { \
    WARNING( LOG_CONN, "sock %d [%s:%d] generate error %d %s\n", \
            (SOCK), inet_ntoa(sockname.sin_addr), sockname.sin_port, \
	    (ERRNO), strerror((ERRNO)) ); \
  } else { \
    WARNING( LOG_CONN, "unable to get the peer name on socket %d. error %d %s\n", \
           (SOCK), errno, strerror(errno) ); \
    if( (ERRNO) != 0 ) \
      WARNING( LOG_CONN, "  initial error was %d:%s\n", (ERRNO), strerror((ERRNO)) ); \
  } \
} while (0)

#define DUMP_SOCKET_INFO(LOGGING, SOCK, SSTR, ESTR) \
do { \
  struct sockaddr_in sockname; \
  socklen_t length = sizeof(struct sockaddr); \
 \
  if( getpeername( (SOCK), (struct sockaddr*)&sockname, &length ) == 0 ) { \
    HINT( LOGGING, 10, " %s sock %d [%s:%d] %s", (SSTR), \
          (SOCK), inet_ntoa(sockname.sin_addr), sockname.sin_port, (ESTR) ); \
  } else { \
    HINT( LOGGING, 10, " unable to get the peer name on socket %d. error %d %s\n", \
          (SOCK), errno, strerror(errno) ); \
    HINT( LOGGING, 10, " %s sock %d [??:??] %s", (SSTR), (SOCK), (ESTR) ); \
  } \
} while (0)

#endif  /* SYSLOG_H_HAS_BEEN_INCLUDED */
