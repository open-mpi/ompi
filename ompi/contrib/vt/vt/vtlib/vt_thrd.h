/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2010, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#ifndef _VT_THRD_H
#define _VT_THRD_H

#ifdef __cplusplus
#   define EXTERN extern "C" 
#else
#   define EXTERN extern 
#endif

#include "vt_defs.h"
#include "vt_error.h"
#include "vt_inttypes.h"
#include "vt_otf_gen.h"

#if defined(VT_JAVA)
# include "vt_java.h"
#endif /* VT_JAVA */

#if defined(VT_RUSAGE)
# include "vt_rusage.h"
#endif /* VT_RUSAGE */

#include "rfg.h"

#if (defined(VT_MT) || defined(VT_HYB))
# define VT_MY_THREAD    VTThrd_getThreadId()
# define VT_CHECK_THREAD VTThrd_registerThread(0)
#elif defined(VT_JAVA)
# define VT_MY_THREAD    VTThrd_getThreadId()
# define VT_CHECK_THREAD
#else
# define VT_MY_THREAD    0
# define VT_CHECK_THREAD
#endif

/*
 *-----------------------------------------------------------------------------
 * VTThrd struct holds all thread-specific data:
 * - Trace buffer and file including file name
 * - Event sets and value vector
 * - ...
 *-----------------------------------------------------------------------------
 */

typedef struct 
{
  VTGen* gen;                     /* trace file and buffer */

  char  name_prefix[512];         /* prefix of thread's name */
  char  name_suffix[128];         /* suffix of thread's name */
  char  name_extern[512];         /* external name of thread */

  int stack_level;                /* current call stack level */
  int stack_level_at_off;         /* call stack level at trace off */

  int8_t trace_status;            /* trace status:
                                     VT_TRACE_ON,
                                     VT_TRACE_OFF, or
                                     VT_TRACE_OFF_PERMANENT */

  uint32_t parent_tid;            /* parent thread id */
  uint32_t child_num;             /* number of child threads */

#if !(defined (VT_DISABLE_RFG))
  RFG_Regions* rfg_regions;
#endif

#if (defined (VT_IOWRAP))

  uint8_t io_tracing_state;       /* save value of enabled flag during
                                     suspend */
  uint8_t io_tracing_suspend_cnt; /* save how often suspend was called */
  uint8_t io_tracing_enabled;     /* actual mode of I/O tracing operation */

#endif

#if (defined (VT_IOWRAP) || (defined(HAVE_MPI2_IO) && HAVE_MPI2_IO))
  uint64_t io_next_handleid;
#endif

#if (defined (VT_GETCPU))

  uint32_t cpuid_val;             /* cpu id counter value */

#endif

#if (defined (VT_RUSAGE))

  uint64_t          ru_next_read; /* next timestamp for reading
                                     rusage counters */
  uint64_t*         ru_valv;      /* vector of rusage values */
  struct vt_rusage* ru_obj;       /* rusage object */

#endif

#if (defined (VT_METR)) 

  uint64_t*       offv;           /* vector of counter offsets */
  uint64_t*       valv;           /* vector of counter values */
  struct vt_metv* metv;           /* vector of metric objects 
                                     (i.e., the event set) */

#endif

} VTThrd;

/* Accessor macros */

#define VTTHRD_MY_VTTHRD                 (VTThrdv[VT_MY_THREAD])

/* flag: is tracing enabled? */
#define VTTHRD_TRACE_STATUS(thrd)        (thrd->trace_status)

/* trace file and buffer */
#define VTTHRD_GEN(thrd)                 (thrd->gen)

/* prefix of thread's name */
#define VTTHRD_NAME_PREFIX(thrd)         (thrd->name_prefix)

/* suffix of thread's name */
#define VTTHRD_NAME_SUFFIX(thrd)         (thrd->name_suffix)

/* external name of thread */
#define VTTHRD_NAME_EXTERNAL(thrd)       (thrd->name_extern)

/* parent thread id */
#define VTTHRD_PARENT_TID(thrd)          (thrd->parent_tid);

/* number of child threads */
#define VTTHRD_CHILD_NUM(thrd)           (thrd->child_num);

/* current call stack level */
#define VTTHRD_STACK_LEVEL(thrd)         (thrd->stack_level)

/* call stack level at trace off */
#define VTTHRD_STACK_LEVEL_AT_OFF(thrd)  (thrd->stack_level_at_off)

/* push the call stack */
#define VTTHRD_STACK_PUSH(thrd)          (thrd->stack_level)++

/* pop the call stack */
#define VTTHRD_STACK_POP(thrd)           (thrd->stack_level)--

/* RFG regions */
#define VTTHRD_RFGREGIONS(thrd)          (thrd->rfg_regions)

#if (defined (VT_IOWRAP))

/* save enabled/disabled state of I/O tracing when switching off temporarily */
#define VTTHRD_IO_TRACING_STATE(thrd)         (thrd->io_tracing_state)
#define VTTHRD_IO_TRACING_SUSPEND_CNT(thrd)   (thrd->io_tracing_suspend_cnt)

/* flag: is I/O tracing enabled? */
#define VTTHRD_IO_TRACING_ENABLED(thrd)       (thrd->io_tracing_enabled)

#endif /* VT_IOWRAP */

#if (defined (VT_IOWRAP) || (defined(HAVE_MPI2_IO) && HAVE_MPI2_IO))
#define VTTHRD_IO_NEXT_HANDLEID(thrd)         (thrd->io_next_handleid++)
#endif /* VT_IOWRAP || (HAVE_MPI2_IO && HAVE_MPI2_IO) */

#if (defined (VT_GETCPU))

/* cpu id counter value */ 
#define VTTHRD_CPUID_VAL(thrd)           (thrd->cpuid_val)

#endif /* VT_GETCPU */

#if (defined (VT_RUSAGE))

/* next timestamp for reading rusage counters */
#define VTTHRD_RU_NEXT_READ(thrd)        (thrd->ru_next_read)

/* rusage values */
#define VTTHRD_RU_VALV(thrd)             (thrd->ru_valv)

/* rusage object */
#define VTTHRD_RU_OBJ(thrd)              (thrd->ru_obj)

#endif /* VT_RUSAGE */

#if (defined (VT_METR)) 

/* vector of metric offsets */
#define VTTHRD_OFFV(thrd)                (thrd->offv)

/* vector of metric values */
#define VTTHRD_VALV(thrd)                (thrd->valv)

/* vector of metric objects (i.e., event sets) */
#define VTTHRD_METV(thrd)                (thrd->metv)

#endif /* VT_METR */


/* initialize thread object management */
EXTERN void     VTThrd_init( void );

/* finalize thread object management */
EXTERN void     VTThrd_finalize( void );

/* create thread object */
EXTERN VTThrd*  VTThrd_create( uint32_t tid, uint32_t ptid,
                               const char* tname );

/* free thread object */
EXTERN void     VTThrd_delete( VTThrd* thrd, uint32_t tid );

/* destroy thread object */
EXTERN void     VTThrd_destroy( VTThrd* thrd, uint32_t tid );

/* open associated trace file */
EXTERN void     VTThrd_open( VTThrd* thrd, uint32_t tid );

/* close associated trace file */
EXTERN void     VTThrd_close( VTThrd* thrd );

#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))

/* macros for (un)locking predefined mutexes */
#define VTTHRD_LOCK_ENV() VTThrd_lock(&VTThrdMutexEnv)
#define VTTHRD_UNLOCK_ENV() VTThrd_unlock(&VTThrdMutexEnv)
#define VTTHRD_LOCK_IDS() VTThrd_lock(&VTThrdMutexIds)
#define VTTHRD_UNLOCK_IDS() VTThrd_unlock(&VTThrdMutexIds)

typedef struct VTThrdMutex_struct VTThrdMutex;

#if !defined(VT_JAVA)
# if defined(VT_THRD_PTHREAD)
    EXTERN void VTThrd_initPthread( void );
# elif defined(VT_THRD_OMP)
    EXTERN void VTThrd_initOmp( void );
# endif /* VT_THRD_[PTHREAD|OMP] */
  EXTERN void   VTThrd_registerThread( uint32_t ptid );
#else /* VT_JAVA */
  EXTERN void   VTThrd_initJava( void );
  EXTERN void   VTThrd_registerThread( jthread thread, const char* tname );
#endif /* VT_JAVA */

/* get ID of current thread */
EXTERN uint32_t VTThrd_getThreadId( void );

/* create a mutex for locking (*mutex must be NULL) */
EXTERN void     VTThrd_createMutex( VTThrdMutex** mutex );

/* delete a mutex for locking */
EXTERN void     VTThrd_deleteMutex( VTThrdMutex** mutex );

/* lock a mutex (*mutex will be initialized, if NULL) */
EXTERN void     VTThrd_lock( VTThrdMutex** mutex );

/* unlock a mutex */
EXTERN void     VTThrd_unlock( VTThrdMutex** mutex );

/* predefined mutexes for locking ... */
EXTERN VTThrdMutex* VTThrdMutexEnv;  /* ... VT environment */
EXTERN VTThrdMutex* VTThrdMutexIds;  /* ... VT IDs */

#endif /* VT_MT || VT_HYB || VT_JAVA */

/* vector of the thread objects */
EXTERN VTThrd** VTThrdv;

/* number of thread objects */
EXTERN uint32_t VTThrdn;

#endif /* _VT_THRD_H */
