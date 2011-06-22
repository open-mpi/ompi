/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2011, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#include "config.h"

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "vt_thrd.h"
#include "vt_metric.h"
#include "vt_pform.h"
#include "vt_error.h"
#include "vt_env.h"
#include "vt_trc.h"
#include "vt_iowrap.h"


#if defined(VT_PLUGIN_CNTR)
# include "vt_plugin_cntr_int.h"
#endif /* VT_PLUGIN_CNTR */


/* vector of the thread objects */
VTThrd** VTThrdv = NULL;

/* number of thread objects */
uint32_t VTThrdn = 1;

/* maximum number of threads */
uint32_t VTThrdMaxNum = 0;

/* mutexes for locking */
#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
VTThrdMutex* VTThrdMutexEnv = NULL;
VTThrdMutex* VTThrdMutexIds = NULL;
#endif /* VT_MT || VT_HYB || VT_JAVA */

void VTThrd_init()
{
  /* get the maximum number of threads */
  VTThrdMaxNum = (uint32_t)vt_env_max_threads();

  /* create vector of the thread objects */
  VTThrdv = (VTThrd**)calloc(VTThrdMaxNum, sizeof(VTThrd*));
  if ( VTThrdv == NULL )
    vt_error();

#if (defined(VT_MT) || defined (VT_HYB) || defined(VT_JAVA))

  /* initialize thread-type specifics */
# if defined(VT_THRD_PTHREAD)
    VTThrd_initPthread();
# elif defined(VT_THRD_OMP)
    VTThrd_initOmp();
# elif defined(VT_JAVA)
    VTThrd_initJava();
# endif /* VT_THRD_[PTHREAD|OMP] || VT_JAVA */

  /* create mutexes for locking */
  VTThrd_createMutex(&VTThrdMutexEnv);
  VTThrd_createMutex(&VTThrdMutexIds);

#endif /* VT_MT || VT_HYB || VT_JAVA */

  /* create object for master thread
     (for Java this will be done in VTThrd_initJava(),
      'cause it gets the read thread name) */
#if !defined(VT_JAVA)
  VTThrd_create(0, 0, NULL, 0);
  VTThrd_open(0);
#endif /* VT_JAVA */
}

void VTThrd_finalize()
{
#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
  /* delete mutexes for locking */
  VTThrd_deleteMutex(&VTThrdMutexEnv);
  VTThrd_deleteMutex(&VTThrdMutexIds);
#endif /* VT_MT || VT_HYB || VT_JAVA */

  if ( VTThrdv != NULL )
    free(VTThrdv);
}

uint32_t VTThrd_createNewThreadId()
{
  uint32_t tid;

#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
  VTTHRD_LOCK_ENV();
#endif /* VT_MT || VT_HYB || VT_JAVA */
  if ( VTThrdn > VTThrdMaxNum )
  {
#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
    VTTHRD_UNLOCK_ENV();
#endif /* VT_MT || VT_HYB || VT_JAVA */
    vt_error_msg("Cannot create more than %d threads", VTThrdMaxNum);
  }
  tid = VTThrdn;
  VTThrdn++;
#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
  VTTHRD_UNLOCK_ENV();
#endif /* VT_MT || VT_HYB || VT_JAVA */

  return tid;
}

void VTThrd_create(uint32_t tid, uint32_t ptid, const char* tname, uint8_t is_virtual)
{
  VTThrd *thread;
#if defined(VT_METR)
  uint32_t num_metrics = (uint32_t)vt_metric_num();
#endif /* VT_METR */
#if defined(VT_RUSAGE)
  uint32_t num_rusage = (uint32_t)vt_rusage_num();
#endif /* VT_RUSAGE */

  thread = (VTThrd*)calloc(1, sizeof(VTThrd));
  if ( thread == NULL )
    vt_error();

  /* set thread name, if available */
  if ( tname == NULL )
  {
    if ( tid == 0 ) tname = "Process";
    else tname = "Thread";
  }

  /* set thread name */
  strncpy(thread->name, tname, sizeof(thread->name));
  thread->name[sizeof(thread->name)-1] = '\0';

  /* set thread name suffix */
  if ( tid != 0 )
  {
#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
    VTTHRD_LOCK_ENV();
#endif /* VT_MT || VT_HYB || VT_JAVA */

    snprintf(thread->name_suffix, sizeof(thread->name_suffix)-1, "%s:%d",
             VTThrdv[ptid]->name_suffix, ++(VTThrdv[ptid]->child_num));

#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
    VTTHRD_UNLOCK_ENV();
#endif /* VT_MT || VT_HYB || VT_JAVA */
  }

  /* set parent ID of thread */
  thread->parent_tid = ptid;

  /* set the virtual thread flag */
  thread->is_virtual_thread = is_virtual;

#if defined(VT_GETCPU)
  thread->cpuid_val = (uint32_t)-1;
#endif /* VT_GETCPU */

#if defined(VT_RUSAGE)
  if ( num_rusage > 0 )
  {
    /* create rusage object */
    thread->ru_obj = vt_rusage_create();

    /* initialize per-thread arrays for rusage counter values */
    thread->ru_valv = (uint64_t*)calloc(num_rusage, sizeof(uint64_t));
    if ( thread->ru_valv == NULL )
      vt_error();

    /* initialize next timestamp for reading rusage counters */
    thread->ru_next_read = 0;
  }
#endif /* VT_RUSAGE */

#if defined(VT_METR)
  if ( num_metrics > 0 && is_virtual == 0)
  {
    /* create event set */
    thread->metv = vt_metric_create();

# if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
    /* initialize per-thread arrays for counter offsets */
    thread->offv = (uint64_t*)calloc(num_metrics, sizeof(uint64_t));
    if ( thread->offv == NULL )
      vt_error();
#endif /* VT_MT || VT_HYB || VT_JAVA */

    /* initialize per-thread arrays for counter values */
    thread->valv = (uint64_t*)calloc(num_metrics, sizeof(uint64_t));
    if ( thread->valv == NULL )
      vt_error();
  }
#endif /* VT_METR */

#if !defined(VT_DISABLE_RFG)
  /* initialize region filter and grouping management */
  thread->rfg_regions = RFG_Regions_init();

  if( thread->rfg_regions == NULL )
    vt_error_msg("Could not initialize region filter and grouping management");
#endif /* VT_DISABLE_RFG */

  /* enable tracing */
  thread->trace_status = VT_TRACE_ON;

  VTThrdv[tid] = thread;

#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
  VTTHRD_LOCK_ENV();
  vt_cntl_msg(2, "Thread object #%u created, total number is %u",
              tid, VTThrdn);
  VTTHRD_UNLOCK_ENV();
#else /* VT_MT || VT_HYB || VT_JAVA */
  vt_cntl_msg(2, "Thread object #%u created, total number is %u",
                 tid, VTThrdn);
#endif /* VT_MT || VT_HYB || VT_JAVA */
}

void VTThrd_open(uint32_t tid)
{
  VTThrd* thrd = VTThrdv[tid];
  size_t bsize = vt_env_bsize();
#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
  if ( tid == 0 ) { /* master thread gets most buffer space */
    bsize = (bsize / 10) * 7;
  } else {        /* worker threads get less buffer space */
    bsize = (bsize / 10);
  }
#endif /* VT_MT || VT_HYB || VT_JAVA */

  if ( thrd )
  {
    thrd->gen = VTGen_open(thrd->name, thrd->name_suffix,
                           thrd->parent_tid, tid, bsize);
  }

#if (defined(VT_PLUGIN_CNTR) || defined(VT_CUDARTWRAP))
  if ( tid != 0 && VTThrdv[tid]->is_virtual_thread )
    return;
#endif /* VT_PLUGIN_CNTR || VT_CUDARTWRAP */

#if (defined (VT_MPI) || defined (VT_HYB))
  /* initialize first matching ID for MPI collective ops. */
  thrd->mpicoll_next_matchingid = 1;
#endif /* VT_MPI || VT_HYB */

#if (defined (VT_IOWRAP) || (defined(HAVE_MPI2_IO) && HAVE_MPI2_IO))
  /* initialize first matching ID and handle */
  thrd->io_next_matchingid = 1;
  thrd->io_next_handle = 1;
#endif /* VT_IOWRAP || HAVE_MPI2_IO */
#if defined(VT_IOWRAP)
  if ( vt_env_iotrace() )
  {
    vt_iowrap_init();
    VT_ENABLE_IO_TRACING();
  }
#endif /* VT_IOWRAP */

#if defined(VT_PLUGIN_CNTR)
  /* if we really use plugins */
  if ( vt_plugin_cntr_used && tid != 0 )
  {
    /* if this is no dummy thread */
    if ( !vt_plugin_cntr_is_registered_monitor_thread() )
    {
      vt_plugin_cntr_thread_init(thrd, tid);

      /* if this thread uses plugins */
      if ( thrd->plugin_cntr_defines )
        vt_plugin_cntr_thread_enable_counters(thrd);
    }
  }
#endif /* VT_PLUGIN_CNTR */

  /* if MPI-rank is disabled, switch tracing off for this thread */
  if( vt_my_trace_is_disabled )
    vt_trace_off(tid, 0, 1);
}

void VTThrd_close(VTThrd* thrd)
{
  if ( !thrd ) return;
  
#if defined(VT_PLUGIN_CNTR)
  if ( vt_plugin_cntr_used && thrd->plugin_cntr_defines ){
    /* then write the post mortem counters */
    vt_plugin_cntr_write_post_mortem(thrd);
  }
#endif /* VT_PLUGIN_CNTR */
  VTGen_close(thrd->gen);
}

void VTThrd_delete(VTThrd* thrd, uint32_t tid)
{
  if ( !thrd ) return;

#if !defined(VT_DISABLE_RFG)
  if ( thrd->rfg_regions )
    RFG_Regions_free(thrd->rfg_regions);
#endif /* VT_DISABLE_RFG */

  /* must be called before VTGen_delete */
#if defined(VT_PLUGIN_CNTR)
  /* if we really use plugins and this thread also uses some */
  if ( vt_plugin_cntr_used && thrd->plugin_cntr_defines ){
    vt_plugin_cntr_thread_exit(thrd);
  }
#endif /* VT_PLUGIN_CNTR */

  if ( thrd->gen )
    VTGen_delete(thrd->gen);

#if defined(VT_RUSAGE)
  if ( vt_rusage_num() > 0 )
  {
    if ( thrd->ru_obj )
    {
      vt_rusage_free(thrd->ru_obj);
      thrd->ru_obj = NULL;
    }
    if ( thrd->ru_valv )
    {
      free(thrd->ru_valv);
      thrd->ru_valv = NULL;
    }
  }
#endif /* VT_RUSAGE */

#if defined(VT_METR)
  if ( vt_metric_num() > 0 && thrd->is_virtual_thread == 0 )
  {
    if ( thrd->metv )
    {
      vt_metric_free(thrd->metv, tid);
      thrd->metv = NULL;
    }
    if ( thrd->offv )
    {
      free( thrd->offv );
      thrd->offv = NULL;
    }
    if ( thrd->valv )
    {
      free(thrd->valv);
      thrd->valv = NULL;
    }
  }
#endif /* VT_METR */
  free(thrd);

  /* decrement the thread object counter */
#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
  VTTHRD_LOCK_ENV();
  VTThrdn--;
  vt_cntl_msg(2, "Thread object #%u deleted, leaving %u", tid, VTThrdn);
  VTTHRD_UNLOCK_ENV();
#else /* VT_MT || VT_HYB || VT_JAVA */
  VTThrdn--;
  vt_cntl_msg(2, "Thread object #%u deleted, leaving %u", tid, VTThrdn);
#endif /* VT_MT || VT_HYB || VT_JAVA */
}

void VTThrd_destroy(VTThrd* thrd, uint32_t tid)
{
#if !defined(VT_DISABLE_RFG)
  RFG_Regions_free(thrd->rfg_regions);
#endif /* VT_DISABLE_RFG */

  VTGen_destroy(thrd->gen);

#if defined(VT_RUSAGE)
  if ( vt_rusage_num() > 0 )
  {
    if ( thrd->ru_obj )
    {
      vt_rusage_free(thrd->ru_obj);
      thrd->ru_obj = NULL;
    }
    if ( thrd->ru_valv )
    {
      free(thrd->ru_valv);
      thrd->ru_valv = NULL;
    }
  }
#endif /* VT_RUSAGE */

#if defined(VT_METR)
  if ( vt_metric_num() > 0 && thrd->is_virtual_thread == 0 )
  {
    if ( thrd->metv )
    {
      vt_metric_free(thrd->metv, tid);
      thrd->metv = NULL;
    }
    if ( thrd->offv )
    {
      free(thrd->offv);
      thrd->offv = NULL;
    }
    if ( thrd->valv )
    {
      free(thrd->valv);
      thrd->valv = NULL;
    }
  }
#endif /* VT_METR */

  free(thrd);

  /* decrement the thread object counter */
#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
  VTTHRD_LOCK_ENV();
  VTThrdn--;
  vt_cntl_msg(2, "Thread object #%u destroyed, leaving %u", tid, VTThrdn);
  VTTHRD_UNLOCK_ENV();
#else /* VT_MT || VT_HYB || VT_JAVA */
  VTThrdn--;
  vt_cntl_msg(2, "Thread object #%u destroyed, leaving %u", tid, VTThrdn);
#endif /* VT_MT || VT_HYB || VT_JAVA */
}
