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

/* vector of the thread objects */
VTThrd** VTThrdv = NULL;

/* number of thread objects */
uint32_t VTThrdn = 0;

/* mutexes for locking */
#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
VTThrdMutex* VTThrdMutexEnv = NULL;
VTThrdMutex* VTThrdMutexIds = NULL;
#endif /* VT_MT || VT_HYB || VT_JAVA */

void VTThrd_init()
{
  /* create vector of the thread objects */
  VTThrdv = (VTThrd**)calloc(vt_env_max_threads(), sizeof(VTThrd*));
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
  VTThrdv[0] = VTThrd_create(0, 0, NULL);
  VTThrd_open(VTThrdv[0], 0);
#endif
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

VTThrd* VTThrd_create(uint32_t tid, uint32_t ptid, const char* tname)
{
  VTThrd *thread;
#if defined(VT_METR)
  uint32_t num_metrics = (uint32_t)vt_metric_num();
#endif /* VT_METR */
#if defined(VT_RUSAGE)
  uint32_t num_rusage = (uint32_t)vt_rusage_num();
#endif /* VT_RUSAGE */

#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
  VTTHRD_LOCK_ENV();
#endif /* VT_MT || VT_HYB || VT_JAVA */

  if (VTThrdn > (uint32_t)vt_env_max_threads())
  {
#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
    VTTHRD_UNLOCK_ENV();
#endif /* VT_MT || VT_HYB || VT_JAVA */
    vt_error_msg("Cannot create more than %d threads", vt_env_max_threads());
  }
#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
  VTTHRD_UNLOCK_ENV();
#endif /* VT_MT || VT_HYB || VT_JAVA */

  thread = (VTThrd*)calloc(1, sizeof(VTThrd));
  if ( thread == NULL )
    vt_error();

  /* set external thread name, if available */
  if (tname)
    strncpy(thread->name_extern, tname, sizeof(thread->name_extern)-1);

  /* set thread's name prefix */
  snprintf(thread->name_prefix, sizeof(thread->name_prefix)-1, "%s",
           tid == 0 ? "Process" : "Thread");

  /* set thread's name suffix */
  if (tid != 0)
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

#if defined(VT_GETCPU)
  thread->cpuid_val = (uint32_t)-1;
#endif /* VT_GETCPU */

#if defined(VT_RUSAGE)
  if (num_rusage > 0) {
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
  if (num_metrics > 0) {
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

  /* increment the thread object counter (for successful creations) */

#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
  VTTHRD_LOCK_ENV();
  VTThrdn++;
  vt_cntl_msg(2, "Thread object #%u created, total number is %u",
                 tid, VTThrdn);
  VTTHRD_UNLOCK_ENV();
#else /* VT_MT || VT_HYB || VT_JAVA */
  VTThrdn++;
  vt_cntl_msg(2, "Thread object #%u created, total number is %u",
                 tid, VTThrdn);
#endif /* VT_MT || VT_HYB || VT_JAVA */

  return thread;
}

void VTThrd_open(VTThrd* thrd, uint32_t tid)
{
  size_t bsize = vt_env_bsize();
#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
  if (tid == 0) { /* master thread gets most buffer space */
    bsize = (bsize / 10) * 7;
  } else {        /* worker threads get less buffer space */
    bsize = (bsize / 10);
  }
#endif /* VT_MT || VT_HYB || VT_JAVA */

  if (thrd)
  {
    thrd->gen = VTGen_open(thrd->name_prefix, thrd->name_suffix,
                           thrd->name_extern, thrd->parent_tid, tid, bsize);
  }

#if defined(VT_IOWRAP)
  if (vt_env_iotrace())
  {
    vt_iowrap_init();
    VT_ENABLE_IO_TRACING();
  }
#endif /* VT_IOWRAP */
}

void VTThrd_close(VTThrd* thrd)
{
  if (thrd)
    VTGen_close(thrd->gen);
}

void VTThrd_delete(VTThrd* thrd, uint32_t tid)
{
  if ( !thrd ) return;

#if !defined(VT_DISABLE_RFG)
  /* write list of regions whose call limit are reached */
  if ( thrd->rfg_regions )
  {
    char* filter_filename;

    filter_filename = (char*)calloc(VT_PATH_MAX + 1, sizeof(char));
    if ( filter_filename == NULL )
      vt_error();

    snprintf(filter_filename, VT_PATH_MAX, "%s/%s.%x.filt",
             vt_env_gdir(), vt_env_fprefix(),
             65536*tid+(vt_my_trace+1));

    RFG_Regions_dumpFilteredRegions(thrd->rfg_regions, filter_filename);

    RFG_Regions_free(thrd->rfg_regions);

    free(filter_filename);
  }
#endif /* VT_DISABLE_RFG */

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
  if ( vt_metric_num() > 0 )
  {
    if ( thrd->metv )
    {
      vt_metric_free(thrd->metv);
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
#endif
}

void VTThrd_destroy(VTThrd* thrd, uint32_t tid)
{
#if !defined(VT_DISABLE_RFG)
  RFG_Regions_free(thrd->rfg_regions);
#endif

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
  if ( vt_metric_num() > 0 )
  {
    if ( thrd->metv )
    {
      vt_metric_free(thrd->metv);
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
