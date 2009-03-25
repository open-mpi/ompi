/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2008, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#include "config.h"

#include "vt_thrd.h"
#include "vt_metric.h"
#include "vt_pform.h"
#include "vt_error.h"
#include "vt_env.h"
#include "vt_trc.h"

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>


#if (defined (VT_MPI) || defined (VT_OMPI))
#include "mpi.h"
#include "vt_sync.h"
#endif
#if (defined (VT_OMPI) || defined (VT_OMP))
#include <omp.h>
#endif

static uint32_t tnum = 0; 

/* create thread object with initialised base filename and metrics,
   but without allocating buffers or files */

VTThrd* VTThrd_create(uint32_t tid)
{
  VTThrd *thread;
#if (defined (VT_METR))  
  int num_metrics = vt_metric_num();
#endif

  if (tnum > (uint32_t)vt_env_max_threads())
    vt_error_msg("FATAL: Cannot create more than %d threads", vt_env_max_threads());

  thread = (VTThrd*)malloc(sizeof(VTThrd));
  if ( thread == NULL )
    vt_error();

  thread->tmp_name = (char*)calloc(VT_PATH_MAX + 1, sizeof(char));
  if ( thread->tmp_name == NULL )
    vt_error();

  /* basename includes local path but neither thread identifier nor suffix */
  snprintf(thread->tmp_name, VT_PATH_MAX, "%s/%s.%lx.%u",
	   vt_env_ldir(), vt_env_fprefix(),
	   vt_pform_node_id(), getpid());

  thread->stack_level = 0;

  thread->omp_collop_stime = 0;

#if (defined (VT_MEMHOOK))

  thread->mem_app_alloc = 0;

#endif

#if (defined (VT_METR))  

  if (num_metrics > 0) {
    /* create event set */
    thread->metv = vt_metric_create();

    /* initialize per-thread arrays for counter values */
    thread->valv = (uint64_t*)calloc(num_metrics, sizeof(uint64_t));
    if ( thread->valv == NULL )
      vt_error();
  }

#endif

#if (defined (RFG))
  /* initialize region filter and grouping management */
  thread->rfg_regions = RFG_Regions_init();
  
  if( thread->rfg_regions == NULL )
    vt_error_msg("Could not initialize region filter and grouping management");
#endif

  /* enable tracing */
  thread->is_trace_on = 1;


  /* increment the thread object counter (for successful creations) */

#if (defined (VT_OMPI) || defined (VT_OMP))
#pragma omp atomic
    tnum++;
#else 
  tnum++;
#endif

  vt_cntl_msg("Thread object #%u created, total number is %u", tid, tnum);
  
  return thread;
}


void VTThrd_open(VTThrd* thrd, uint32_t tid)
{
  uint8_t mode = (uint8_t)vt_env_mode();
  size_t bsize = vt_env_bsize();
#if (defined (VT_OMPI) || defined (VT_OMP))
  if (tid == 0) { /* master thread gets most buffer space */
    bsize = (bsize / 10) * 7;
  } else {        /* worker threads get less buffer space */
    bsize = (bsize / 10);
  }
#endif

  if (thrd && thrd->tmp_name)
    thrd->gen = VTGen_open(thrd->tmp_name, tid, bsize, mode);
}


void VTThrd_close(VTThrd* thrd)
{
  if (thrd && thrd->tmp_name)
  {
    uint64_t time;
    while(thrd->stack_level > 0) {
      time = vt_pform_wtime();
      vt_exit(&time);
    }
    VTGen_close(thrd->gen);
  }
}


void VTThrd_delete(VTThrd* thrd, uint32_t tid)
{
  if (!thrd) return;

  if (thrd->gen)
    VTGen_delete(thrd->gen);

#if (defined (VT_METR))
  if (thrd->metv && vt_metric_num() > 0 )
    vt_metric_free(thrd->metv);
#endif
  
#if (defined (RFG))
  if (thrd->rfg_regions)
    RFG_Regions_free( thrd->rfg_regions );
#endif

  if (thrd->tmp_name)
    free(thrd->tmp_name);

  free(thrd);

  /* decrement the thread object counter */
#if (defined (VT_OMPI) || defined (VT_OMP))
#pragma omp atomic
    tnum--;
#else 
  tnum--;
#endif

  vt_cntl_msg("Thread object #%u deleted, leaving %u", tid, tnum);
}


uint32_t VTThrd_get_num_thrds()
{
  return tnum;
}
