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

#include <omp.h>

#include "vt_defs.h"
#include "vt_env.h"
#include "vt_error.h"
#include "vt_inttypes.h"
#include "vt_metric.h"
#include "vt_thrd.h"
#include "vt_trc.h"

/* data structure which hold the actual OpenMP mutex */
struct VTThrdMutex_struct
{
  omp_lock_t m;
};

static uint32_t threadId     = VT_NO_ID;
#pragma omp threadprivate(threadId)
static uint32_t threadCount  = 1;
static uint32_t threadMaxNum = 0;

void VTThrd_initOmp()
{
  static uint8_t initflag = 1;

  if (initflag)
  {
    initflag = 0;

    /* get the maximum number of threads */
    threadMaxNum = (uint32_t)vt_env_max_threads();

    /* set ID for master thread (=0) */
    threadId = 0;

#if defined(VT_METR)
    if (vt_metric_num() > 0)
      vt_metric_thread_init((long (*)(void))(omp_get_thread_num));
#endif /* VT_METR */
  }
}

void VTThrd_registerThread(uint32_t ptid)
{
  if (!vt_is_alive) return;

  /* check whether an ID is already created for this thread */
  if (threadId == VT_NO_ID)
  {
#pragma omp critical (threadCountLock)
    threadId = threadCount++;

    /* check upper bound of thread count */
    if (threadId >= threadMaxNum)
      vt_error_msg("Cannot create more than %d threads", threadMaxNum);

    /* create new thread object */
    vt_cntl_msg(2, "Dynamic thread creation. Thread #%d", threadId);
    VTThrdv[threadId] = VTThrd_create(threadId, ptid, NULL);
    VTThrd_open(VTThrdv[threadId], threadId);
  }
}

uint32_t VTThrd_getThreadId()
{
  vt_assert(threadId != VT_NO_ID);
  return threadId;
}

void VTThrd_createMutex(VTThrdMutex** mutex)
{
# pragma omp critical (mutexInitMutex)
  {
    if (*mutex == NULL)
    {
      *mutex = (VTThrdMutex*)malloc(sizeof(VTThrdMutex));
      if (*mutex == NULL)
        vt_error();
      omp_init_lock(&((*mutex)->m));
    }
  }
}

void VTThrd_deleteMutex(VTThrdMutex** mutex)
{
  if (*mutex == NULL) return;

# pragma omp critical (mutexInitMutex)
  {
    if (*mutex != NULL )
    {
      omp_destroy_lock(&((*mutex)->m));
      free(*mutex);
      *mutex = NULL;
    }
  }
}

void VTThrd_lock(VTThrdMutex** mutex)
{
  if (*mutex == NULL)
    VTThrd_createMutex(mutex);

  omp_set_lock(&((*mutex)->m));
}

void VTThrd_unlock(VTThrdMutex** mutex)
{
  vt_assert(*mutex != NULL);

  omp_unset_lock(&((*mutex)->m));
}
