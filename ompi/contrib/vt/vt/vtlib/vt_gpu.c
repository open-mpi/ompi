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

#include "vt_gpu.h"
#include "vt_env.h"

uint32_t vt_gpu_groupCID;
uint32_t vt_gpu_commCID;
uint8_t *vt_gpu_prop;

/* gpu debugging flag is '0' by default */
uint8_t vt_gpu_debug = 0;

static uint8_t finalized = 0;

static void vt_gpu_createGroups(void);

/*
 * Common initialization for GPU tracing.
 * Has to be in between VTTHRD_LOCK_IDS()!!!
 */
void vt_gpu_init(void)
{
  static uint8_t initflag = 0;

  if(!initflag){

    /* create group property list for threads */
    vt_gpu_prop = (uint8_t*)calloc(VTThrdMaxNum, sizeof(uint8_t));

    /* get a communicator id for GPU communication */
    vt_gpu_commCID = vt_get_curid();
    vt_gpu_groupCID = vt_get_curid();

    vt_gpu_debug = vt_env_gputrace_debug();

    initflag = 1;
  }
}

void vt_gpu_finalize(void)
{
  if(!finalized){
#if (defined(VT_MT) || defined(VT_HYB))
    VTTHRD_LOCK_IDS();
#endif
    if(!finalized){
      vt_gpu_createGroups();

      vt_cntl_msg(2, "[GPU] vt_gpu_finalize() done");
      
      finalized = 1;
    }
#if (defined(VT_MT) || defined(VT_HYB))
    VTTHRD_UNLOCK_IDS();
#endif
  }
}

/*
 * Creates process groups for all GPU threads in trace and groups for threads,
 * which participate in GPU communication.
 */
static void vt_gpu_createGroups()
{
  uint32_t i, ctrGPUGroup, ctrGPUComm;

  ctrGPUGroup = 0;
  ctrGPUComm = 0;

  /* get number of GPU communication threads and gpu threads to determine
     array size */
  for(i = 0; i < VTThrdn; i++){
    if((vt_gpu_prop[i] & VTGPU_GPU_COMM) == VTGPU_GPU_COMM) ctrGPUComm++;
    if((vt_gpu_prop[i] & VTGPU_GPU) == VTGPU_GPU) ctrGPUGroup++;
  }

  /* create array of GPU communication threads and define group */
  if(ctrGPUComm > 0){
    uint32_t *gpu_comm_array = (uint32_t*)malloc(ctrGPUComm*sizeof(uint32_t));
    int j = 0;
    for(i = 0; i < VTThrdn; i++){
      if((vt_gpu_prop[i] & VTGPU_GPU_COMM) == VTGPU_GPU_COMM){
        gpu_comm_array[j++] = i;
      }
    }
    vt_def_gpu_comm(ctrGPUComm, gpu_comm_array, 
                    VT_UNIFY_STRID_GPU_COMM_PROCGRP, vt_gpu_commCID);
  }

  /* create array of GPU threads and define group */
  if(ctrGPUGroup > 0){
    uint32_t *gpu_group_array = (uint32_t*)malloc(ctrGPUGroup*sizeof(uint32_t));
    int j = 0;
    for(i = 0; i < VTThrdn; i++){
      if((vt_gpu_prop[i] & VTGPU_GPU) == VTGPU_GPU){
        gpu_group_array[j++] = i;
      }
    }
    vt_def_gpu_comm(ctrGPUGroup, gpu_group_array, 
                    VT_UNIFY_STRID_GPU_GROUP_PROCGRP, vt_gpu_groupCID);
  }
}

/* Uses VampirTrace Thread API to create a GPU thread
 * @param tname the name of the thread to be registered
 * @param the parent thread id
 * @param vt_tid pointer to the thread id of the thread to be registered
 */
void vt_gpu_registerThread(const char* tname, uint32_t ptid, uint32_t *vt_tid)
{
  if(!vt_is_alive){
    vt_cntl_msg(2, "VampirTrace is not alive. No GPU thread created.\n "
                   "Writing events on master thread (0)");
    return;
  }

  /* create new thread object */
  *vt_tid = VTThrd_createNewThreadId();
  VTThrd_create(*vt_tid, ptid, tname, 1);
  VTThrd_open(*vt_tid);
  vt_cntl_msg(2, "[GPU] Created thread '%s' with id: %d", tname, *vt_tid);
}
