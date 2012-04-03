/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2012, ZIH, TU Dresden, Federal Republic of Germany
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

uint8_t vt_gpu_trace_idle = 0;

uint32_t vt_gpu_rid_idle = VT_NO_ID;

/* gpu debugging flag is '0' by default */
uint8_t vt_gpu_debug = 0;

uint8_t vt_gpu_error = 0;

static uint8_t vt_gpu_initialized = 0;
static uint8_t vt_gpu_finalized = 0;

static void vt_gpu_createGroups(void);

/*
 * Common initialization for GPU tracing.
 * Has to be in between VTTHRD_LOCK_IDS()!!!
 */
void vt_gpu_init(void)
{
  if(!vt_gpu_initialized){
    /* create group property list for threads */
    vt_gpu_prop = (uint8_t*)calloc(VTThrdMaxNum, sizeof(uint8_t));

    /* get a communicator id for GPU communication */
    vt_gpu_commCID = vt_get_curid();
    vt_gpu_groupCID = vt_get_curid();

    vt_gpu_trace_idle = (uint8_t)vt_env_gputrace_idle();
    vt_gpu_debug = (uint8_t)vt_env_gputrace_debug();
    vt_gpu_error = (uint8_t)vt_env_gputrace_error();

    if(vt_gpu_trace_idle && vt_env_gputrace_kernel()){
      vt_gpu_rid_idle = vt_def_region(VT_MASTER_THREAD, "compute_idle", VT_NO_ID,
                                VT_NO_LNO, VT_NO_LNO, "CUDA_IDLE", VT_FUNCTION);
    }else{
      vt_gpu_trace_idle = 0;
    }

    vt_gpu_initialized = 1;
  }
}

void vt_gpu_finalize(void)
{
  if(!vt_gpu_finalized && vt_gpu_initialized){
#if (defined(VT_MT) || defined(VT_HYB))
    VTTHRD_LOCK_IDS();
#endif
    if(!vt_gpu_finalized && vt_gpu_initialized){
      vt_gpu_createGroups();

      vt_cntl_msg(2, "[GPU] vt_gpu_finalize() done");
      
      vt_gpu_finalized = 1;
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

  /* get number of GPU communication threads and GPU threads to determine
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
        gpu_comm_array[j++] = VT_PROCESS_ID(vt_my_trace, i);
      }
    }
    
    vt_def_procgrp(VT_CURRENT_THREAD, "GPU_COMM_GLOBAL",
                   VT_PROCGRP_ISCOMMUNICATOR, ctrGPUComm, gpu_comm_array,
                   vt_gpu_commCID);
    
    free(gpu_comm_array);
  }

  /* create array of GPU threads and define group */
  if(ctrGPUGroup > 0){
    uint32_t *gpu_group_array = (uint32_t*)malloc(ctrGPUGroup*sizeof(uint32_t));
    int j = 0;
    
    for(i = 0; i < VTThrdn; i++){
      if((vt_gpu_prop[i] & VTGPU_GPU) == VTGPU_GPU){
        gpu_group_array[j++] = VT_PROCESS_ID(vt_my_trace, i);
      }
    }

    vt_def_procgrp(VT_CURRENT_THREAD, "GPU_GROUP", 0, ctrGPUGroup,
                   gpu_group_array, vt_gpu_groupCID);
    
    free(gpu_group_array);
  }
}

/* 
 * Uses VampirTrace Thread API to create a GPU thread.
 * 
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
  *vt_tid = VTThrd_create(tname, ptid, 1);
  /* open thread associated trace file */
  VTThrd_open(*vt_tid);

  vt_cntl_msg(2, "[GPU] Created thread '%s' with id: %d", tname, *vt_tid);
  
  /* set the threads property to GPU */
  vt_gpu_prop[*vt_tid] = VTGPU_GPU;
}

#if (defined(VT_CUDA) && defined(VT_CUPTI))

/*
 * Handles errors returned from CUDA driver API calls.
 * 
 * @param ecode the CUDA driver API error code
 * @param msg a message to get more detailed information about the error
 * @param the corresponding file
 * @param the line the error occurred
 */
void vt_gpu_handleCuError(CUresult ecode, const char* msg,
                          const char *file, const int line)
{
  if(msg != NULL) vt_cntl_msg(1, "[CUDA] %s", msg);
  VT_CHECK_THREAD;
  if(vt_gpu_error){
    vt_error_msg("[CUDA Error %d in <%s>:%i] (ptid %d)", ecode, file, line, VT_MY_THREAD);
  }else{
    vt_warning("[CUDA Error %d in <%s>:%i] (ptid %d)", ecode, file, line, VT_MY_THREAD);
  }
}

#endif /* defined(VT_CUDA) && defined(VT_CUPTI) */

#if (defined(VT_CUDARTWRAP) || defined(VT_CUPTI))

#include <string.h>
#include <stdio.h>

/*
 * Parse the device function name:
 * "_Z<kernel_length><kernel_name><templates>..." (no name space)
 * "_ZN<ns_length><ns_name>...<ns_length><ns_name><kernel_length>..." (with name space)
 *
 * @param kname the extracted kernel name
 * @param devFunc the CUDA internal kernel function name
 */
void vt_cuda_symbolToKernel(char *kname, const char* devFunc)
{
  int i = 0;       /* position in device function (source string) */
  int nlength = 0; /* length of name space or kernel */
  int ePos = 0;    /* position in final kernel string */
  char *curr_elem, kn_templates[VTGPU_KERNEL_STRING_SIZE];
  char *tmpEnd, *tmpElemEnd;

  /*vt_cntl_msg(1,"[CUDART] device function name: %s'", devFunc);*/

  /* init for both cases: name space available or not */
  if(devFunc[2] == 'N'){
    nlength = atoi(&devFunc[3]); /* get length of first name space */
    i = 4;
  }else{
    nlength = atoi(&devFunc[2]); /* get length of kernel */
    i = 3;
  }

  /* unless string null termination */
  while(devFunc[i] != '\0'){
    /* found either name space or kernel name (no digits) */
    if(devFunc[i] < '0' || devFunc[i] > '9'){
      /* copy name to kernel function */
      if((ePos + nlength) < VTGPU_KERNEL_STRING_SIZE){
        (void)strncpy(&kname[ePos], &devFunc[i], nlength);
        ePos += nlength; /* set next position to write */
      }else{
        nlength = VTGPU_KERNEL_STRING_SIZE - ePos;
        (void)strncpy(&kname[ePos], &devFunc[i], nlength);
        vt_cntl_msg(1,"[CUDART]: kernel name '%s' contains more than %d chars!",
                      devFunc, VTGPU_KERNEL_STRING_SIZE);
        return;
      }

      i += nlength; /* jump over name */
      nlength = atoi(&devFunc[i]); /* get length of next name space or kernel */

      /* finish if no digit after name space or kernel */
      if(nlength == 0){
        kname[ePos] = '\0'; /* set string termination */
        break;
      }else{
        if((ePos + 3) < VTGPU_KERNEL_STRING_SIZE){
          (void)strncpy(&kname[ePos], "::\0", 3);
          ePos += 2;
        }else{
          vt_cntl_msg(1,"[CUDART]: kernel name '%s' contains more than %d chars!",
                        devFunc, VTGPU_KERNEL_STRING_SIZE);
          return;
        }
      }
    }else i++;
  }

  /* copy the end of the kernel name string to extract templates */
  if(-1 == snprintf(kn_templates, VTGPU_KERNEL_STRING_SIZE, "%s", &devFunc[i+1]))
    vt_cntl_msg(1, "[CUDART]: Error parsing kernel '%s'", devFunc);
  curr_elem = kn_templates; /* should be 'L' */

  /* search templates (e.g. "_Z10cptCurrentILb1ELi10EEv6SField8SParListifff") */
  tmpEnd=strstr(curr_elem,"EE");
  /* check for templates: curr_elem[0] points to 'L' AND string contains "EE" */
  if(tmpEnd != NULL && curr_elem[0]=='L'){ /* templates exist */
    tmpEnd[1] = '\0'; /* set 2nd 'E' to \0 as string end marker */

    /* write at position 'I' with '<' */
    /* elem->name[ePos]='<'; */
    if(-1 == snprintf(&(kname[ePos]),VTGPU_KERNEL_STRING_SIZE-ePos,"<"))
      vt_cntl_msg(1,"[CUDART] Parsing templates of kernel '%s' failed!", devFunc);
    ePos++; /* continue with next character */

    do{
      int res;
      curr_elem++; /* set pointer to template type length or template type */
      /* find end of template element */
      tmpElemEnd = strchr(curr_elem + atoi(curr_elem), 'E');
      tmpElemEnd[0] = '\0'; /* set termination char after template element */
      /* find next non-digit char */
      while(*curr_elem >= '0' && *curr_elem <= '9') curr_elem++;
      /* append template value to kernel name */
      if(-1 == (res = snprintf(&(kname[ePos]),
                               VTGPU_KERNEL_STRING_SIZE-ePos,"%s,",curr_elem)))
        vt_cntl_msg(1,"[CUDART]: Parsing templates of kernel '%s' crashed!", devFunc);
      ePos += res; /* continue after template value */
      curr_elem =tmpElemEnd + 1; /* set current element to begin of next template */
    }while(tmpElemEnd < tmpEnd);
    if((ePos-1) < VTGPU_KERNEL_STRING_SIZE) (void)strncpy(&kname[ePos-1], ">\0", 2);
    else vt_cntl_msg(1,"[CUDART]: Templates of '%s' too long for internal buffer!", devFunc);
  } /* else: kernel has no templates */
  /*vt_cntl_msg(1,"[CUDART] function name: %s'",e->name);*/
}

#endif /* defined(VT_CUDARTWRAP) || defined(VT_CUPTI) */

/***************************** hashing of strings *****************************/
#include "util/hash.h"

#define VT_GPU_HASHTABLE_SIZE 1021

static vt_gpu_hn_string_t* vt_gpu_string_htab[VT_GPU_HASHTABLE_SIZE];

void* vt_gpu_stringHashPut(const char* n, uint32_t rid)
{
  uint32_t id = (uint32_t)vt_hash((uint8_t*)n, strlen(n), 0) 
              % VT_GPU_HASHTABLE_SIZE;
  vt_gpu_hn_string_t *add = 
                (vt_gpu_hn_string_t*)malloc(sizeof(vt_gpu_hn_string_t));
  
  add->sname = strdup(n);
  add->rid = rid;
  add->next = vt_gpu_string_htab[id];
  vt_gpu_string_htab[id] = add;
  
  return add;
}

void* vt_gpu_stringHashGet(const char* n)
{
  uint32_t id = (uint32_t)vt_hash((uint8_t*)n, strlen(n), 0) 
              % VT_GPU_HASHTABLE_SIZE;
  vt_gpu_hn_string_t *curr = vt_gpu_string_htab[id];
  
  while ( curr ) {
    if ( strcmp( curr->sname, n ) == 0 )
      return curr;

    curr = curr->next;
  }
  
  return NULL;
}

void vt_gpu_stringhashClear()
{
  int i;
  vt_gpu_hn_string_t* tmp_node;

  for ( i = 0; i < VT_GPU_HASHTABLE_SIZE; i++ )
  {
    while( vt_gpu_string_htab[i] )
    {
      tmp_node = vt_gpu_string_htab[i]->next;
      free( vt_gpu_string_htab[i]->sname );
      free( vt_gpu_string_htab[i] );
      vt_gpu_string_htab[i] = tmp_node;
    }
  }
}
