/* -*- C -*-
 *
 * $HEADER$
 * 
 */

#include "lam_config.h"

#include "mca/lam/pcm/pcm.h"
#include "mca/lam/pcm/cofs/src/pcm_cofs.h"
#include "lam/types.h"

#include <stdio.h>
#include <sys/types.h>
#include <dirent.h>
#include <string.h>
#include <unistd.h>

#define HANDLE_FILE_NAME "pcm_cofs_handle_list"

static int handle_new_count = 0;


int 
mca_pcm_cofs_query_get_nodes(mca_pcm_rte_node_t **nodes, size_t * nodes_len,
			     int *available_procs)
{
  *nodes = NULL;
  *nodes_len = 0;
  *available_procs = 0;
  
  return LAM_ERR_NOT_SUPPORTED;
}


lam_job_handle_t 
mca_pcm_cofs_handle_new(lam_job_handle_t parent)
{
  pid_t pid;
  char *ret;
  size_t ret_len;

  /* should really make this a file lookup kind of thing */
  pid = getpid();

  ret_len = sizeof(pid_t) * 8 + strlen("pcm_cofs_job_handle") + sizeof(int) * 8 + 5;
  ret = malloc(ret_len);
  if (ret == NULL) {
    return NULL;
  }

  snprintf(ret, ret_len, "pcm_cofs_job_handle_%d_%d", (int) pid, handle_new_count);
  handle_new_count++;

  return ret;
}


lam_job_handle_t 
mca_pcm_cofs_handle_get(void)
{
  return mca_pcm_cofs_my_handle;
}


void 
mca_pcm_cofs_handle_free(lam_job_handle_t * job_handle)
{
  if (*job_handle == mca_pcm_cofs_my_handle) {
    printf("WARNING: attempting to free static internal job handle!\n");
    printf("         Did you perhaps try to free the return from handle_get()?\n");
  } else if (*job_handle != NULL) {
    free(*job_handle);
    *job_handle = NULL;
  }
}


int 
mca_pcm_cofs_job_can_spawn(lam_job_handle_t job_handle)
{
#if 1
  /* Currently, have not coded up spawning support.  Need to do
     so soon */
  return LAM_ERR_NOT_SUPPORTED;
#else
  if (job_handle != NULL) {
    return LAM_ERR_NOT_SUPPORTED;
  } else {
    return LAM_SUCCESS;
  }
#endif
}


int 
mca_pcm_cofs_job_set_arguments(lam_job_handle_t job_handle,
			       mca_pcm_control_args_t * opts,
			       size_t opts_len)
{
  /* need to implement, but not needed to get INIT going */
  return LAM_ERR_NOT_IMPLEMENTED;
}


int 
mca_pcm_cofs_job_launch_procs(lam_job_handle_t job_handle,
			      mca_pcm_rte_node_t *nodes,
			      size_t nodes_len, const char *file,
			      int argc, const char *argv[],
			      const char *env[])
{
  /* need to implement, but not needed to get INIT going */
  return LAM_ERR_NOT_IMPLEMENTED;
}


int 
mca_pcm_cofs_job_rendezvous(lam_job_handle_t job_handle)
{
  /* need to implement, but not needed to get INIT going */
  return LAM_ERR_NOT_IMPLEMENTED;
}


int 
mca_pcm_cofs_job_wait(lam_job_handle_t job_handle)
{
  /* need to implement, but not needed to get INIT going */
  return LAM_ERR_NOT_IMPLEMENTED;
}


int 
mca_pcm_cofs_job_running(lam_job_handle_t job_handle,
			 int *running)
{
  /* need to implement, but not needed to get INIT going */
  return LAM_ERR_NOT_IMPLEMENTED;
}


int 
mca_pcm_cofs_job_list_running(lam_job_handle_t ** handles,
			      size_t handles_len)
{
  /* need to implement, but not needed to get INIT going */
  return LAM_ERR_NOT_IMPLEMENTED;
}


int 
mca_pcm_cofs_proc_startup(void)
{
  int i;

  if (mca_pcm_cofs_nprocs == 0) {
    /* well, this really shouldn't happen - we know we have at least ourselves */
    return LAM_ERR_FATAL;
  }

  mca_pcm_cofs_procs = malloc(sizeof(mca_pcm_proc_t) * mca_pcm_cofs_nprocs);
  if (mca_pcm_cofs_procs == NULL) {
    return LAM_ERR_OUT_OF_RESOURCE;
  }

  for (i = 0 ; i < mca_pcm_cofs_nprocs ; ++i) {
    /* for now, assume everyone in the same job :( */
    mca_pcm_cofs_procs[i].job_handle = mca_pcm_cofs_handle_get();
    mca_pcm_cofs_procs[i].vpid = i;
  }

  return LAM_SUCCESS;
}


int 
mca_pcm_cofs_proc_get_peers(mca_pcm_proc_t **procs, size_t *nprocs)
{
  if (mca_pcm_cofs_procs == NULL) {
    return LAM_ERROR;
  }

  *procs = mca_pcm_cofs_procs;
  *nprocs = mca_pcm_cofs_nprocs;

  return LAM_SUCCESS;
}


mca_pcm_proc_t* 
mca_pcm_cofs_proc_get_me(void)
{
  return &(mca_pcm_cofs_procs[mca_pcm_cofs_my_vpid]);
}


int 
mca_pcm_cofs_proc_get_parent(void)
{
  return LAM_ERR_NOT_IMPLEMENTED;
}
