/* -*- C -*-
 *
 * $HEADER$
 *
 */

#include "lam_config.h"

#include "mca/lam/pcm/pcm.h"
#include "mca/lam/pcm/cofs/src/pcm_cofs.h"
#include "lam/util/malloc.h"
#include "lam/types.h"

#include <stdio.h>
#include <sys/types.h>
#include <dirent.h>
#include <string.h>
#include <unistd.h>

#define HANDLE_FILE_NAME "pcm_cofs_handle_list"

static int handle_new_count = 0;

int 
mca_pcm_cofs_query_get_nodes(lam_pcm_node_t ** nodes, size_t * nodes_len,
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

  pid = getpid();

  ret_len = sizeof(pid_t) * 8 + strlen("pcm_cofs") + sizeof(int) * 8 + 5;
  ret = LAM_MALLOC(ret_len);
  if (ret == NULL) {
    return NULL;
  }

  snprintf(ret, ret_len, "pcm_cofs_%d_%d", (int) pid, handle_new_count);
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
  if (*job_handle != NULL) {
    LAM_FREE(*job_handle);
    *job_handle = NULL;
  }
}


int 
mca_pcm_cofs_job_can_spawn(lam_job_handle_t job_handle)
{
  if (job_handle != NULL) {
    return LAM_ERR_NOT_SUPPORTED;
  } else {
    return LAM_SUCCESS;
  }
}


int 
mca_pcm_cofs_job_set_arguments(lam_job_handle_t job_handle,
			       lam_pcm_control_args_t * opts,
			       size_t opts_len)
{
  return LAM_ERR_NOT_IMPLEMENTED;
}


int 
mca_pcm_cofs_job_launch_procs(lam_job_handle_t job_handle,
			      lam_pcm_node_t * nodes,
			      size_t nodes_len, const char *file,
			      int argc, const char *argv[],
			      const char *env[])
{
  return LAM_ERR_NOT_IMPLEMENTED;
}


int 
mca_pcm_cofs_job_rendezvous(lam_job_handle_t job_handle)
{
  return LAM_ERR_NOT_IMPLEMENTED;
}


int 
mca_pcm_cofs_job_wait(lam_job_handle_t job_handle)
{
  return LAM_ERR_NOT_IMPLEMENTED;
}


int 
mca_pcm_cofs_job_running(lam_job_handle_t job_handle,
			 int *running)
{
  return LAM_ERR_NOT_IMPLEMENTED;
}


int 
mca_pcm_cofs_job_list_running(lam_job_handle_t ** handles,
			      size_t handles_len)
{
  return LAM_ERR_NOT_IMPLEMENTED;
}


int 
mca_pcm_cofs_proc_startup(void)
{
  return LAM_ERR_NOT_IMPLEMENTED;
}


int 
mca_pcm_cofs_proc_get_peers(void)
{
  return LAM_ERR_NOT_IMPLEMENTED;
}


int 
mca_pcm_cofs_proc_get_me(void)
{
  return LAM_ERR_NOT_IMPLEMENTED;
}


int 
mca_pcm_cofs_proc_get_parent(void)
{
  return LAM_ERR_NOT_IMPLEMENTED;
}
