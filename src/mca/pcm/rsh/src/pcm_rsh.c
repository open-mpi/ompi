/* -*- C -*-
 *
 * $HEADER$
 * 
 */

#include "lam_config.h"

#include "mca/pcm/pcm.h"
#include "mca/pcm/rsh/src/pcm_rsh.h"
#include "mem/malloc.h"
#include "types.h"

#include <stdio.h>
#include <sys/types.h>
#include <dirent.h>
#include <string.h>
#include <unistd.h>

static const char *handle_base = "pcm_rsh_job";
static unsigned long handle_count = 0;
static int have_connected = 0;
static int have_launched = 0;

int 
mca_pcm_rsh_query_get_nodes(mca_pcm_rte_node_t **nodes, size_t * nodes_len,
			     int *available_procs)
{
  return LAM_ERR_NOT_SUPPORTED;
}


lam_job_handle_t 
mca_pcm_rsh_handle_new(lam_job_handle_t parent)
{
  char *new_handle = NULL;
  int mypid = 0;
  char hostname[MAXHOSTNAMELEN];
  size_t handle_len = strlen(handle_base) + strlen("___") + 
    (8 * sizeof(int) * 2) + MAXHOSTNAMELEN;

  /* we don't support spawning just yet */
  if (parent != NULL) return NULL;

  gethostname(hostname, MAXHOSTNAMELEN);

  new_handle = (char*) malloc(handle_len);
  if (new_handle == NULL) return new_handle;

  snprintf(new_handle, handle_len, "%s_%s_%d_%d", handle_base, hostname, 
           mypid, handle_count++);
  return new_handle;
}


lam_job_handle_t 
mca_pcm_rsh_handle_get(void)
{
  return mca_pcm_rsh_my_job_handle;
}


void 
mca_pcm_rsh_handle_free(lam_job_handle_t * job_handle)
{
  if (*job_handle != NULL) {
    free(*job_handle);
    *job_handle = NULL;
  }
}


int 
mca_pcm_rsh_job_can_spawn(lam_job_handle_t job_handle)
{
  if (mca_pcm_rsh_handle_get() != NULL) {
    /* no support for spawned jobs */
    return LAM_ERR_NOT_SUPPORTED;
  } else {
    return LAM_SUCCESS;
  }
}


int 
mca_pcm_rsh_job_set_arguments(lam_job_handle_t job_handle,
			       mca_pcm_control_args_t * opts,
			       size_t opts_len)
{
  if (have_launched != 0) return LAM_ERROR;


  return LAM_ERR_NOT_IMPLEMENTED;
}


int 
mca_pcm_rsh_job_launch_procs(lam_job_handle_t job_handle,
			      mca_pcm_rte_node_t *nodes,
			      size_t nodes_len, const char *file,
			      int argc, const char *argv[],
			      const char *env[])
{
  if (have_connected != 0) return LAM_ERROR;

  have_launched = 1;
  return LAM_ERR_NOT_IMPLEMENTED;
}


int 
mca_pcm_rsh_job_rendezvous(lam_job_handle_t job_handle)
{
  if (have_connected != 0 || have_launched == 0) return LAM_ERROR;

  return LAM_ERR_NOT_IMPLEMENTED;
}


int 
mca_pcm_rsh_job_wait(lam_job_handle_t job_handle)
{
  if (have_connected == 0 || have_launched == 0) return LAM_ERROR;

  return LAM_ERR_NOT_IMPLEMENTED;
}


int 
mca_pcm_rsh_job_running(lam_job_handle_t job_handle,
			 int *running)
{
  return LAM_ERR_NOT_IMPLEMENTED;
}


int 
mca_pcm_rsh_job_list_running(lam_job_handle_t ** handles,
			      size_t handles_len)
{
  return LAM_ERR_NOT_IMPLEMENTED;
}


int 
mca_pcm_rsh_proc_startup(void)
{
  if (have_connected != 0) return LAM_ERROR;
  have_connected = 1;

  return LAM_SUCCESS;
}


int 
mca_pcm_rsh_proc_get_peers(mca_pcm_proc_t **procs, size_t *nprocs)
{
  if (have_connected == 0) return LAM_ERROR;

  return LAM_SUCCESS;
}


mca_pcm_proc_t* 
mca_pcm_rsh_proc_get_me(void)
{
  return NULL;
}


int 
mca_pcm_rsh_proc_get_parent(void)
{
  return LAM_ERR_NOT_IMPLEMENTED;
}
