/* -*- C -*-
 *
 * $HEADER$
 * 
 */

#include "ompi_config.h"

#include "mca/pcm/pcm.h"
#include "mca/pcm/rsh/src/pcm_rsh.h"
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
  return OMPI_ERR_NOT_SUPPORTED;
}


ompi_job_handle_t 
mca_pcm_rsh_handle_new(ompi_job_handle_t parent)
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

  snprintf(new_handle, handle_len, "%s_%s_%d_%lu", handle_base, hostname, 
           mypid, handle_count++);
  return new_handle;
}


ompi_job_handle_t 
mca_pcm_rsh_handle_get(void)
{
  return mca_pcm_rsh_my_job_handle;
}


void 
mca_pcm_rsh_handle_free(ompi_job_handle_t * job_handle)
{
  if (*job_handle != NULL) {
    free(*job_handle);
    *job_handle = NULL;
  }
}


int 
mca_pcm_rsh_job_can_spawn(ompi_job_handle_t job_handle)
{
  if (mca_pcm_rsh_handle_get() != NULL) {
    /* no support for spawned jobs */
    return OMPI_ERR_NOT_SUPPORTED;
  } else {
    return OMPI_SUCCESS;
  }
}


int 
mca_pcm_rsh_job_set_arguments(ompi_job_handle_t job_handle,
			       mca_pcm_control_args_t * opts,
			       size_t opts_len)
{
  if (have_launched != 0) return OMPI_ERROR;


  return OMPI_ERR_NOT_IMPLEMENTED;
}


int 
mca_pcm_rsh_job_launch_procs(ompi_job_handle_t job_handle,
			      mca_pcm_rte_node_t *nodes,
			      size_t nodes_len, const char *file,
			      int argc, const char *argv[],
			      const char *env[])
{
  if (have_connected != 0) return OMPI_ERROR;

  have_launched = 1;
  return OMPI_ERR_NOT_IMPLEMENTED;
}


int 
mca_pcm_rsh_job_rendezvous(ompi_job_handle_t job_handle)
{
  if (have_connected != 0 || have_launched == 0) return OMPI_ERROR;

  return OMPI_ERR_NOT_IMPLEMENTED;
}


int 
mca_pcm_rsh_job_wait(ompi_job_handle_t job_handle)
{
  if (have_connected == 0 || have_launched == 0) return OMPI_ERROR;

  return OMPI_ERR_NOT_IMPLEMENTED;
}


int 
mca_pcm_rsh_job_running(ompi_job_handle_t job_handle,
			 int *running)
{
  return OMPI_ERR_NOT_IMPLEMENTED;
}


int 
mca_pcm_rsh_job_list_running(ompi_job_handle_t ** handles,
			      size_t handles_len)
{
  return OMPI_ERR_NOT_IMPLEMENTED;
}


int 
mca_pcm_rsh_proc_startup(void)
{
  if (have_connected != 0) return OMPI_ERROR;
  have_connected = 1;

  return OMPI_SUCCESS;
}


int 
mca_pcm_rsh_proc_get_peers(mca_pcm_proc_t **procs, size_t *nprocs)
{
  if (have_connected == 0) return OMPI_ERROR;

  return OMPI_SUCCESS;
}


mca_pcm_proc_t* 
mca_pcm_rsh_proc_get_me(void)
{
  return NULL;
}


int 
mca_pcm_rsh_proc_get_parent(void)
{
  return OMPI_ERR_NOT_IMPLEMENTED;
}
