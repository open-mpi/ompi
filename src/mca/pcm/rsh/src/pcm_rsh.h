/* -*- C -*-
 * 
 * $HEADER$
 *
 */
#include "ompi_config.h"

#include "mca/pcm/pcm.h"
#include "types.h"

#include <sys/types.h>

/*
 * Module open / close
 */
int mca_pcm_rsh_open(void);
int mca_pcm_rsh_close(void);

/*
 * Startup / Shutdown
 */
struct mca_pcm_1_0_0_t* mca_pcm_rsh_init(int *priority, 
                                          bool *allow_multi_user_threads,
                                          bool *have_hidden_threads);
int mca_pcm_rsh_finalize(void);


/*
 * "Action" functions
 */
int mca_pcm_rsh_query_get_nodes(mca_pcm_rte_node_t **nodes, size_t *nodes_len, 
                                            int *available_procs);

ompi_job_handle_t mca_pcm_rsh_handle_new(ompi_job_handle_t parent);
ompi_job_handle_t mca_pcm_rsh_handle_get(void);
void mca_pcm_rsh_handle_free(ompi_job_handle_t *job_handle);

int mca_pcm_rsh_job_can_spawn(ompi_job_handle_t job_handle);
int mca_pcm_rsh_job_set_arguments(ompi_job_handle_t job_handle, 
                                              mca_pcm_control_args_t* opts, 
                                              size_t opts_len);
int mca_pcm_rsh_job_launch_procs(ompi_job_handle_t job_handle, 
                                             mca_pcm_rte_node_t *nodes, 
                                             size_t nodes_len, const char* file, 
                                             int argc, const char* argv[], 
                                             const char *env[]);
int mca_pcm_rsh_job_rendezvous(ompi_job_handle_t job_handle);
int mca_pcm_rsh_job_wait(ompi_job_handle_t job_handle);
int mca_pcm_rsh_job_running(ompi_job_handle_t job_handle, 
                                        int* running);
int mca_pcm_rsh_job_list_running(ompi_job_handle_t **handles, 
                                             size_t handles_len);

int mca_pcm_rsh_proc_startup(void);
int mca_pcm_rsh_proc_get_peers(mca_pcm_proc_t **procs, size_t *nprocs);
mca_pcm_proc_t* mca_pcm_rsh_proc_get_me(void);
int mca_pcm_rsh_proc_get_parent(void);

/*
 * Private types / data / etc.
 */

struct mca_pcm_rsh_connection_item_t {
  ompi_list_item_t super;

  pid_t rshpid;
  int vpid;
  int status;
};
typedef struct mca_pcm_rsh_connection_t mca_pcm_rsh_connection_t;

extern ompi_list_t mca_pcm_rsh_connections;

extern ompi_job_handle_t mca_pcm_rsh_my_job_handle;
extern int mca_pcm_rsh_my_vpid;

extern char *mca_pcm_rsh_rsh;


/*
 * Private functions
 */
void mca_pcm_rsh_lock_connections(void);
void mca_pcm_rsh_unlock_connections(void);
