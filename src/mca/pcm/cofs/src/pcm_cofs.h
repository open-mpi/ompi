/* -*- C -*-
 * 
 * $HEADER$
 *
 */
#include "ompi_config.h"

#include "mca/pcm/pcm.h"
#include "types.h"

/*
 * Module open / close
 */
int mca_pcm_cofs_open(void);
int mca_pcm_cofs_close(void);

/*
 * Startup / Shutdown
 */
struct mca_pcm_1_0_0_t* mca_pcm_cofs_init(int *priority, 
                                          bool *allow_multi_user_threads,
                                          bool *have_hidden_threads);
int mca_pcm_cofs_finalize(void);


/*
 * "Action" functions
 */
int mca_pcm_cofs_query_get_nodes(mca_pcm_rte_node_t **nodes, size_t *nodes_len, 
                                            int *available_procs);

ompi_job_handle_t mca_pcm_cofs_handle_new(ompi_job_handle_t parent);
ompi_job_handle_t mca_pcm_cofs_handle_get(void);
void mca_pcm_cofs_handle_free(ompi_job_handle_t *job_handle);

int mca_pcm_cofs_job_can_spawn(ompi_job_handle_t job_handle);
int mca_pcm_cofs_job_set_arguments(ompi_job_handle_t job_handle, 
                                              mca_pcm_control_args_t* opts, 
                                              size_t opts_len);
int mca_pcm_cofs_job_launch_procs(ompi_job_handle_t job_handle, 
                                             mca_pcm_rte_node_t *nodes, 
                                             size_t nodes_len, const char* file, 
                                             int argc, const char* argv[], 
                                             const char *env[]);
int mca_pcm_cofs_job_rendezvous(ompi_job_handle_t job_handle);
int mca_pcm_cofs_job_wait(ompi_job_handle_t job_handle);
int mca_pcm_cofs_job_running(ompi_job_handle_t job_handle, 
                                        int* running);
int mca_pcm_cofs_job_list_running(ompi_job_handle_t **handles, 
                                             size_t handles_len);

int mca_pcm_cofs_proc_startup(void);
int mca_pcm_cofs_proc_get_peers(mca_pcm_proc_t **procs, size_t *nprocs);
mca_pcm_proc_t* mca_pcm_cofs_proc_get_me(void);
int mca_pcm_cofs_proc_get_parent(void);

extern char mca_pcm_cofs_comm_loc[OMPI_PATH_MAX]; /* location for file drop-off */

extern int mca_pcm_cofs_my_vpid;
extern char *mca_pcm_cofs_my_handle;

extern mca_pcm_proc_t *mca_pcm_cofs_procs;
extern size_t mca_pcm_cofs_nprocs;

