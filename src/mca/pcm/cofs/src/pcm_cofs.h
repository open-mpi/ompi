/* -*- C -*-
 * 
 * $HEADER$
 *
 */
#include "ompi_config.h"

#include "mca/pcm/pcm.h"
#include "include/types.h"

/*
 * Module open / close
 */
int mca_pcm_cofs_open(void);
int mca_pcm_cofs_close(void);

/*
 * Startup / Shutdown
 */
struct mca_pcm_base_module_1_0_0_t* mca_pcm_cofs_init(int *priority, bool *allow_multi_user_threads, bool *have_hidden_threads);
int mca_pcm_cofs_finalize(void);


/*
 * "Action" functions
 */
int mca_pcm_cofs_get_peers(ompi_process_name_t **peers, size_t *npeers);
ompi_process_name_t* mca_pcm_cofs_get_self(void);

extern ompi_process_name_t *mca_pcm_cofs_procs;
extern size_t mca_pcm_cofs_num_procs;
extern size_t mca_pcm_cofs_procid;

