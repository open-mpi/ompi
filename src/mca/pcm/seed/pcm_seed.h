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
int mca_pcm_seed_open(void);
int mca_pcm_seed_close(void);

/*
 * Startup / Shutdown
 */
struct mca_pcm_base_module_1_0_0_t* mca_pcm_seed_init(int *priority, bool *allow_multi_user_threads, bool *have_hidden_threads);
int mca_pcm_seed_finalize(void);


/*
 * "Action" functions
 */
char *mca_pcm_seed_get_unique_name(void);
bool mca_pcm_seed_can_spawn(void);
int mca_pcm_seed_get_peers(ompi_process_name_t **peers, size_t *npeers);
ompi_process_name_t* mca_pcm_seed_get_self(void);

