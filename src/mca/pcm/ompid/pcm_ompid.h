/*
 *
 * $HEADER$
 *
 */
#include "ompi_config.h"
#include "mca/pcm/pcm.h"
                                                                                                                 
/*
 * Module open / close
 */
int mca_pcm_ompid_open(void);
int mca_pcm_ompid_close(void);
                                                                                                                 
/*
 * Startup / Shutdown
 */
struct mca_pcm_base_module_1_0_0_t* mca_pcm_ompid_init(
    int *priority, 
    bool *allow_multi_user_threads, 
    bool *have_hidden_threads);

int mca_pcm_ompid_finalize(void);
                                                                                                                 
                                                                                                                 
/*
 * "Action" functions
 */

