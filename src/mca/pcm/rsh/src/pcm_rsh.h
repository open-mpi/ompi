/* -*- C -*-
 * 
 * $HEADER$
 *
 */
#include "ompi_config.h"

#include "mca/pcm/pcm.h"
#include "include/types.h"

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

