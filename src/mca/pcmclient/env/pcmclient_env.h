/* -*- C -*-
 * 
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */
#include "ompi_config.h"

#include "mca/pcmclient/pcmclient.h"
#include "include/types.h"

/*
 * Module open / close
 */
int mca_pcmclient_env_open(void);
int mca_pcmclient_env_close(void);

/*
 * Startup / Shutdown
 */
struct mca_pcmclient_base_module_1_0_0_t* mca_pcmclient_env_init(int *priority, bool *allow_multi_user_threads, bool *have_hidden_threads);
int mca_pcmclient_env_finalize(void);


/*
 * "Action" functions
 */
int mca_pcmclient_env_init_cleanup(void);
int mca_pcmclient_env_get_peers(ompi_process_name_t **peers, size_t *npeers);
ompi_process_name_t* mca_pcmclient_env_get_self(void);

