/* -*- C -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_PCMCLIENT_BASE_H_
#define MCA_PCMCLIENT_BASE_H_

#include "ompi_config.h"

#include "include/types.h"
#include "mca/mca.h"
#include "mca/pcmclient/pcmclient.h"


/*
 * Global functions for MCA overall collective open and close
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
OMPI_DECLSPEC    int mca_pcmclient_base_open(void);
OMPI_DECLSPEC    int mca_pcmclient_base_select(bool *allow_multi_user_threads, 
                                  bool *have_hidden_threads);
OMPI_DECLSPEC    int mca_pcmclient_base_close(void);


/*
 * Globals
 */
OMPI_DECLSPEC extern int mca_pcmclient_base_output;
OMPI_DECLSPEC extern ompi_list_t mca_pcmclient_base_components_available;
OMPI_DECLSPEC extern mca_pcmclient_base_component_t mca_pcmclient_base_selected_component;
OMPI_DECLSPEC extern mca_pcmclient_base_module_t mca_pcmclient;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* MCA_PCMCLIENT_BASE_H */
