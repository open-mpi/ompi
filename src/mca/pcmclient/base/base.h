/* -*- C -*-
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
    int mca_pcmclient_base_open(void);
    int mca_pcmclient_base_select(bool *allow_multi_user_threads, 
                                  bool *have_hidden_threads);
    int mca_pcmclient_base_close(void);


/*
 * Globals
 */
extern int mca_pcmclient_base_output;
extern ompi_list_t mca_pcmclient_base_components_available;
extern mca_pcmclient_base_component_t mca_pcmclient_base_selected_component;
extern mca_pcmclient_base_module_t mca_pcmclient;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* MCA_PCMCLIENT_BASE_H */
