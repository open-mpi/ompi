/*
 * $HEADER$
 */
/** @file:
 */

#ifndef MCA_NS_BASE_H
#define MCA_NS_BASE_H

/*
 * includes
 */
#include "ompi_config.h"
#include "class/ompi_list.h"
#include "mca/mca.h"
#include "mca/ns/ns.h"


/*
 * Global functions for MCA overall collective open and close
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
    int mca_ns_base_open(void);
    int mca_ns_base_select(bool *allow_multi_user_threads,
			    bool *have_hidden_threads);
    int mca_ns_base_close(void);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


/*
 * globals that might be needed
 */

extern int mca_ns_base_output;
extern mca_ns_t ompi_name_server;  /* holds selected module's function pointers */
extern ompi_list_t mca_ns_base_modules_available;
extern mca_ns_base_module_t mca_ns_base_selected_module;

/*
 * external API functions will be documented in the mca/ns/ns.h file
 */

#endif
