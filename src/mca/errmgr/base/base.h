/*
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
/** @file:
 */

#ifndef MCA_ERRMGR_BASE_H
#define MCA_ERRMGR_BASE_H

/*
 * includes
 */
#include "ompi_config.h"
#include "include/constants.h"

#include "class/ompi_list.h"

#include "mca/mca.h"
#include "mca/errmgr/errmgr.h"


/*
 * Global functions for MCA overall collective open and close
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Internal definitions
 */
/*
 * function definitions
 */
OMPI_DECLSPEC    int mca_errmgr_base_open(void);
OMPI_DECLSPEC    int mca_errmgr_base_select(bool *allow_multi_user_threads,
			                                bool *have_hidden_threads);
OMPI_DECLSPEC    int mca_errmgr_base_close(void);

    /*
     * Base functions that are common to all implementations - can be overridden
     */

OMPI_DECLSPEC    void mca_errmgr_base_log(char *msg, char *filename, int line);

/*
 * globals that might be needed
 */

OMPI_DECLSPEC extern int mca_errmgr_base_output;
OMPI_DECLSPEC extern bool mca_errmgr_base_selected;
OMPI_DECLSPEC extern ompi_list_t mca_errmgr_base_components_available;
OMPI_DECLSPEC extern mca_errmgr_base_component_t mca_errmgr_base_selected_component;

/*
 * external API functions will be documented in the mca/errmgr/errmgr.h file
 */

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
