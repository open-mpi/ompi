/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 */

#ifndef MCA_SDS_BASE_H
#define MCA_SDS_BASE_H

#include "orte_config.h"

#include "opal/mca/mca.h"
#include "orte/mca/sds/sds.h"
#include "orte/mca/ns/ns_types.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /*
     * Global functions for MCA overall collective open and close
     */

    /**
     * Open the sds framework
     */
    OMPI_DECLSPEC int orte_sds_base_open(void);

    /**
     * Select a sds module
     */
    OMPI_DECLSPEC int orte_sds_base_select(void);

    /**
     * Setup universe contact information
     */
    OMPI_DECLSPEC int orte_sds_base_contact_universe(void);

    /**
     * Setup self and peer naming
     */
    OMPI_DECLSPEC int orte_sds_base_set_name(void);

    /**
     * Close the sds framework
     */
    OMPI_DECLSPEC int orte_sds_base_close(void);

    /*
     * Internal helper functions used by components
     */
    OMPI_DECLSPEC int orte_sds_base_basic_contact_universe(void);
    OMPI_DECLSPEC int orte_sds_base_seed_set_name(void);

    /*
     * Put functions
     */
    OMPI_DECLSPEC int orte_ns_nds_env_put(const orte_process_name_t* proc, 
                                          orte_vpid_t vpid_start,
                                          size_t num_procs,
                                          char ***environ);
    OMPI_DECLSPEC int orte_ns_nds_pipe_put(const orte_process_name_t* proc, 
                                           orte_vpid_t vpid_start, 
                                           size_t num_procs, 
                                           int fd);
    OMPI_DECLSPEC int orte_ns_nds_bproc_put(orte_cellid_t cell, 
                                            orte_jobid_t job,
                                            orte_vpid_t vpid_start, 
                                            orte_vpid_t global_vpid_start,
                                            int num_procs, char ***env);

    extern opal_list_t orte_sds_base_components_available;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
