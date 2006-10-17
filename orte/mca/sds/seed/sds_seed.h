/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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

#ifndef ORTE_SDS_SEED_H
#define ORTE_SDS_SEED_H

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /*
     * Module open / close
     */
    int orte_sds_seed_component_open(void);
    int orte_sds_seed_component_close(void);
    orte_sds_base_module_t* orte_sds_seed_component_init(int *priority);

    /*
     * Startup / Shutdown
     */
    int orte_sds_seed_finalize(void);

    /*
     * Module functions
     */
    int orte_sds_seed_set_name(void);

    ORTE_MODULE_DECLSPEC extern orte_sds_base_component_t mca_sds_seed_component;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* ORTE_SDS_SEED_H */
