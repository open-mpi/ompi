/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      UT-Battelle, LLC
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef ORTE_SDS_ALPS_H
#define ORTE_SDS_ALPS_H

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /*
     * Module open / close
     */
    int orte_sds_alps_component_open(void);
    int orte_sds_alps_component_close(void);
    orte_sds_base_module_t* orte_sds_alps_component_init(int *priority);

    /*
     * Startup / Shutdown
     */
    int orte_sds_alps_finalize(void);

    /*
     * Module functions
     */
    int orte_sds_alps_set_name(void);
    int orte_sds_alps_contact_universe(void);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* ORTE_SDS_ALPS_H */
