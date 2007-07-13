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
 * Copyright (c) 2007      Cisco, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef ORTE_SDS_LSF_H
#define ORTE_SDS_LSF_H

BEGIN_C_DECLS

ORTE_MODULE_DECLSPEC extern orte_sds_base_component_t mca_sds_lsf_component;
    
/*
 * Module open / close
 */
int orte_sds_lsf_component_open(void);
int orte_sds_lsf_component_close(void);
orte_sds_base_module_t* orte_sds_lsf_component_init(int *priority);

/*
 * Startup / Shutdown
 */
int orte_sds_lsf_finalize(void);

/*
 * Module functions
 */
int orte_sds_lsf_set_name(void);

END_C_DECLS

#endif /* ORTE_SDS_LSF_H */
