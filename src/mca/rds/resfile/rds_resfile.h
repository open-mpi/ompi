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
/**
 * @file
 *
 * Resource Discovery (Hostfile)
 */
#ifndef ORTE_RDS_RESFILE_H
#define ORTE_RDS_RESFILE_H

#include "mca/rds/rds.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * RDS Resource file functions
 */
int orte_rds_resfile_query(void);

int orte_rds_resfile_finalize(void);


/**
 * RDS Resource file Component 
 */
struct orte_rds_resfile_component_t {
    orte_rds_base_component_t super;
    int debug;
    char *filename;
};
typedef struct orte_rds_resfile_component_t orte_rds_resfile_component_t;

OMPI_COMP_EXPORT extern orte_rds_resfile_component_t mca_rds_resfile_component;
OMPI_COMP_EXPORT extern orte_rds_base_module_t orte_rds_resfile_module;


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
