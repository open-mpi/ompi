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
#ifndef ORTE_RDS_HOSTFILE_H
#define ORTE_RDS_HOSTFILE_H

#include "orte_config.h"
#include "threads/mutex.h"
#include "mca/rds/rds.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * Resource Discovery Component 
 */
struct orte_rds_hostfile_component_t {
    orte_rds_base_component_t super;
    int debug;
    char* path;
    ompi_mutex_t lock;
};
typedef struct orte_rds_hostfile_component_t orte_rds_hostfile_component_t;

OMPI_COMP_EXPORT extern orte_rds_hostfile_component_t mca_rds_hostfile_component;
OMPI_COMP_EXPORT extern orte_rds_base_module_t orte_rds_hostfile_module;


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
