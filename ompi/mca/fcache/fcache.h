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
 * Copyright (c) 2008-2011 University of Houston. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_MCA_FCACHE_H
#define OMPI_MCA_FCACHE_H

#include "ompi_config.h"
#include "mpi.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

BEGIN_C_DECLS

struct mca_io_ompio_file_t;
struct mca_io_ompio_io_servers;
/*
 * Macro for use in components that are of type coll
 */
#define MCA_FCACHE_BASE_VERSION_2_0_0 \
    MCA_BASE_VERSION_2_0_0, \
        "fcache", 2, 0, 0

/*
 * These are the component function prototypes. These function pointers
 * go into the component structure. These functions (query() and finalize()
 * are called during fcache_base_select(). Each component is query() ied
 * and subsequently, all the unselected components are finalize() 'ed
 * so that any *stuff* they did during query() can be undone. By
 * similar logic, finalize() is also called on the component which
 * was selected when the communicator is being destroyed.
 *
 * So, to sum it up, every component carries 4 functions:
 * 1. open() - called during MPI_INIT
 * 2. close() - called during MPI_FINALIZE
 * 3. query() - called to select a particular component
 * 4. finalize() - called when actions taken during query have
 *                 to be undone
 */

/*
 * **************** component struct *******************************
 * *********** These functions go in the component struct **********
 * **************** component struct *******************************
 */

typedef int (*mca_fcache_base_component_init_query_1_0_0_fn_t) 
    (bool enable_progress_threads, 
     bool enable_mpi_threads);

typedef struct mca_fcache_base_module_1_0_0_t * 
(*mca_fcache_base_component_file_query_1_0_0_fn_t) (int *priority);

typedef int (*mca_fcache_base_component_file_unquery_1_0_0_fn_t)
    (struct mca_io_ompio_file_t *file);

/*
 * ****************** component struct ******************************
 * Structure for fcache v2.0.0 components.This is chained to MCA v2.0.0
 * ****************** component struct ******************************
 */
struct mca_fcache_base_component_2_0_0_t {
    mca_base_component_t fcachem_version;
    mca_base_component_data_t fcachem_data;
    
    mca_fcache_base_component_init_query_1_0_0_fn_t fcachem_init_query;
    mca_fcache_base_component_file_query_1_0_0_fn_t fcachem_file_query;
    mca_fcache_base_component_file_unquery_1_0_0_fn_t fcachem_file_unquery;
};
typedef struct mca_fcache_base_component_2_0_0_t mca_fcache_base_component_2_0_0_t;
typedef struct mca_fcache_base_component_2_0_0_t mca_fcache_base_component_t;

/*
 * ***********************************************************************
 * ************************  Interface function definitions **************
 * These are the typedefcache for the function pointers to various fcache
 * backend functions which will be used by the various fcache components
 * ***********************************************************************
 */

typedef int (*mca_fcache_base_module_init_1_0_0_fn_t)
(struct mca_io_ompio_file_t *file);

typedef int (*mca_fcache_base_module_finalize_1_0_0_fn_t)
(struct mca_io_ompio_file_t *file);

typedef int (*mca_fcache_base_module_get_file_layout_fn_t)(
    char* filename, 
    int *num_io_servers, 
    size_t *depth, 
    int *file_io_servers);

typedef int (*mca_fcache_base_module_set_file_layout_fn_t)(
    char* filename, 
    int *num_io_servers, 
    size_t *depth, 
    int *file_io_servers);

typedef int (*mca_fcache_base_module_get_io_servers_fn_t)(
    char* filename,
    struct mca_io_ompio_io_servers *io_servers,
    int num_io_servers);

/*
 * ***********************************************************************
 * ***************************  module structure *************************
 * ***********************************************************************
 */
struct mca_fcache_base_module_1_0_0_t {
    /*
     * Per-file initialization function. This is called only
     * on the module which is selected. The finalize corresponding to
     * this function is present on the component struct above
     */
    mca_fcache_base_module_init_1_0_0_fn_t fcache_module_init;
    mca_fcache_base_module_finalize_1_0_0_fn_t fcache_module_finalize;
    
    /* FCACHE function pointers */
    mca_fcache_base_module_get_file_layout_fn_t  fcache_get_file_layout;
    mca_fcache_base_module_set_file_layout_fn_t  fcache_set_file_layout;
    mca_fcache_base_module_get_io_servers_fn_t   fcache_get_io_servers;
};
typedef struct mca_fcache_base_module_1_0_0_t mca_fcache_base_module_1_0_0_t;
typedef mca_fcache_base_module_1_0_0_t mca_fcache_base_module_t;

END_C_DECLS

#endif /* OMPI_MCA_FCACHE_H */
