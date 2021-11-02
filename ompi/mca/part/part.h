/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2015 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2011-2020 Sandia National Laboratories. All rights reserved.
 * Copyright (c) 2021      Bull S.A.S. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 *
 * Partitioned Communication (PART)
 *
 * An MCA component type that provides the partitioned interface functionality
 * required by the MPI-4 specification. Part is designed to act as intermediary 
 * between the MPI layer and another transfer layer. This differs from other 
 * components, such as PML, by allowing the component to leverage the underlying 
 * transfer mechanism to be another MPI layer, such as the osc component/the 
 * RMA interface.
 *
 *   ------------------------------------
 *   |                MPI               |
 *   ------------------------------------
 *   |               PART               |
 *   ------------------------------------
 *   |             OSC (RDMA)           |
 *   ------------------------------------
 * 
 * The initial implementation is currently leveraging the RMA interface, 
 * with the intent to remove the MPI layer and directly call the osc component.
 * Other transport mechanisms could be used in future implementation (such as 
 * the MTL and BTL components). 
 *
 * This component and it's initial module are under development and have 
 * extra restrictions on use than described in the MPI-4 specification. 
 * Currently, MPI_Psend_init and MPI_Precv_init are both blocking in the RMA 
 * component which requires careful use to avoid deadlocks. This will 
 * be addressed in future updates. 
 */

#ifndef MCA_PART_H
#define MCA_PART_H

#include "ompi_config.h"
#include "ompi/mca/mca.h"
#include "mpi.h" /* needed for MPI_ANY_TAG */
#include "ompi/request/request.h"

BEGIN_C_DECLS

struct ompi_proc_t;

/**
 * MCA->PART Called by MCA framework to initialize the component.
 *
 * @param priority (OUT) Relative priority or ranking used by MCA to
 * selected a component.
 *
 * @param enable_progress_threads (IN) Whether this component is
 * allowed to run a hidden/progress thread or not.
 *
 * @param enable_mpi_threads (IN) Whether support for multiple MPI
 * threads is enabled or not (i.e., MPI_THREAD_MULTIPLE), which
 * indicates whether multiple threads may invoke this component
 * simultaneously or not.
 */
typedef struct mca_part_base_module_1_0_1_t * (*mca_part_base_component_init_fn_t)(
    int *priority,
    bool enable_progress_threads,
    bool enable_mpi_threads);

typedef int (*mca_part_base_component_finalize_fn_t)(void);

/**
 * PART component version and interface functions.
 */

struct mca_part_base_component_4_0_0_t {
   mca_base_component_t partm_version;
   mca_base_component_data_t partm_data;
   mca_part_base_component_init_fn_t partm_init;
   mca_part_base_component_finalize_fn_t partm_finalize;
};
typedef struct mca_part_base_component_4_0_0_t mca_part_base_component_4_0_0_t;
typedef mca_part_base_component_4_0_0_t mca_part_base_component_t;


/**
 * MCA management functions.
 */


/**
 * For non-threaded case, provides MCA the opportunity to
 * progress outstanding requests on all btls.
 *
 *  * @return        Count of "completions", a metric of
 *                   how many items where completed in the call
 *                   to progress.
*/
typedef int (*mca_part_base_module_progress_fn_t)(void);

/**
 * MPI Interface Functions
 */

/**
 *  Initialize a partitioned receive request.
 *
 *  @param buf (IN)         User buffer.
 *  @param parts (IN)       Number of partitions.
 *  @param count (IN)       Number of elements of the specified datatype.
 *  @param datatype (IN)    User defined datatype.
 *  @param src (IN)         Source rank w/in communicator.
 *  @param tag (IN)         User defined tag.
 *  @param comm (IN)        Communicator.
 *  @param request (OUT)    Request handle.
 *  @return                 OMPI_SUCCESS or failure status.
 */

typedef int (*mca_part_base_module_precv_init_fn_t)(
    void *buf,
    size_t parts,
    size_t count,
    struct ompi_datatype_t *datatype,
    int src,
    int tag,
    struct ompi_communicator_t* comm,
    struct ompi_info_t * info,
    struct ompi_request_t **request
);

/**
 *  Initialize a partitioned send request.
 *
 *  @param buf (IN)         User buffer.
 *  @param parts (IN)       Number of partitions.
 *  @param count (IN)       Number of elements of the specified datatype.
 *  @param datatype (IN)    User defined datatype.
 *  @param dst (IN)         Peer rank w/in communicator.
 *  @param tag (IN)         User defined tag.
 *  @param mode (IN)        Send mode (STANDARD,BUFFERED,SYNCHRONOUS,READY)
 *  @param comm (IN)        Communicator.
 *  @param request (OUT)    Request handle.
 *  @return                 OMPI_SUCCESS or failure status.
 */
typedef int (*mca_part_base_module_psend_init_fn_t)(
    const void *buf,
    size_t parts, 
    size_t count,
    struct ompi_datatype_t *datatype,
    int dst,
    int tag,
    struct ompi_communicator_t* comm,
    struct ompi_info_t * info,
    struct ompi_request_t **request
);

/**
 * Initiate one or more partitioned requests.
 *
 * @param count (IN)        Number of requests
 * @param requests (IN/OUT) Array of persistent requests
 * @return                  OMPI_SUCCESS or failure status.
 */
typedef ompi_request_start_fn_t mca_part_base_module_start_fn_t;

/**
 * Mark a range of partitions ready in a partitioned send request.
 *
 * @param min_part         Minimum partition to mark ready for transfer. 
 * @param max_part         Maximum partition to mark ready for transfer.
 * @param request (IN/OUT) Request
 * @return                 OMPI_SUCCESS or failure status.
 *
 */
typedef int (*mca_part_base_module_pready_fn_t)(
    size_t min_part,
    size_t max_part,
    struct ompi_request_t* request
);

/**
 * Check a range of partitions in a partitioned receive request.
 *
 * @param min_part         Minimum partition to check. 
 * @param max_part         Maximum partition to check.
 * @param flag             Flag for completion of entire range. 
 * @param request (IN/OUT) Request
 * @return                 OMPI_SUCCESS or failure status.
 *
 */
typedef int (*mca_part_base_module_parrived_fn_t)(
    size_t min_part,
    size_t max_part,
    int* flag,
    struct ompi_request_t* request
);

/**
 *  PART instance.
 */

struct mca_part_base_module_1_0_1_t {

    /* downcalls from MCA to PART */
    mca_part_base_module_progress_fn_t    part_progress;

    /* downcalls from MPI to PART */
    mca_part_base_module_precv_init_fn_t  part_precv_init;
    mca_part_base_module_psend_init_fn_t  part_psend_init;
    mca_part_base_module_start_fn_t       part_start; 
    mca_part_base_module_pready_fn_t      part_pready;
    mca_part_base_module_parrived_fn_t    part_parrived;
    /* diagnostics */

    /* FT Event */

    /* maximum constant sizes */
};
typedef struct mca_part_base_module_1_0_1_t mca_part_base_module_1_0_1_t;
typedef mca_part_base_module_1_0_1_t mca_part_base_module_t;

/*
 * Macro for use in components that are of type part
 */
#define MCA_PART_BASE_VERSION_2_0_0 \
    OMPI_MCA_BASE_VERSION_2_1_0("part", 4, 0, 0)

OMPI_DECLSPEC extern mca_part_base_module_t mca_part;

END_C_DECLS
#endif /* MCA_PART_H */
