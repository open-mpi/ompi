/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2016 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2017      Intel, Inc. All rights reserved
 * Copyright (c) 2020      Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PART_RMA_REQUEST_H
#define PART_RMA_REQUEST_H

#include "ompi/mca/part/base/part_base_psendreq.h"
#include "ompi/mca/part/part.h"
#include "opal/sys/atomic.h"
/**
 * Type of request.
 */
typedef enum {
    MCA_PART_RMA_REQUEST_PSEND,
    MCA_PART_RMA_REQUEST_PRECV,
    MCA_PART_RMA_REQUEST_NULL
} mca_part_rma_request_type_t;

struct mca_part_rma_list_t;

/**
 *  Base type for PART RMA requests
 */
struct mca_part_rma_request_t {

/* START: These fields have to match the definition of the mca_part_rma_request_t */
    ompi_request_t req_ompi;              /**< base request */
    volatile int32_t req_part_complete;   /**< flag indicating if the pt-2-pt layer is done with this request */
    volatile int32_t req_free_called;     /**< flag indicating if the user has freed this request */
    mca_part_rma_request_type_t req_type; /**< MPI request type - used for test */
    struct ompi_communicator_t *req_comm; /**< communicator pointer */
    struct ompi_datatype_t *req_datatype; /**< pointer to data type */
    opal_convertor_t req_convertor;       /**< always need the convertor */

    const void *req_addr;                       /**< pointer to application buffer */
    size_t req_parts;                     /**< number of partitions */
    size_t req_count;                     /**< count of user datatype elements */
    int32_t req_peer;                     /**< peer process - rank w/in this communicator */
    int32_t req_tag;                      /**< user defined tag */
    struct ompi_proc_t* req_proc;         /**< peer process */

/* END: These fields have to match the definition of the mca_part_rma_request_t */

    size_t  req_bytes;                    /**< bytes for completion status */

    MPI_Comm req_window_comm;             /**< New communicator for the window */
    MPI_Win req_data_window;              /**< RMA window for data transfer */
    MPI_Win req_flags_window;             /**< RMA window for completion */

    int32_t req_partitions_send;          /**< Send side number of partitions */
    int32_t req_sent;                     /**< TODO - Does this do anything? */
    int32_t req_partitions_recv;          /**< Recv side number of partitions */

    int32_t req_flags_size;
    int32_t req_counter_thresh;           /**< threshold for counters*/ 
    volatile int32_t *req_counters; /**< counters for local partitions (should be size of 'req_flags_size') */
    int32_t *req_flags; /**< completion flags to transfer (should be size of 'req_flags_size') */

    struct mca_part_rma_list_t* progress_elem; /**< pointer to progress list element for removal durring free. */ 

};
typedef struct mca_part_rma_request_t mca_part_rma_request_t;
OBJ_CLASS_DECLARATION(mca_part_rma_request_t);

#endif
