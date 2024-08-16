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
 * Copyright (c) 2020-2021 Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PART_DIRECT_REQUEST_H
#define PART_DIRECT_REQUEST_H

#include "ompi/mca/part/base/part_base_psendreq.h"
#include "ompi/mca/part/part.h"
#include "opal/sys/atomic.h"
/**
 * Type of request.
 */
typedef enum {
    MCA_PART_DIRECT_REQUEST_PSEND,
    MCA_PART_DIRECT_REQUEST_PRECV,
    MCA_PART_DIRECT_REQUEST_NULL
} mca_part_persist_request_type_t;

struct mca_part_direct_list_t;

struct ompi_mca_direct_setup_t {
   int world_rank;
   int start_tag;
   int setup_tag;
   size_t num_parts;
   size_t count;
};


/**
 *  Base type for PART DIRECT requests
 */
struct mca_part_direct_request_t {

/* START: These fields have to match the definition of the mca_part_direct_request_t */
    ompi_request_t req_ompi;              /**< base request */
    volatile int32_t req_part_complete;   /**< flag indicating if the pt-2-pt layer is done with this request */
    volatile int32_t req_free_called;     /**< flag indicating if the user has freed this request */
    mca_part_persist_request_type_t req_type; /**< MPI request type - used for test */
    struct ompi_communicator_t *req_comm; /**< communicator pointer */
    struct ompi_datatype_t *req_datatype; /**< pointer to data type */
    opal_convertor_t req_convertor;       /**< always need the convertor */

    const void *req_addr;                       /**< pointer to application buffer */
    size_t req_parts;                     /**< number of partitions */
    size_t req_count;                     /**< count of user datatype elements */
    int32_t req_peer;                     /**< peer process - rank w/in this communicator */
    int32_t req_tag;                      /**< user defined tag */
    struct ompi_proc_t* req_proc;         /**< peer process */

/* END: These fields have to match the definition of the mca_part_direct_request_t */

    size_t req_bytes;                    /**< bytes for completion status */

    size_t count;
    size_t parts;
    size_t part_size;
    ompi_datatype_t* datatype;

    int32_t round;                        /**< This is a simple counter pair to match for the flag window */ 
    int32_t tround;

    MPI_Comm comm;                        /**< To limit window create to two processes, we need a per request communicator. */
    MPI_Win window;                       /**< RMA Window for Data Transfer */
    MPI_Win window_flags;                 /**< RMA Window for completion flags. And RTS Flag. */

    int32_t req_partitions_send;          /**< Send side number of partitions */
    int32_t req_partitions_recv;          /**< Recv side number of partitions */

    int32_t world_peer;                   /**< peer's rank in MPI_COMM_WORLD */

    size_t done_count;             /**< counter for the number of partitions marked ready */

    int32_t *flags;               /**< array of flags to determine whether a partition has arrived */

    struct mca_part_direct_list_t* progress_elem; /**< pointer to progress list element for removal durring free. */ 

};
typedef struct mca_part_direct_request_t mca_part_direct_request_t;
OBJ_CLASS_DECLARATION(mca_part_direct_request_t);

#endif
