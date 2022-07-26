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

#ifndef PART_PERSIST_REQUEST_H
#define PART_PERSIST_REQUEST_H

#include "ompi/mca/part/base/part_base_psendreq.h"
#include "ompi/mca/part/part.h"
#include "opal/sys/atomic.h"
/**
 * Type of request.
 */
typedef enum {
    MCA_PART_PERSIST_REQUEST_PSEND,
    MCA_PART_PERSIST_REQUEST_PRECV,
    MCA_PART_PERSIST_REQUEST_NULL
} mca_part_persist_request_type_t;

struct mca_part_persist_list_t;

struct ompi_mca_persist_setup_t {
   int world_rank;
   int start_tag;
   int setup_tag;
   size_t num_parts;
   size_t count;
};


/**
 *  Base type for PART PERSIST requests
 */
struct mca_part_persist_request_t {

/* START: These fields have to match the definition of the mca_part_persist_request_t */
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

/* END: These fields have to match the definition of the mca_part_persist_request_t */

    size_t  req_bytes;                    /**< bytes for completion status */

    size_t real_parts;                   /**< internal number of partitions */
    size_t real_count;
    size_t part_size; 

    ompi_request_t** persist_reqs;            /**< requests for persistent sends/recvs */
    ompi_request_t* setup_req [2];                /**< Request structure for setup messages */


    int32_t req_partitions_send;          /**< Send side number of partitions */
    int32_t req_partitions_recv;          /**< Recv side number of partitions */

    int32_t my_send_tag;                  /**< This is a counter for send tags for the actual data transfer. */
    int32_t my_recv_tag;                  /**< This is a counter for receive tags, for incoming setup messages. */ 

    int32_t world_peer;                   /**< peer's rank in MPI_COMM_WORLD */

    int32_t initialized;                  /**< flag for initialized state */
    int32_t first_send;                   /**< flag for whether the first send has happened */
    int32_t flag_post_setup_recv;  
    size_t done_count;             /**< counter for the number of partitions marked ready */

    int32_t *flags;               /**< array of flags to determine whether a partition has arrived */

    struct ompi_mca_persist_setup_t setup_info[2]; /**< Setup info to send during initialization. */
  
    struct mca_part_persist_list_t* progress_elem; /**< pointer to progress list element for removal during free. */ 

};
typedef struct mca_part_persist_request_t mca_part_persist_request_t;
OBJ_CLASS_DECLARATION(mca_part_persist_request_t);

#endif
