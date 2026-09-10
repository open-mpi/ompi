/*
 * Copyright (c) 2026 Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 *
 * $HEADER$
 */

#ifndef PART_ONESIDED_REQUEST_H
#define PART_ONESIDED_REQUEST_H

#include "ompi/config.h"
#include "ompi/request/request.h"
#include "ompi/mca/part/part.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
#include "opal/sys/atomic.h"
#include "opal/mutex.h"
#include "opal/mca/btl/btl.h"

BEGIN_C_DECLS

typedef enum {
    MCA_PART_ONESIDED_REQUEST_PSEND,
    MCA_PART_ONESIDED_REQUEST_PRECV,
    MCA_PART_ONESIDED_REQUEST_NULL
} mca_part_onesided_request_type_t;

struct mca_part_onesided_request_t {
    ompi_request_t req_ompi;              /**< base request */
    volatile int32_t req_part_complete;   /**< flag indicating if the pt-2-pt layer is done */
    volatile int32_t req_free_called;     /**< flag indicating if the user has freed this request */
    mca_part_onesided_request_type_t req_type; /**< MPI request type */
    
    struct ompi_communicator_t *req_comm; /**< communicator pointer */
    struct ompi_datatype_t *req_datatype; /**< pointer to data type */
    
    void *req_buf;                        /**< application buffer */
    size_t req_parts;                     /**< number of partitions */
    size_t req_count;                     /**< count of user datatype elements */
    int32_t req_peer;                     /**< peer rank */
    int32_t req_tag;                      /**< user defined tag */
    
    size_t req_datatype_size;             /**< size of a single element */
    size_t req_total_bytes;               /**< total bytes for the whole request */
    size_t req_part_bytes;                /**< bytes per partition */

    /* BTL endpoint for this peer */
    struct mca_btl_base_endpoint_t *btl_endpoint;
    
    /* Registration handle for the receive buffer */
    struct mca_btl_base_registration_handle_t *reg_handle;

    /* Epoch state */
    opal_mutex_t lock;                    /**< protects epoch state */
    uint64_t epoch;                       /**< current epoch counter */
    uint8_t *ready_bitmap;                /**< bitmap to track ready partitions for current epoch */
    uint64_t req_id;                     /**< unique request ID for control messages */
    uint8_t *issued_bitmap;               /**< bitmap to track partitions issued to transport */
    uint8_t *completed_bitmap;            /**< bitmap to track partitions completed by transport */
    size_t bitmap_size;                   /**< size of the bitmaps in bytes */
    size_t completed_count;               /**< count of completed partitions */

    /* Matching and Epoch synchronization */
    uint64_t remote_req_id;               /**< ID of the matched peer request */
    void *remote_metadata;                /**< Metadata for remote receive buffer */
    size_t remote_metadata_size;
    
    bool epoch_open_sent;                 /**< Did we send EPOCH_OPEN for this epoch? */
    bool epoch_open_recv;                 /**< Did we receive EPOCH_OPEN for this epoch? */
};
typedef struct mca_part_onesided_request_t mca_part_onesided_request_t;

OBJ_CLASS_DECLARATION(mca_part_onesided_request_t);

#endif
END_C_DECLS
