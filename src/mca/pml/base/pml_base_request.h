/*
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PML_BASE_REQUEST_H
#define MCA_PML_BASE_REQUEST_H

#include "mem/free_list.h"
#include "request/request.h"
#include "datatype/datatype.h"
#include "communicator/communicator.h"
#include "mca/ptl/ptl.h"

extern ompi_class_t mca_pml_base_request_t_class;

/**
 * Type of request.
 */
typedef enum {
    MCA_PML_REQUEST_NULL,
    MCA_PML_REQUEST_SEND,
    MCA_PML_REQUEST_RECV
} mca_pml_base_request_type_t;


/**
 *  Base type for PML P2P requests 
 */
struct mca_pml_base_request_t {
    ompi_request_t super;                  /**< base request */
    void *req_addr;                       /**< pointer to application buffer */
    size_t req_count;                     /**< count of user datatype elements */
    int32_t req_peer;                     /**< peer process - rank w/in this communicator */
    int32_t req_tag;                      /**< user defined tag */
    ompi_communicator_t *req_comm;         /**< communicator pointer */
    ompi_proc_t* req_proc;                 /**< peer process */
    mca_ptl_sequence_t req_sequence;      /**< sequence number for MPI pt-2-pt ordering */
    ompi_datatype_t *req_datatype;         /**< pointer to data type */
    mca_pml_base_request_type_t req_type; /**< MPI request type - used for test */
    ompi_status_public_t req_status;       /**< completion status */
    bool req_persistent;                  /**< flag indicating if the this is a persistent request */
    volatile bool req_mpi_done;           /**< flag indicating if MPI is done with this request */
    volatile bool req_pml_done;           /**< flag indicating if the pt-2-pt layer is done with this request */
    volatile bool req_free_called;        /**< flag indicating if the user has freed this request */
};
typedef struct mca_pml_base_request_t mca_pml_base_request_t;


#endif

