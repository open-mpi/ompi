/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PML_BASE_REQUEST_H
#define MCA_PML_BASE_REQUEST_H

#include "ompi/class/ompi_free_list.h"
#include "ompi/request/request.h"
#include "ompi/datatype/convertor.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * External list for the requests. They are declared as lists of
 * the basic request type, which will allow all PML to overload
 * the list. Beware these free lists have to be initialized
 * directly by the PML who win the PML election.
 */
OMPI_DECLSPEC extern ompi_free_list_t mca_pml_base_send_requests;
OMPI_DECLSPEC extern ompi_free_list_t mca_pml_base_recv_requests;

/**
 * Type of request.
 */
typedef enum {
    MCA_PML_REQUEST_NULL,
    MCA_PML_REQUEST_SEND,
    MCA_PML_REQUEST_RECV,
    MCA_PML_REQUEST_IPROBE,
    MCA_PML_REQUEST_PROBE
} mca_pml_base_request_type_t;


/**
 *  Base type for PML P2P requests 
 */
struct mca_pml_base_request_t {

/* START: These fields have to match the definition of the mca_pml_cm_request_t */
    ompi_request_t req_ompi;              /**< base request */
    volatile bool req_pml_complete;       /**< flag indicating if the pt-2-pt layer is done with this request */
    mca_pml_base_request_type_t req_type; /**< MPI request type - used for test */
    struct ompi_communicator_t *req_comm; /**< communicator pointer */
    struct ompi_datatype_t *req_datatype; /**< pointer to data type */
    volatile bool req_free_called;        /**< flag indicating if the user has freed this request */
    ompi_convertor_t req_convertor;       /**< always need the convertor */
/* END: These field have to match the definition of the mca_pml_cm_request_t */

    void *req_addr;                       /**< pointer to application buffer */
    size_t req_count;                     /**< count of user datatype elements */
    int32_t req_peer;                     /**< peer process - rank w/in this communicator */
    int32_t req_tag;                      /**< user defined tag */
    struct ompi_proc_t* req_proc;         /**< peer process */
    uint64_t req_sequence;                /**< sequence number for MPI pt-2-pt ordering */
};
typedef struct mca_pml_base_request_t mca_pml_base_request_t;

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_pml_base_request_t);

/*
 * Mark the MPI request as completed and trigger the condition if required.
 */
#define MCA_PML_BASE_REQUEST_MPI_COMPLETE( request )  \
do {                                                  \
   (request)->req_complete = true;                    \
   ompi_request_completed++;                          \
   if(ompi_request_waiting) {                         \
       opal_condition_broadcast(&ompi_request_cond);  \
   }                                                  \
} while(0)

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

