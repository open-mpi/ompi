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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 *  @file 
 */

#ifndef MCA_PML_OB1_H
#define MCA_PML_OB1_H

#include "ompi_config.h"
#include "opal/threads/threads.h"
#include "opal/threads/condition.h"
#include "ompi/class/ompi_free_list.h"
#include "opal/util/cmd_line.h"
#include "ompi/request/request.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/pml/base/pml_base_request.h"
#include "ompi/mca/pml/base/pml_base_bsend.h"
#include "ompi/mca/pml/base/pml_base_sendreq.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/datatype/datatype.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
/**
 * OB1 PML module
 */

struct mca_pml_ob1_t {
    mca_pml_base_module_t super; 

    int priority;
    int free_list_num;      /* initial size of free list */
    int free_list_max;      /* maximum size of free list */
    int free_list_inc;      /* number of elements to grow free list */
    size_t eager_limit;     /* maximum eager limit size - overrides btl setting */
    size_t send_pipeline_depth;
    size_t recv_pipeline_depth;
    bool leave_pinned; 
    int leave_pinned_pipeline;
    
    /* lock queue access */
    opal_mutex_t lock;

    /* free lists */
    ompi_free_list_t send_requests;
    ompi_free_list_t recv_requests;
    ompi_free_list_t rdma_frags;
    ompi_free_list_t recv_frags;
    ompi_free_list_t buffers;

    /* list of pending operations */
    opal_list_t acks_pending;
    opal_list_t send_pending;
    opal_list_t recv_pending;
    opal_list_t rdma_pending;
    bool enabled; 
};
typedef struct mca_pml_ob1_t mca_pml_ob1_t; 

extern mca_pml_ob1_t mca_pml_ob1;


/*
 * PML module functions.
 */


extern int mca_pml_ob1_component_open(void);
extern int mca_pml_ob1_component_close(void);

extern mca_pml_base_module_t* mca_pml_ob1_component_init(
    int *priority, 
    bool enable_progress_threads,
    bool enable_mpi_threads
);

extern int mca_pml_ob1_component_fini(void);



/*
 * PML interface functions.
 */

extern int mca_pml_ob1_add_comm(
    struct ompi_communicator_t* comm
);

extern int mca_pml_ob1_del_comm(
    struct ompi_communicator_t* comm
);

extern int mca_pml_ob1_add_procs(
    struct ompi_proc_t **procs,
    size_t nprocs
);

extern int mca_pml_ob1_del_procs(
    struct ompi_proc_t **procs,
    size_t nprocs
);

extern int mca_pml_ob1_enable(
    bool enable
);

extern int mca_pml_ob1_progress(void);

extern int mca_pml_ob1_iprobe(
    int dst,
    int tag,
    struct ompi_communicator_t* comm,
    int *matched,
    ompi_status_public_t* status
);

extern int mca_pml_ob1_probe(
    int dst,
    int tag,
    struct ompi_communicator_t* comm,
    ompi_status_public_t* status
);

extern int mca_pml_ob1_isend_init(
    void *buf,
    size_t count,
    ompi_datatype_t *datatype,
    int dst,
    int tag,
    mca_pml_base_send_mode_t mode,
    struct ompi_communicator_t* comm,
    struct ompi_request_t **request
);

extern int mca_pml_ob1_isend(
    void *buf,
    size_t count,
    ompi_datatype_t *datatype,
    int dst,
    int tag,
    mca_pml_base_send_mode_t mode,
    struct ompi_communicator_t* comm,
    struct ompi_request_t **request
);

extern int mca_pml_ob1_send(
    void *buf,
    size_t count,
    ompi_datatype_t *datatype,
    int dst,
    int tag,
    mca_pml_base_send_mode_t mode,
    struct ompi_communicator_t* comm
);

extern int mca_pml_ob1_irecv_init(
    void *buf,
    size_t count,
    ompi_datatype_t *datatype,
    int src,
    int tag,
    struct ompi_communicator_t* comm,
    struct ompi_request_t **request
);

extern int mca_pml_ob1_irecv(
    void *buf,
    size_t count,
    ompi_datatype_t *datatype,
    int src,
    int tag,
    struct ompi_communicator_t* comm,
    struct ompi_request_t **request
);

extern int mca_pml_ob1_recv(
    void *buf,
    size_t count,
    ompi_datatype_t *datatype,
    int src,
    int tag,
    struct ompi_communicator_t* comm,
    ompi_status_public_t* status
);

extern int mca_pml_ob1_progress(void);

extern int mca_pml_ob1_start(
    size_t count,
    ompi_request_t** requests
);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#define MCA_PML_OB1_DES_ALLOC(bml_btl, des, size) \
MCA_BML_BASE_BTL_DES_ALLOC(bml_btl, des,  \
   sizeof(mca_pml_ob1_hdr_t) + (sizeof(mca_btl_base_segment_t) << 4), size)
                                                                                                                       

/**
 * structure to associate rdma btl with a registration
 */

struct mca_pml_ob1_rdma_reg_t {
    struct mca_bml_base_btl_t* bml_btl;
    struct mca_mpool_base_registration_t* btl_reg;
};
typedef struct mca_pml_ob1_rdma_reg_t mca_pml_ob1_rdma_reg_t;

#define MCA_PML_OB1_MAX_REGISTRATIONS 4

#endif

