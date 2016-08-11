/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2013      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 */

#include "ompi_config.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif
#include <fcntl.h>
#include <errno.h>

#include "coll_hcoll.h"

#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/datatype/ompi_datatype_internal.h"
#include "ompi/mca/pml/pml.h"


#include "hcoll/api/hcoll_dte.h"
#include "hcoll/api/hcoll_api.h"
#include "hcoll/api/hcoll_constants.h"
#include "coll_hcoll_dtypes.h"
/*
 * Local functions
 */


static int recv_nb(dte_data_representation_t data ,
                   uint32_t count ,
                   void *buffer,
                   rte_ec_handle_t ,
                   rte_grp_handle_t ,
                   uint32_t tag,
                   rte_request_handle_t * req);

static int send_nb(dte_data_representation_t data,
                   uint32_t count,
                   void *buffer,
                   rte_ec_handle_t ec_h,
                   rte_grp_handle_t grp_h,
                   uint32_t tag, rte_request_handle_t *req);

static int test( rte_request_handle_t * request ,
                 int * completed );

static int ec_handle_compare( rte_ec_handle_t handle_1 ,
                              rte_grp_handle_t
                              group_handle_1 ,
                              rte_ec_handle_t handle_2 ,
                              rte_grp_handle_t
                              group_handle_2 );

static int get_ec_handles( int num_ec ,
                           int * ec_indexes ,
                           rte_grp_handle_t ,
                           rte_ec_handle_t * ec_handles );

static int get_my_ec(rte_grp_handle_t , rte_ec_handle_t *ec_handle);

static int group_size ( rte_grp_handle_t group );
static int my_rank (rte_grp_handle_t grp_h);
static int ec_on_local_node (rte_ec_handle_t ec, rte_grp_handle_t group);
static rte_grp_handle_t get_world_group_handle(void);
static uint32_t jobid(void);

static void progress(void){
    opal_progress();
}

static void* get_coll_handle(void);
static int coll_handle_test(void* handle);
static void coll_handle_free(void *handle);
static void coll_handle_complete(void *handle);
static int group_id(rte_grp_handle_t group);

static int world_rank(rte_grp_handle_t grp_h, rte_ec_handle_t ec);
/* Module Constructors */
#if HCOLL_API >= HCOLL_VERSION(3,6)
static int get_mpi_type_envelope(void *mpi_type, int *num_integers,
                                 int *num_addresses, int *num_datatypes,
                                 hcoll_mpi_type_combiner_t *combiner);
static int get_mpi_type_contents(void *mpi_type, int max_integers, int max_addresses,
                                 int max_datatypes, int *array_of_integers,
                                 void *array_of_addresses, void *array_of_datatypes);
static int get_hcoll_type(void *mpi_type, dte_data_representation_t *hcoll_type);
static int set_hcoll_type(void *mpi_type, dte_data_representation_t hcoll_type);
static int get_mpi_constants(size_t *mpi_datatype_size,
                             int *mpi_order_c, int *mpi_order_fortran,
                             int *mpi_distribute_block,
                             int *mpi_distribute_cyclic,
                             int *mpi_distribute_none,
                             int *mpi_distribute_dflt_darg);
#endif

static void init_module_fns(void){
    hcoll_rte_functions.send_fn = send_nb;
    hcoll_rte_functions.recv_fn = recv_nb;
    hcoll_rte_functions.ec_cmp_fn = ec_handle_compare;
    hcoll_rte_functions.get_ec_handles_fn = get_ec_handles;
    hcoll_rte_functions.rte_group_size_fn = group_size;
    hcoll_rte_functions.test_fn = test;
    hcoll_rte_functions.rte_my_rank_fn = my_rank;
    hcoll_rte_functions.rte_ec_on_local_node_fn = ec_on_local_node;
    hcoll_rte_functions.rte_world_group_fn = get_world_group_handle;
    hcoll_rte_functions.rte_jobid_fn = jobid;
    hcoll_rte_functions.rte_progress_fn = progress;
    hcoll_rte_functions.rte_get_coll_handle_fn = get_coll_handle;
    hcoll_rte_functions.rte_coll_handle_test_fn = coll_handle_test;
    hcoll_rte_functions.rte_coll_handle_free_fn = coll_handle_free;
    hcoll_rte_functions.rte_coll_handle_complete_fn = coll_handle_complete;
    hcoll_rte_functions.rte_group_id_fn = group_id;
    hcoll_rte_functions.rte_world_rank_fn = world_rank;
#if HCOLL_API >= HCOLL_VERSION(3,6)
    hcoll_rte_functions.rte_get_mpi_type_envelope_fn = get_mpi_type_envelope;
    hcoll_rte_functions.rte_get_mpi_type_contents_fn = get_mpi_type_contents;
    hcoll_rte_functions.rte_get_hcoll_type_fn = get_hcoll_type;
    hcoll_rte_functions.rte_set_hcoll_type_fn = set_hcoll_type;
    hcoll_rte_functions.rte_get_mpi_constants_fn = get_mpi_constants;
#endif
}


void hcoll_rte_fns_setup(void)
{
    init_module_fns();
    OBJ_CONSTRUCT(&mca_coll_hcoll_component.requests, opal_free_list_t);
    opal_free_list_init(
                &(mca_coll_hcoll_component.requests),
                sizeof(ompi_request_t),
                /* no special alignment needed */
                8,
                OBJ_CLASS(ompi_request_t),
                /* no payload data */
                0, 0,
                /* NOTE: hack - need to parametrize this */
                10,
                -1,
                10,
                /* No Mpool or init function */
                NULL,
                0,
                NULL,
                NULL,
                NULL
                );
}

static int recv_nb(struct dte_data_representation_t data,
                   uint32_t count ,
                   void *buffer,
                   rte_ec_handle_t ec_h,
                   rte_grp_handle_t grp_h,
                   uint32_t tag,
                   rte_request_handle_t *req)
{
    ompi_communicator_t *comm = (ompi_communicator_t *)grp_h;

    if (NULL == ec_h.handle && -1 != ec_h.rank) {
        fprintf(stderr,"***Error in hcolrte_rml_recv_nb: wrong null argument: "
                "ec_h.handle = %p, ec_h.rank = %d\n",ec_h.handle,ec_h.rank);
        return 1;
    }
    assert(HCOL_DTE_IS_INLINE(data));
    /*do inline nb recv*/
    size_t size;
    ompi_request_t *ompi_req;

    if (!buffer && !HCOL_DTE_IS_ZERO(data)) {
        fprintf(stderr, "***Error in hcolrte_rml_recv_nb: buffer pointer is NULL"
                " for non DTE_ZERO INLINE data representation\n");
        return 1;
    }
    size = (size_t)data.rep.in_line_rep.data_handle.in_line.packed_size*count/8;

    HCOL_VERBOSE(30,"PML_IRECV: dest = %d: buf = %p: size = %u: comm = %p",
                 ec_h.rank, buffer, (unsigned int)size, (void *)comm);
    if (MCA_PML_CALL(irecv(buffer,size,&(ompi_mpi_unsigned_char.dt),ec_h.rank,
                           tag,comm,&ompi_req)))
    {
        return 1;
    }
    req->data = (void *)ompi_req;
    req->status = HCOLRTE_REQUEST_ACTIVE;

    return HCOLL_SUCCESS;
}


static int send_nb( dte_data_representation_t data,
                    uint32_t count,
                    void *buffer,
                    rte_ec_handle_t ec_h,
                    rte_grp_handle_t grp_h,
                    uint32_t tag,
                    rte_request_handle_t *req)
{
    ompi_communicator_t *comm = (ompi_communicator_t *)grp_h;

    if (! ec_h.handle) {
        fprintf(stderr,"***Error in hcolrte_rml_send_nb: wrong null argument: "
                "ec_h.handle = %p, ec_h.rank = %d\n",ec_h.handle,ec_h.rank);
        return 1;
    }
    assert(HCOL_DTE_IS_INLINE(data));
    /*do inline nb recv*/
    size_t size;
    ompi_request_t *ompi_req;
    if (!buffer && !HCOL_DTE_IS_ZERO(data)) {
        fprintf(stderr, "***Error in hcolrte_rml_send_nb: buffer pointer is NULL"
                " for non DTE_ZERO INLINE data representation\n");
        return 1;
    }
    size = (size_t)data.rep.in_line_rep.data_handle.in_line.packed_size*count/8;
    HCOL_VERBOSE(30,"PML_ISEND: dest = %d: buf = %p: size = %u: comm = %p",
                 ec_h.rank, buffer, (unsigned int)size, (void *)comm);
    if (MCA_PML_CALL(isend(buffer,size,&(ompi_mpi_unsigned_char.dt),ec_h.rank,
                           tag,MCA_PML_BASE_SEND_STANDARD,comm,&ompi_req)))
    {
        return 1;
    }
    req->data = (void *)ompi_req;
    req->status = HCOLRTE_REQUEST_ACTIVE;
    return HCOLL_SUCCESS;
}

static int test( rte_request_handle_t * request ,
                 int * completed )
{
    ompi_request_t * ompi_req  = (ompi_request_t *)request->data;
    if (HCOLRTE_REQUEST_ACTIVE != request->status){
        *completed = true;
        return HCOLL_SUCCESS;
    }

    /*ompi_request_test(&ompi_req,completed,MPI_STATUS_IGNORE); */
    *completed = REQUEST_COMPLETE(ompi_req);
    if (*completed){
        ompi_request_free(&ompi_req);
        request->status = HCOLRTE_REQUEST_DONE;
    }

    return HCOLL_SUCCESS;
}

static int ec_handle_compare( rte_ec_handle_t handle_1 ,
                              rte_grp_handle_t
                              group_handle_1 ,
                              rte_ec_handle_t handle_2 ,
                              rte_grp_handle_t
                              group_handle_2 )
{
    return handle_1.handle == handle_2.handle;
}

static int get_ec_handles( int num_ec ,
                           int * ec_indexes ,
                           rte_grp_handle_t grp_h,
                           rte_ec_handle_t * ec_handles )
{
    int i;
    ompi_communicator_t *comm = (ompi_communicator_t *)grp_h;
    for (i=0; i<num_ec; i++){
        ompi_proc_t *proc = ompi_comm_peer_lookup(comm,ec_indexes[i]);
        ec_handles[i].rank = ec_indexes[i];
        ec_handles[i].handle = (void *)proc;
    }
    return HCOLL_SUCCESS;
}

static int get_my_ec ( rte_grp_handle_t grp_h, rte_ec_handle_t *ec_handle)
{
    ompi_communicator_t *comm = (ompi_communicator_t *)grp_h;
    int my_rank = ompi_comm_rank(comm);
    ompi_proc_t *my_proc = ompi_comm_peer_lookup(comm,my_rank);
    ec_handle->handle = (void *)my_proc;
    ec_handle->rank = my_rank;
    return HCOLL_SUCCESS;
}


static int group_size ( rte_grp_handle_t grp_h )
{
    return ompi_comm_size((ompi_communicator_t *)grp_h);
}

static int my_rank (rte_grp_handle_t grp_h )
{
    return ompi_comm_rank((ompi_communicator_t *)grp_h);
}

static int ec_on_local_node (rte_ec_handle_t ec, rte_grp_handle_t group){
    ompi_proc_t *proc = (ompi_proc_t *)ec.handle;
    return OPAL_PROC_ON_LOCAL_NODE(proc->super.proc_flags);
}


static rte_grp_handle_t get_world_group_handle(void)
{
    return (rte_grp_handle_t)&ompi_mpi_comm_world.comm;
}

static uint32_t jobid(void){
    return ORTE_PROC_MY_NAME->jobid;
}

static int group_id(rte_grp_handle_t group){
    return ((ompi_communicator_t *)group)->c_contextid;
}

static int
request_free(struct ompi_request_t **ompi_req)
{
    ompi_request_t *req = *ompi_req;
    if (!coll_handle_test(req)) {
        return OMPI_ERROR;
    }
    coll_handle_free(req);
    *ompi_req = &ompi_request_empty;
    return OMPI_SUCCESS;
}

static void* get_coll_handle(void)
{
    ompi_request_t *ompi_req;
    opal_free_list_item_t *item;
    item = opal_free_list_wait (&(mca_coll_hcoll_component.requests));
    if (OPAL_UNLIKELY(NULL == item)) {
        HCOL_ERROR("Wait for free list failed.\n");
        return NULL;
    }
    ompi_req = (ompi_request_t *)item;
    OMPI_REQUEST_INIT(ompi_req,false);
    ompi_req->req_complete_cb = NULL;
    ompi_req->req_status.MPI_ERROR = MPI_SUCCESS;
    ompi_req->req_state = OMPI_REQUEST_ACTIVE;
    ompi_req->req_free = request_free;
    ompi_req->req_type = OMPI_REQUEST_COLL;
    return (void *)ompi_req;
}

static int coll_handle_test(void* handle)
{
    ompi_request_t *ompi_req = (ompi_request_t *)handle;
    return REQUEST_COMPLETE(ompi_req);;
}

static void coll_handle_free(void *handle){
    ompi_request_t *ompi_req = (ompi_request_t *)handle;
    opal_free_list_return (&mca_coll_hcoll_component.requests,
                           (opal_free_list_item_t *)ompi_req);
}

static void coll_handle_complete(void *handle)
{
    ompi_request_t *ompi_req = (ompi_request_t *)handle;
    ompi_request_complete(ompi_req,true);
}


static int world_rank(rte_grp_handle_t grp_h, rte_ec_handle_t ec){
    ompi_proc_t *proc = (ompi_proc_t *)ec.handle;
    return ((ompi_process_name_t*)&proc->super.proc_name)->vpid;
}

#if HCOLL_API >= HCOLL_VERSION(3,6)
hcoll_mpi_type_combiner_t ompi_combiner_2_hcoll_combiner(int ompi_combiner) {
    switch (ompi_combiner)
    {
    case MPI_COMBINER_CONTIGUOUS:
        return HCOLL_MPI_COMBINER_CONTIGUOUS;
    case MPI_COMBINER_VECTOR:
        return HCOLL_MPI_COMBINER_VECTOR;
    case MPI_COMBINER_HVECTOR:
        return HCOLL_MPI_COMBINER_HVECTOR;
    case MPI_COMBINER_INDEXED:
        return HCOLL_MPI_COMBINER_INDEXED;
    case MPI_COMBINER_HINDEXED_INTEGER:
    case MPI_COMBINER_HINDEXED:
        return HCOLL_MPI_COMBINER_HINDEXED;
    case MPI_COMBINER_DUP:
        return HCOLL_MPI_COMBINER_DUP;
    case MPI_COMBINER_INDEXED_BLOCK:
        return HCOLL_MPI_COMBINER_INDEXED_BLOCK;
    case MPI_COMBINER_HINDEXED_BLOCK:
        return HCOLL_MPI_COMBINER_HINDEXED_BLOCK;
    case MPI_COMBINER_SUBARRAY:
        return HCOLL_MPI_COMBINER_SUBARRAY;
    case MPI_COMBINER_DARRAY:
        return HCOLL_MPI_COMBINER_DARRAY;
    case MPI_COMBINER_F90_REAL:
        return HCOLL_MPI_COMBINER_F90_REAL;
    case MPI_COMBINER_F90_COMPLEX:
        return HCOLL_MPI_COMBINER_F90_COMPLEX;
    case MPI_COMBINER_F90_INTEGER:
        return HCOLL_MPI_COMBINER_F90_INTEGER;
    case MPI_COMBINER_RESIZED:
        return HCOLL_MPI_COMBINER_RESIZED;
    case MPI_COMBINER_STRUCT:
    case MPI_COMBINER_STRUCT_INTEGER:
        return HCOLL_MPI_COMBINER_STRUCT;
    default:
        break;
    }
    return HCOLL_MPI_COMBINER_LAST;
}


static int get_mpi_type_envelope(void *mpi_type, int *num_integers,
                                 int *num_addresses, int *num_datatypes,
                                 hcoll_mpi_type_combiner_t *combiner) {
    int ompi_combiner, rc;
    rc = ompi_datatype_get_args( (ompi_datatype_t*)mpi_type, 0, num_integers, NULL,
                                 num_addresses, NULL,
                                 num_datatypes, NULL, &ompi_combiner);
    *combiner = ompi_combiner_2_hcoll_combiner(ompi_combiner);
    return rc == OMPI_SUCCESS ? HCOLL_SUCCESS : HCOLL_ERROR;
}

static int get_mpi_type_contents(void *mpi_type, int max_integers, int max_addresses,
                                 int max_datatypes, int *array_of_integers,
                                 void *array_of_addresses, void *array_of_datatypes) {
    int rc;
    rc = ompi_datatype_get_args( (ompi_datatype_t*)mpi_type, 1, &max_integers, array_of_integers,
                                 &max_addresses, array_of_addresses,
                                 &max_datatypes, array_of_datatypes, NULL );
    return rc == OMPI_SUCCESS ? HCOLL_SUCCESS : HCOLL_ERROR;
}

static int get_hcoll_type(void *mpi_type, dte_data_representation_t *hcoll_type) {
    *hcoll_type = ompi_dtype_2_hcoll_dtype((ompi_datatype_t*)mpi_type, TRY_FIND_DERIVED);
    return HCOL_DTE_IS_ZERO((*hcoll_type)) ? HCOLL_ERR_NOT_FOUND : HCOLL_SUCCESS;
}

static int set_hcoll_type(void *mpi_type, dte_data_representation_t hcoll_type) {
    int rc;
    mca_coll_hcoll_dtype_t *hcoll_dtype = (mca_coll_hcoll_dtype_t*)
        opal_free_list_get(&mca_coll_hcoll_component.dtypes);
    ompi_datatype_t *dtype = (ompi_datatype_t*)mpi_type;
    hcoll_dtype->type = hcoll_type;
    rc = ompi_attr_set_c(TYPE_ATTR, (void*)dtype, &(dtype->d_keyhash), hcoll_type_attr_keyval, (void *)hcoll_dtype, false);
    if (OMPI_SUCCESS != rc) {
        HCOL_VERBOSE(1,"hcoll ompi_attr_set_c failed for derived dtype");
        goto Cleanup;
    }
    return HCOLL_SUCCESS;
Cleanup:
    opal_free_list_return(&mca_coll_hcoll_component.dtypes,
                          &hcoll_dtype->super);
    return rc;
}

static int get_mpi_constants(size_t *mpi_datatype_size,
                             int *mpi_order_c, int *mpi_order_fortran,
                             int *mpi_distribute_block,
                             int *mpi_distribute_cyclic,
                             int *mpi_distribute_none,
                             int *mpi_distribute_dflt_darg) {
    *mpi_datatype_size = sizeof(MPI_Datatype);
    *mpi_order_c = MPI_ORDER_C;
    *mpi_order_fortran = MPI_ORDER_FORTRAN;
    *mpi_distribute_block = MPI_DISTRIBUTE_BLOCK;
    *mpi_distribute_cyclic = MPI_DISTRIBUTE_CYCLIC;
    *mpi_distribute_none = MPI_DISTRIBUTE_NONE;
    *mpi_distribute_dflt_darg = MPI_DISTRIBUTE_DFLT_DARG;
    return HCOLL_SUCCESS;
}

#endif
