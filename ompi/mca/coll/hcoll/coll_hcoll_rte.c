/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2013      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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


#include "hcoll_dte.h"
#include "hcoll_api.h"
#include "hcoll_constants.h"
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
}


void hcoll_rte_fns_setup(void)
{
    init_module_fns();
    OBJ_CONSTRUCT(&mca_coll_hcoll_component.requests, ompi_free_list_t);
    ompi_free_list_init_ex_new(
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
                /* No Mpool */
                NULL,
                NULL,
                NULL
                );
}

/* This one converts dte_general_representation data into regular iovec array which is
  used in rml
  */

static inline int count_total_dte_repeat_entries(struct dte_data_representation_t *data){
    unsigned int i;

    struct dte_generalized_iovec_t * dte_iovec =
            data->rep.general_rep->data_representation.data;
    int total_entries_number = 0;
    for (i=0; i< dte_iovec->repeat_count; i++){
        total_entries_number += dte_iovec->repeat[i].n_elements;
    }
    return total_entries_number;
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
    if (HCOL_DTE_IS_INLINE(data)){
        /*do inline nb recv*/
        int rc;
        size_t size;
        ompi_request_t *ompi_req;
        ompi_free_list_item_t *item;

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
    }else{
        /*do iovec nb recv*/
        int total_entries_number;
        int i;
        unsigned int j;
        void *buf;
        uint64_t len;
        int repeat_count;
        struct dte_struct_t * repeat;
        if (NULL != buffer) {
            /* We have a full data description & buffer pointer simultaneously.
               It is ambiguous. Throw a warning since the user might have made a
               mistake with data reps*/
            fprintf(stderr,"Warning: buffer_pointer != NULL for NON-inline data representation: buffer_pointer is ignored.\n");
        }
        total_entries_number = count_total_dte_repeat_entries(&data);
        repeat = data.rep.general_rep->data_representation.data->repeat;
        repeat_count = data.rep.general_rep->data_representation.data->repeat_count;
        for (i=0; i< repeat_count; i++){
            for (j=0; j<repeat[i].n_elements; j++){
                char *repeat_unit = (char *)&repeat[i];
                buf = (void *)(repeat_unit+repeat[i].elements[j].base_offset);
                len = repeat[i].elements[j].packed_size;
                recv_nb(DTE_BYTE,len,buf,ec_h,grp_h,tag,req);
            }
        }

    }

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
    if (HCOL_DTE_IS_INLINE(data)){
        /*do inline nb recv*/
        int rc;
        size_t size;
        ompi_request_t *ompi_req;
        ompi_free_list_item_t *item;
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
    }else{
        int total_entries_number;
        int i;
        unsigned int j;
        void *buf;
        uint64_t len;
        int repeat_count;
        struct dte_struct_t * repeat;
        if (NULL != buffer) {
            /* We have a full data description & buffer pointer simultaneously.
               It is ambiguous. Throw a warning since the user might have made a
               mistake with data reps*/
            fprintf(stderr,"Warning: buffer_pointer != NULL for NON-inline data representation: buffer_pointer is ignored.\n");
        }
        total_entries_number = count_total_dte_repeat_entries(&data);
        repeat = data.rep.general_rep->data_representation.data->repeat;
        repeat_count = data.rep.general_rep->data_representation.data->repeat_count;
        for (i=0; i< repeat_count; i++){
            for (j=0; j<repeat[i].n_elements; j++){
                char *repeat_unit = (char *)&repeat[i];
                buf = (void *)(repeat_unit+repeat[i].elements[j].base_offset);
                len = repeat[i].elements[j].packed_size;
                send_nb(DTE_BYTE,len,buf,ec_h,grp_h,tag,req);
            }
        }
    }
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
    *completed = ompi_req->req_complete;
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
    return OPAL_PROC_ON_LOCAL_NODE(proc->proc_flags);
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
    ompi_free_list_item_t *item;
    OMPI_FREE_LIST_WAIT_MT(&(mca_coll_hcoll_component.requests),item);
    if (OPAL_UNLIKELY(NULL == item)) {
        HCOL_ERROR("Wait for free list failed.\n");
        return NULL;
    }
    ompi_req = (ompi_request_t *)item;
    OMPI_REQUEST_INIT(ompi_req,false);
    ompi_req->req_complete_cb = NULL;
    ompi_req->req_status.MPI_ERROR = MPI_SUCCESS;
    ompi_req->req_free = request_free;
    return (void *)ompi_req;
}

static int coll_handle_test(void* handle)
{
    ompi_request_t *ompi_req = (ompi_request_t *)handle;
    return ompi_req->req_complete;
}

static void coll_handle_free(void *handle){
    ompi_request_t *ompi_req = (ompi_request_t *)handle;
    OMPI_FREE_LIST_RETURN_MT(&mca_coll_hcoll_component.requests,
                          (ompi_free_list_item_t *)ompi_req);
}

static void coll_handle_complete(void *handle)
{
    ompi_request_t *ompi_req = (ompi_request_t *)handle;
    ompi_request_complete(ompi_req,true);
}


static int world_rank(rte_grp_handle_t grp_h, rte_ec_handle_t ec){
    ompi_proc_t *proc = (ompi_proc_t *)ec.handle;
    return proc->proc_name.vpid;
}
