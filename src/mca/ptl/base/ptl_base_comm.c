/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include <string.h>
#include "ptl_base_comm.h"

static void mca_pml_ptl_comm_construct(mca_pml_ptl_comm_t* comm);
static void mca_pml_ptl_comm_destruct(mca_pml_ptl_comm_t* comm);


ompi_class_t mca_pml_ptl_comm_t_class = {
    "mca_pml_ptl_comm_t",
    OBJ_CLASS(ompi_object_t),
    (ompi_construct_t)mca_pml_ptl_comm_construct,
    (ompi_destruct_t)mca_pml_ptl_comm_destruct
};


static void mca_pml_ptl_comm_construct(mca_pml_ptl_comm_t* comm)
{
    OBJ_CONSTRUCT(&comm->c_wild_receives, ompi_list_t);
    OBJ_CONSTRUCT(&comm->c_matching_lock, ompi_mutex_t);
    comm->c_recv_seq = 0;
}


static void mca_pml_ptl_comm_destruct(mca_pml_ptl_comm_t* comm)
{
    free(comm->c_msg_seq);
    free(comm->c_next_msg_seq);
    free(comm->c_unexpected_frags);
    free(comm->c_frags_cant_match);
    free(comm->c_specific_receives);
    OBJ_DESTRUCT(&comm->c_wild_receives);
    OBJ_DESTRUCT(&comm->c_matching_lock);
}


int mca_pml_ptl_comm_init_size(mca_pml_ptl_comm_t* comm, size_t size)
{
    size_t i;

    /* send message sequence-number support - sender side */
    comm->c_msg_seq = malloc(sizeof(uint32_t) * size);
    if(NULL == comm->c_msg_seq)
        return OMPI_ERR_OUT_OF_RESOURCE;
    memset(comm->c_msg_seq, 0, sizeof(uint32_t) * size);

    /* send message sequence-number support - receiver side */
    comm->c_next_msg_seq = malloc(sizeof(uint16_t) * size);
    if(NULL == comm->c_next_msg_seq)
        return OMPI_ERR_OUT_OF_RESOURCE;
    memset(comm->c_next_msg_seq, 0, sizeof(uint16_t) * size);

    /* unexpected fragments queues */
    comm->c_unexpected_frags = malloc(sizeof(ompi_list_t) * size);
    if(NULL == comm->c_unexpected_frags)
        return OMPI_ERR_OUT_OF_RESOURCE;
    for(i=0; i<size; i++) {
        ompi_list_t* object = comm->c_unexpected_frags+i;
        OBJ_CONSTRUCT(object, ompi_list_t);
    }

     /* out-of-order fragments queues */
    comm->c_frags_cant_match = malloc(sizeof(ompi_list_t) * size);
    if(NULL == comm->c_frags_cant_match)
        return OMPI_ERR_OUT_OF_RESOURCE;
    for(i=0; i<size; i++) {
        ompi_list_t* object = comm->c_frags_cant_match+i;
        OBJ_CONSTRUCT(object, ompi_list_t);
    }

    /* queues of unmatched specific (source process specified) receives */
    comm->c_specific_receives = malloc(sizeof(ompi_list_t) * size);
    if(NULL == comm->c_specific_receives)
        return OMPI_ERR_OUT_OF_RESOURCE;
    for(i=0; i<size; i++) {
        ompi_list_t *object = comm->c_specific_receives+i;
        OBJ_CONSTRUCT(object, ompi_list_t);
    }
    return OMPI_SUCCESS;
}


