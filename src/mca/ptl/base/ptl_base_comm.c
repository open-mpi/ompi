/*
 * $HEADER$
 */

#include <string.h>
#include "ptl_base_comm.h"

static void mca_pml_ptl_comm_construct(mca_pml_ptl_comm_t* comm);
static void mca_pml_ptl_comm_destruct(mca_pml_ptl_comm_t* comm);


lam_class_t mca_pml_ptl_comm_t_class = {
    "mca_pml_ptl_comm_t",
    OBJ_CLASS(lam_object_t),
    (lam_construct_t)mca_pml_ptl_comm_construct,
    (lam_destruct_t)mca_pml_ptl_comm_destruct
};


static void mca_pml_ptl_comm_construct(mca_pml_ptl_comm_t* comm)
{
    OBJ_CONSTRUCT(&comm->c_wild_receives, lam_list_t);
    OBJ_CONSTRUCT(&comm->c_wild_lock, lam_mutex_t);
}


static void mca_pml_ptl_comm_destruct(mca_pml_ptl_comm_t* comm)
{
    free(comm->c_msg_seq);
    free(comm->c_next_msg_seq);
    free(comm->c_matching_lock);
    free(comm->c_unexpected_frags);
    free(comm->c_unexpected_frags_lock);
    free(comm->c_frags_cant_match);
    free(comm->c_specific_receives);
    OBJ_DESTRUCT(&comm->c_wild_receives);
}


int mca_pml_ptl_comm_init_size(mca_pml_ptl_comm_t* comm, size_t size)
{
    size_t i;

    /* send message sequence-number support - sender side */
    comm->c_msg_seq = malloc(sizeof(mca_ptl_base_sequence_t) * size);
    if(NULL == comm->c_msg_seq)
        return LAM_ERR_OUT_OF_RESOURCE;
    memset(comm->c_msg_seq, 0, sizeof(mca_ptl_base_sequence_t) * size);

    /* send message sequence-number support - receiver side */
    comm->c_next_msg_seq = malloc(sizeof(mca_ptl_base_sequence_t) * size);
    if(NULL == comm->c_next_msg_seq)
        return LAM_ERR_OUT_OF_RESOURCE;
    memset(comm->c_next_msg_seq, 0, sizeof(mca_ptl_base_sequence_t) * size);

    /* matching lock */
    comm->c_matching_lock = malloc(sizeof(lam_mutex_t) * size);
    if(NULL == comm->c_matching_lock)
        return LAM_ERR_OUT_OF_RESOURCE;
    for(i=0; i<size; i++) {
        lam_mutex_t *object = comm->c_matching_lock+i;
        OBJ_CONSTRUCT(object, lam_mutex_t);
    }

    /* unexpected fragments queues */
    comm->c_unexpected_frags = malloc(sizeof(lam_list_t) * size);
    if(NULL == comm->c_unexpected_frags)
        return LAM_ERR_OUT_OF_RESOURCE;
    for(i=0; i<size; i++) {
        lam_list_t* object = comm->c_unexpected_frags+i;
        OBJ_CONSTRUCT(object, lam_list_t);
    }

    /* these locks are needed to avoid a probe interfering with a match */
    comm->c_unexpected_frags_lock = malloc(sizeof(lam_mutex_t) * size);
    if(NULL == comm->c_unexpected_frags_lock)
        return LAM_ERR_OUT_OF_RESOURCE;
    for(i=0; i<size; i++) {
        lam_mutex_t* object = comm->c_unexpected_frags_lock+i;
        OBJ_CONSTRUCT(object, lam_mutex_t);
    }

     /* out-of-order fragments queues */
    comm->c_frags_cant_match = malloc(sizeof(lam_list_t) * size);
    if(NULL == comm->c_frags_cant_match)
        return LAM_ERR_OUT_OF_RESOURCE;
    for(i=0; i<size; i++) {
        lam_list_t* object = comm->c_frags_cant_match+i;
        OBJ_CONSTRUCT(object, lam_list_t);
    }

    /* queues of unmatched specific (source process specified) receives */
    comm->c_specific_receives = malloc(sizeof(lam_list_t) * size);
    if(NULL == comm->c_specific_receives)
        return LAM_ERR_OUT_OF_RESOURCE;
    for(i=0; i<size; i++) {
        lam_list_t *object = comm->c_specific_receives+i;
        OBJ_CONSTRUCT(object, lam_list_t);
    }
    return LAM_SUCCESS;
}


