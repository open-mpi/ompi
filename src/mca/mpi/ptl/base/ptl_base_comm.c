#include "ptl_base_comm.h"

                                                                                                         
lam_class_info_t mca_pml_ptl_comm_cls = {
    "mca_pml_comm_t",
    &lam_object_cls,
    (class_init_t)mca_pml_ptl_comm_init,
    (class_destroy_t)mca_pml_ptl_comm_destroy
};
                                                                                                                       
void mca_pml_ptl_comm_init(mca_pml_comm_t* comm)
{
    SUPER_INIT(comm, &lam_object_cls);
    lam_list_init(&comm->wild_receives);
}

void mca_pml_ptl_comm_destroy(mca_pml_comm_t* comm)
{
    LAM_FREE(comm->c_msg_seq);
    LAM_FREE(comm->c_next_msg_seq);
    LAM_FREE(comm->c_matching_lock);
    LAM_FREE(comm->unexpected_frags);
    LAM_FREE(comm->unexpected_frags_lock);
    LAM_FREE(comm->frags_cant_match);
    LAM_FREE(comm->specific_receives);
    lam_list_destroy(&comm->wild_receives);
    SUPER_DESTROY(comm, &lam_object_cls);
}


int mca_pml_ptl_comm_init_size(mca_pml_comm_t* comm, size_t size)
{
    size_t i;

    /* send message sequence-number support - sender side */
    comm->c_msg_seq = (mca_ptl_base_sequence_t*)LAM_MALLOC(sizeof(mca_ptl_base_sequence_t) * size);
    if(NULL == comm->c_msg_seq)
        return LAM_ERR_OUT_OF_RESOURCE;

    /* send message sequence-number support - receiver side */
    comm->c_next_msg_seq = (mca_ptl_base_sequence_t*)LAM_MALLOC(sizeof(mca_ptl_base_sequence_t) * size);
    if(NULL == comm->c_next_msg_seq)
        return LAM_ERR_OUT_OF_RESOURCE;

    /* matching lock */
    comm->c_matching_lock = (lam_mutex_t*)LAM_MALLOC(sizeof(lam_mutex_t) * size);
    if(NULL == comm->c_matching_lock)
        return LAM_ERR_OUT_OF_RESOURCE;
    for(i=0; i<size; i++)
        lam_mutex_init(comm->c_matching_lock+i);

    /* unexpected fragments queues */
    comm->unexpected_frags = (lam_list_t*)LAM_MALLOC(sizeof(lam_list_t) * size);
    if(NULL == comm->unexpected_frags)
        return LAM_ERR_OUT_OF_RESOURCE;
    for(i=0; i<size; i++)
        lam_list_init(comm->unexpected_frags+i);

    /* these locks are needed to avoid a probe interfering with a match */
    comm->unexpected_frags_lock = (lam_mutex_t*)LAM_MALLOC(sizeof(lam_mutex_t) * size);
    if(NULL == comm->unexpected_frags_lock)
        return LAM_ERR_OUT_OF_RESOURCE;
    for(i=0; i<size; i++)
        lam_mutex_init(comm->unexpected_frags_lock+i);

     /* out-of-order fragments queues */
    comm->frags_cant_match = (lam_list_t*)LAM_MALLOC(sizeof(lam_list_t) * size);
    if(NULL == comm->frags_cant_match)
        return LAM_ERR_OUT_OF_RESOURCE;
    for(i=0; i<size; i++)
        lam_list_init(comm->frags_cant_match+i);

    /* queues of unmatched specific (source process specified) receives */
    comm->specific_receives = (lam_list_t*)LAM_MALLOC(sizeof(lam_list_t) * size);
    if(NULL == comm->specific_receives)
        return LAM_ERR_OUT_OF_RESOURCE;
    for(i=0; i<size; i++)
        lam_list_init(comm->specific_receives+i);

    return LAM_SUCCESS;
}


