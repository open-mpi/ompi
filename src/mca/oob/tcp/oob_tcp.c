/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "mca/oob/tcp/oob_tcp.h"

/*
 * Struct of function pointers and all that to let us be initialized
 */
mca_oob_base_component_1_0_0_t mca_oob_tcp_module = {
    {
        MCA_OOB_BASE_VERSION_1_0_0,

        "tcp", /* MCA module name */
        1,  /* MCA module major version */
        0,  /* MCA module minor version */
        0,  /* MCA module release version */
        mca_oob_tcp_open,  /* module open */
        mca_oob_tcp_close /* module close */
    },
    {
        false /* checkpoint / restart */
    },
    mca_oob_tcp_init,    /* module init */
    mca_oob_tcp_finalize
};

struct mca_oob_1_0_0_t mca_oob_tcp_1_0_0 = {
    mca_oob_tcp_send,
    mca_oob_tcp_recv,
    mca_oob_tcp_send_nb,
    mca_oob_tcp_recv_nb
};

/*
 * for now these 2 functions simply return an error so we won't
 * use this module
 */
int mca_oob_tcp_open(void)
{
    return OMPI_ERROR;
}


int mca_oob_tcp_close(void)
{
    return OMPI_ERROR;
}

/*
 * this function will temporarily return NULL so we don't use it
 */
struct mca_oob_1_0_0_t* mca_oob_tcp_init(bool *allow_multi_user_threads,
                                         bool *have_hidden_threads)
{
    /* set up the list for the cache of peer processes */
    OBJ_CONSTRUCT(&mca_oob_tcp_peer_list, ompi_list_t);
    /* set up the rb tree for the cache of peer processes */
    OBJ_CONSTRUCT(&mca_oob_tcp_peer_tree, ompi_rb_tree_t);
    ompi_rb_tree_init(&mca_oob_tcp_peer_tree, &mca_oob_tcp_peer_comp);
    /* return &mca_oob_tcp_1_0_0; */
    return NULL;
}


int mca_oob_tcp_finalize(void)
{
    OBJ_DESTRUCT(&mca_oob_tcp_peer_list);
    OBJ_DESTRUCT(&mca_oob_tcp_peer_tree);
    return OMPI_SUCCESS;
}
