/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "mca/oob/tcp/oob_tcp.h"

/*
 * Struct of function pointers and all that to let us be initialized
 */
mca_oob_tcp_component_t mca_oob_tcp_component = {
  {
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
  }
};

static struct mca_oob_base_module_1_0_0_t mca_oob_tcp = {
    mca_oob_tcp_send,
    mca_oob_tcp_recv,
    mca_oob_tcp_send_nb,
    mca_oob_tcp_recv_nb
};

/*
 * Initialize global variables used w/in this module.
 */
int mca_oob_tcp_open(void)
{
#if 0
    mca_oob_tcp_module.tcp_listen_port = 1;
    OBJ_CONSTRUCT(&mca_oob_tcp_module.tcp_peer_list, ompi_list_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_module.tcp_peer_tree, ompi_rb_tree_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_module.tcp_peer_free, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_module.tcp_lock,      ompi_mutex_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_module.tcp_condition, ompi_condition_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_module.tcp_post_recv, ompi_list_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_module.tcp_msg_recv, ompi_list_t);
#endif
    return OMPI_SUCCESS;
}


/*
 * Cleanup of global variables used by this module.
 */

int mca_oob_tcp_close(void)
{
#if 0
    OBJ_DESTRUCT(&mca_oob_tcp_module.tcp_peer_list);
    OBJ_DESTRUCT(&mca_oob_tcp_module.tcp_peer_tree);
    OBJ_DESTRUCT(&mca_oob_tcp_module.tcp_peer_free);
    OBJ_DESTRUCT(&mca_oob_tcp_module.tcp_condition);
    OBJ_DESTRUCT(&mca_oob_tcp_module.tcp_lock);
    OBJ_DESTRUCT(&mca_oob_tcp_module.tcp_post_recv);
    OBJ_DESTRUCT(&mca_oob_tcp_module.tcp_msg_recv);
#endif
    return OMPI_SUCCESS;
}


/**
* Compare two process names for equality.
*
* @param  n1  Process name 1.
* @param  n2  Process name 2.
* @return     (-1 for n1<n2 0 for equality, 1 for n1>n2)
*
* Note that the definition of < or > is somewhat arbitrary -
* just needs to be consistently applied to maintain an ordering
* when process names are used as indices.
*/

static int ompi_process_name_compare(ompi_process_name_t* n1, 
                                     ompi_process_name_t* n2)
{
   if(n1->cellid < n2->cellid)
       return -1;
   else if(n1->cellid > n2->cellid)
       return 1;
   else if(n1->jobid < n2->jobid)
       return -1;
   else if(n1->jobid > n2->jobid)
       return 1;
   else if(n1->vpid < n2->vpid)
       return -1;
   else if(n1->vpid > n2->vpid)
       return 1;
   return(0);
} 


/*
 * this function will temporarily return NULL so we don't use it
 */
struct mca_oob_base_module_1_0_0_t* mca_oob_tcp_init(bool *allow_multi_user_threads,
                                                     bool *have_hidden_threads)
{
#if 0
    /* initialize data structures */
    ompi_rb_tree_init(&mca_oob_tcp_module.tcp_peer_tree, (ompi_rb_tree_comp_fn_t)ompi_process_name_compare);
#endif
    /* return &mca_oob_tcp; */
    return NULL;
}


int mca_oob_tcp_finalize(void)
{
    /* probably want to try to finish all sends and recieves here
     * before we return */
    return OMPI_SUCCESS;
}

