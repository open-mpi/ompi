#include "mca/oob/tcp/oob_tcp_peer.h"


OBJ_CLASS_INSTANCE(
    mca_oob_tcp_peer_t,
    ompi_list_item_t,
    &mca_oob_tcp_peer_construct,
    &mca_oob_tcp_peer_destruct);


/*
 * The function to compare 2 peers. Used for the rb tree
 *
 * @param peer1 the first peer
 * @param peer2 the second peer
 *
 * @retval <0 if peer1 < peer2
 * @retval >0 if peer1 > peer2
 * @retval 0 if peer1 == peer2
 */
/*****NEED TO MAKE WORK *****/
int mca_oob_tcp_peer_comp(void * key1, void * key2)
{
/*    mca_oob_tcp_peer_t * p1 = (mca_oob_tcp_peer_t *) key1;
    mca_oob_tcp_peer_t * p2 = (mca_oob_tcp_peer_t *) key2;

    if(p1->peer_name < p2->peer_name) {
        return(-1);
    } else if(p1->peer_name > p2->peer_name) {
        return(1);
    }*/
    return(0);
}

/*
 * This is the constructor function for the mca_oob_tcp_peer
 * struct. Note that this function and OBJ_NEW should NEVER
 * be called directly. Instead, use mca_oob_tcp_add_peer
 *
 * @param peer a pointer to the mca_oob_tcp_peer_t struct to be initialized
 * @retval none
 */
void mca_oob_tcp_peer_construct(mca_oob_tcp_peer_t* peer) 
{ 
    OBJ_CONSTRUCT(&(peer->peer_send), ompi_list_t);
    OBJ_CONSTRUCT(&(peer->peer_recv), ompi_list_t);
    OBJ_CONSTRUCT(&(peer->peer_lock), ompi_mutex_t);
}

/*
 * This is the destructor function for the mca_oob_tcp_peer
 * struct. Note that this function and OBJ_RELEASE should NEVER
 * be called directly. Instead, use mca_oob_tcp_del_peer
 *
 * @param peer a pointer to the mca_oob_tcp_peer_t struct to be destroyed
 * @retval none
 */
void mca_oob_tcp_peer_destruct(mca_oob_tcp_peer_t * peer)
{
    OBJ_DESTRUCT(&(peer->peer_send));
    OBJ_DESTRUCT(&(peer->peer_recv));
    OBJ_DESTRUCT(&(peer->peer_lock));

}

/*
 * Creates a peer structure and adds to the tree and list.
 *
 * @param peer_name the name of the peer
 *
 * @retval pointer to the newly created struture
 * @retval NULL if there was a problem
 */
mca_oob_tcp_peer_t * mca_oob_tcp_add_peer(ompi_process_name_t peer_name)
{
    mca_oob_tcp_peer_t * new_peer = OBJ_NEW(mca_oob_tcp_peer_t);
    new_peer->peer_name = peer_name;
    if(OMPI_SUCCESS != ompi_rb_tree_insert(&mca_oob_tcp_peer_tree, &new_peer, NULL)) {
        free(new_peer);
        return NULL;
    }
    ompi_list_prepend(&mca_oob_tcp_peer_list, (ompi_list_item_t *) new_peer);
    return new_peer;
}

/*
 * Deletes a peer structure from the tree and lists and frees its memory
 *
 * @param peer_name the name of the peer
 *
 * @retval OMPI_SUCCESS
 */ 
int mca_oob_tcp_del_peer(ompi_process_name_t peer_name)
{
    mca_oob_tcp_peer_t * peer = ompi_rb_tree_find(&mca_oob_tcp_peer_tree, &peer_name);
    ompi_rb_tree_delete(&mca_oob_tcp_peer_tree, peer);
    ompi_list_remove_item(&mca_oob_tcp_peer_list, (ompi_list_item_t *)peer);
    OBJ_RELEASE(peer);
    return OMPI_SUCCESS;
}

