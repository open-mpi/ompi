#include "mca/oob/tcp/oob_tcp_peer.h"

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

