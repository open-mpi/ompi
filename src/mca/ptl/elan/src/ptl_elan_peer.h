/*
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_ELAN_PEER_H
#define MCA_PTL_ELAN_PEER_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "class/ompi_list.h"
#include "event/event.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "ptl_elan_frag.h"

typedef enum { 
    MCA_PTL_ELAN_CLOSED, 
    MCA_PTL_ELAN_CONNECTED,
    MCA_PTL_ELAN_FAILED,
    NUM_MCA_PTL_ELAN_STAT
} mca_ptl_elan_status_t;
    
/**
 *  An abstraction that represents a connection to a peer process.
 *  Peers are always connected unless they are in different LAN or died.
 */
struct mca_ptl_elan_peer_t {
    ompi_list_item_t            super;

    struct mca_ptl_elan_t*      peer_ptl; 
    struct mca_ptl_elan_proc_t* peer_proc; 
    struct mca_ptl_elan_addr_t* peer_addr;        /**< address of peer */

    int     resending;   /* A resending stage, no more new dma's */
    int     num_resend;  /* How many times I have retried */
    double  open_time; 
    double  close_time; 
    double  known_alive_time;                
};
typedef struct mca_ptl_elan_peer_t mca_ptl_elan_peer_t;

extern ompi_class_t mca_ptl_elan_peer_t_class; 

#endif

