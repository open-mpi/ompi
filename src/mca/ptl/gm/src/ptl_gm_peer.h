/* -*- Mode: C; c-basic-offset:4 ; -*- */

/*
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_GM_PEER_H
#define MCA_PTL_GM_PEER_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "class/ompi_list.h"
#include "event/event.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "include/types.h"
/*#include "ptl_gm_sendfrag.h"*/
#include "ptl_gm_proc.h"
#include "ptl_gm_addr.h"
#include "ptl_gm.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
/**
 *  An abstraction that represents a connection to a peer process.
 */
struct mca_ptl_gm_peer_t {
    ompi_list_item_t super;
    struct mca_ptl_gm_module_t *peer_ptl;
    struct mca_ptl_gm_proc_t *peer_proc;
    struct mca_ptl_gm_addr_t *peer_addr;   /**< address of peer */
    unsigned int global_id;
    unsigned int port_number;
    unsigned int local_id;
    int         num_credits;
    int         max_credits;
    int         resending;
    int         num_resend;
};
typedef struct mca_ptl_gm_peer_t mca_ptl_gm_peer_t;
/*extern omp_class_t mca_ptl_gm_peer_t_class;*/

OBJ_CLASS_DECLARATION(mca_ptl_gm_peer_t);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
