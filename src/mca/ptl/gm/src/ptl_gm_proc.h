/* -*- Mode: C; c-basic-offset:4 ; -*- */

/* 
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_GM_PROC_H
#define MCA_PTL_GM_PROC_H

#include "class/ompi_object.h"
#include "ptl_gm.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

extern ompi_class_t mca_ptl_gm_proc_t_class;

struct mca_ptl_gm_proc_t {
    ompi_list_item_t super;                 /**< allow proc to be placed on a list */
    struct ompi_proc_t *proc_ompi;          /**< pointer to corresponding ompi_proc_t */
    struct mca_ptl_gm_addr_t *proc_addrs;   /**< array of addresses published by peer */
    ompi_mutex_t proc_lock;                 /**< lock to protect against concurrent access to proc state */
    size_t      proc_peer_count;
    size_t      proc_addr_count;
    struct mca_ptl_gm_peer_t **peer_arr;
    ompi_process_name_t proc_guid;
};
typedef struct mca_ptl_gm_proc_t mca_ptl_gm_proc_t;


mca_ptl_gm_proc_t *mca_ptl_gm_proc_create (mca_ptl_gm_module_t * ptl,
                                           struct ompi_proc_t * ompi_proc);
mca_ptl_gm_proc_t *mca_ptl_gm_proc_lookup (void *guid, size_t size);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
