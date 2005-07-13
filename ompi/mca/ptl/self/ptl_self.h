/*
 *Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                        All rights reserved.
 *Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                        All rights reserved.
 *Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                        University of Stuttgart.  All rights reserved.
 *Copyright (c) 2004-2005 The Regents of the University of California.
 *                        All rights reserved.
 *$COPYRIGHT$
 *
 *Additional copyrights may follow
 *
 *$HEADER$
 */
/**
 * @file
 */

#ifndef PTL_SELF_H_HAS_BEEN_INCLUDED
#define PTL_SELF_H_HAS_BEEN_INCLUDED

#include "class/ompi_free_list.h"
#include "opal/event/event.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "mca/ptl/base/ptl_base_sendreq.h"
#include "mca/ptl/base/ptl_base_recvreq.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * SELF PTL component.
 */
struct mca_ptl_self_component_t {
   mca_ptl_base_component_1_0_0_t super;              /**< base PTL component */
   uint32_t            self_buf_size;         /**< the size of the internal buffer used to pack/unpack the data */
   uint32_t            self_is_non_blocking;  /**< how the memcopy operations are done segmented or not */
   int32_t             self_free_list_num;    /**< initial size of free lists */
   int32_t             self_free_list_max;    /**< maximum size of free lists */
   int32_t             self_free_list_inc;    /**< number of elements to alloc when growing free lists */
   ompi_free_list_t    self_send_requests;    /**< free list of self send requests -- sendreq + sendfrag */
   struct ompi_proc_t* self_local;            /**< the self proc instance corresponding to the local process */
};
typedef struct mca_ptl_self_component_t mca_ptl_self_component_t;

/**
 * Self send request derived type. The send request contains both the
 * base send request, and the base receive fragment which will be used to do the match.
 */
struct mca_ptl_self_send_request_t {
   mca_ptl_base_send_request_t req_ptl;
   mca_ptl_base_recv_frag_t req_frag; /* first fragment */
};
typedef struct mca_ptl_self_send_request_t mca_ptl_self_send_request_t;
OBJ_CLASS_DECLARATION(mca_ptl_self_send_request_t);

extern mca_ptl_self_component_t mca_ptl_self_component;

/**
 * Register SELF component parameters with the MCA framework
 */
extern int mca_ptl_self_component_open(void);

/**
 * Any final cleanup before being unloaded.
 */
extern int mca_ptl_self_component_close(void);

/**
 * SELF component initialization.
 * 
 * @param num_ptls (OUT)                  Number of PTLs returned in PTL array.
 * @param allow_multi_user_threads (OUT)  Flag indicating wether PTL supports user threads (TRUE)
 * @param have_hidden_threads (OUT)       Flag indicating wether PTL uses threads (TRUE)
 *
 *  (1) prepare the local buffering and initialize the SELF
 *      engine.
 */
extern mca_ptl_base_module_t** mca_ptl_self_component_init(
    int *num_ptls, 
    bool enable_progress_threads,
    bool enable_mpi_threads
);

int  mca_ptl_self_add_proc(struct mca_ptl_base_module_t* ptl, size_t nprocs, struct ompi_proc_t **ompi_proc, struct mca_ptl_base_peer_t** peer_ret, struct ompi_bitmap_t* reachable);
int  mca_ptl_self_del_proc(struct mca_ptl_base_module_t* ptl, size_t nprocs, struct ompi_proc_t **proc, struct mca_ptl_base_peer_t** ptl_peer);
int  mca_ptl_self_finalize(struct mca_ptl_base_module_t* ptl);
int mca_ptl_self_request_init(struct mca_ptl_base_module_t* ptl, struct mca_ptl_base_send_request_t* request);
void mca_ptl_self_request_fini(struct mca_ptl_base_module_t* ptl, struct mca_ptl_base_send_request_t* request);
int  mca_ptl_self_send( struct mca_ptl_base_module_t* ptl, struct mca_ptl_base_peer_t* ptl_base_peer, struct mca_ptl_base_send_request_t* request,
                      size_t offset, size_t size, int flags );
void mca_ptl_self_matched( mca_ptl_base_module_t* ptl, mca_ptl_base_recv_frag_t* frag );

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif  /* PTL_SELF_H_HAS_BEEN_INCLUDED */

