/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MTL_MX_H_HAS_BEEN_INCLUDED
#define MTL_MX_H_HAS_BEEN_INCLUDED

#include "opal/threads/threads.h"
#include "opal/threads/condition.h"
#include "ompi/class/ompi_free_list.h"
#include "opal/util/cmd_line.h"
#include "ompi/request/request.h"
#include "ompi/mca/mtl/mtl.h"
#include "ompi/mca/mtl/base/base.h"
#include "ompi/datatype/datatype.h"
#include "ompi/datatype/convertor.h"
#include "mtl_mx_endpoint.h" 

#include "myriexpress.h"


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


/* MTL interface functions */
extern int ompi_mtl_mx_add_procs(struct mca_mtl_base_module_t* mtl, 
                          size_t nprocs,
                          struct ompi_proc_t** procs, 
                          struct mca_mtl_base_endpoint_t **mtl_peer_data);
    
extern int ompi_mtl_mx_del_procs(struct mca_mtl_base_module_t* mtl, 
                                 size_t nprocs,
                                 struct ompi_proc_t** procs, 
                                 struct mca_mtl_base_endpoint_t **mtl_peer_data);

extern int ompi_mtl_mx_isend(struct mca_mtl_base_module_t* mtl, 
                             struct ompi_communicator_t* comm,
                             int dest,
                             int tag,
                             struct ompi_convertor_t *convertor,
                             mca_pml_base_send_mode_t mode,
                             bool blocking,
                             mca_mtl_request_t * mtl_request);

extern int ompi_mtl_mx_irecv(struct mca_mtl_base_module_t* mtl,
                             struct ompi_communicator_t *comm,
                             int src,
                             int tag,
                             struct ompi_convertor_t *convertor,
                             struct mca_mtl_request_t *mtl_request);
    
    
extern int ompi_mtl_mx_iprobe(struct mca_mtl_base_module_t* mtl, 
                              struct ompi_communicator_t *comm,
                              int src,
                              int tag,
                              int *flag,
                              struct ompi_status_public_t *status);

extern int ompi_mtl_mx_cancel(struct mca_mtl_base_module_t* mtl,
                              struct mca_mtl_request_t *mtl_request, 
                              int flag);
    
extern int ompi_mtl_mx_finalize(struct mca_mtl_base_module_t* mtl);

int ompi_mtl_mx_module_init(void);

/** 
 * MTL Module Interface
 */
struct mca_mtl_mx_module_t { 
    mca_mtl_base_module_t super; /**< base MTL interface */
    int32_t  mx_unexp_queue_max; /**< maximium size of the MX unexpected message queue */ 
    int32_t  mx_filter; /**< user assigned value used to filter incomming messages */
    int32_t  mx_timeout;
    int32_t  mx_retries;
    int32_t  mx_support_sharedmem;
    mx_endpoint_t mx_endpoint; /**< mx data structure for local endpoint */
    mx_endpoint_addr_t mx_endpoint_addr; /**< mx data structure for local endpoint address */
    mca_mtl_mx_addr_t mx_addr;
}; 
typedef struct mca_mtl_mx_module_t mca_mtl_mx_module_t;

extern mca_mtl_mx_module_t ompi_mtl_mx;

struct mca_mtl_mx_component_t{ 
    mca_mtl_base_component_1_0_0_t          super;  /**< base MTL component */ 
};
typedef struct mca_mtl_mx_component_t mca_mtl_mx_component_t;

extern mca_mtl_mx_component_t mca_mtl_mx_component;
    

/* match/ignore bit manipulation
 *
 * 01234567 01234567 01234567 01234567 01234567 01234567 01234567 01234567
 *                  |                 |
 *      context id  |      source     |            message tag
 *                  |                 |
 */

#define MX_CONTEXT_MASK  0xFFFF000000000000
#define MX_SOURCE_MASK   0x0000FFFF00000000
#define MX_TAG_MASK      0x00000000FFFFFFFF

#define MX_CONTEXT_IGNR  MX_CONTEXT_MASK
#define MX_SOURCE_IGNR   MX_SOURCE_MASK
#define MX_TAG_IGNR      0x00000000EFFFFFFF

/* get the tag from the bits */ 
#define MX_GET_TAG(match_bits, tag)                 \
{                                                   \
    tag = (int) (match_bits & MX_TAG_MASK);         \
}


/* get the tag from the bits */ 
#define MX_GET_SRC(match_bits, src)                     \
{                                                       \
    src = (int) ((match_bits & MX_SOURCE_MASK) >> 32);  \
}

/* send posting */
#define MX_SET_SEND_BITS(match_bits, contextid, source, tag)            \
{                                                                       \
    match_bits = contextid;                                             \
    match_bits = (match_bits << 16);                                    \
    match_bits |= source;                                               \
    match_bits = (match_bits << 32);                                    \
    match_bits |= (MX_TAG_MASK & tag);                                  \
}

/* receive posting */
#define MX_SET_RECV_BITS(match_bits, mask_bits, contextid, source, tag) \
{                                                                       \
    match_bits = mask_bits = 0;                                         \
    match_bits = contextid;                                             \
    match_bits = (match_bits << 16);                                    \
                                                                        \
    if (MPI_ANY_SOURCE == source) {                                     \
        match_bits = (match_bits << 32);                                \
        mask_bits |= MX_SOURCE_IGNR;                                    \
    } else {                                                            \
        match_bits |= source;                                           \
        match_bits = (match_bits << 32);                                \
    }                                                                   \
                                                                        \
    if (MPI_ANY_TAG == tag) {                                           \
        mask_bits |= MX_TAG_IGNR;                                       \
    } else {                                                            \
        match_bits |= (MX_TAG_MASK & tag);                              \
    }                                                                   \
                                                                        \
    mask_bits = ~mask_bits;                                             \
}
    

   
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif  /* MTL_MX_H_HAS_BEEN_INCLUDED */

