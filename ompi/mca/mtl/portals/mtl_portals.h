/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MTL_PORTALS_H_HAS_BEEN_INCLUDED
#define MTL_PORTALS_H_HAS_BEEN_INCLUDED

#include "opal/threads/threads.h"
#include "opal/threads/condition.h"
#include "opal/class/opal_list.h"
#include "ompi/class/ompi_free_list.h"
#include "opal/util/cmd_line.h"
#include "ompi/request/request.h"
#include "ompi/mca/mtl/mtl.h"
#include "ompi/mca/mtl/base/base.h"
#include "ompi/datatype/datatype.h"
#include "ompi/datatype/convertor.h"

#include "ompi/mca/common/portals/common_portals.h"


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

struct mca_mtl_portals_module_t {
    mca_mtl_base_module_t base;

    ptl_handle_ni_t ptl_ni_h;
    size_t eager_limit;

    ptl_handle_eq_t ptl_eq_h;
    ptl_handle_eq_t ptl_unexpected_recv_eq_h;

    /* insert all posted receives before this handle */
    ptl_handle_me_t ptl_match_ins_me_h;
    /* last handle in the SEND table entry */
    ptl_handle_me_t ptl_unexpected_me_h;

    ompi_free_list_t event_fl;

    int ptl_recv_short_mds_num;
    int ptl_recv_short_mds_size;
    
    opal_list_t ptl_recv_short_blocks;
    opal_list_t unexpected_messages;

    int ptl_expected_queue_size;
    int ptl_unexpected_queue_size;
};
typedef struct mca_mtl_portals_module_t mca_mtl_portals_module_t;

extern mca_mtl_portals_module_t ompi_mtl_portals;

OMPI_DECLSPEC mca_mtl_base_component_1_0_0_t mca_mtl_portals_component;


struct ompi_mtl_portals_event_t {
    struct ompi_free_list_item_t super;
    ptl_event_t ev;
    bool is_complete;
};
typedef struct ompi_mtl_portals_event_t ompi_mtl_portals_event_t;
OBJ_CLASS_DECLARATION(ompi_mtl_portals_event_t);


/* match/ignore bit manipulation
 *
 * 0123 4567 01234567 01234567 01234567 01234567 01234567 01234567 01234567
 *     |             |                 |
 * ^   | context id  |      source     |            message tag
 * |   |             |                 |
 * +---- protocol
 */

#define PTL_PROTOCOL_MASK 0xF000000000000000ULL
#define PTL_CONTEXT_MASK  0x0FFF000000000000ULL
#define PTL_SOURCE_MASK   0x0000FFFF00000000ULL
#define PTL_TAG_MASK      0x00000000FFFFFFFFULL

#define PTL_PROTOCOL_IGNR PTL_PROTOCOL_MASK
#define PTL_CONTEXT_IGNR  PTL_CONTEXT_MASK
#define PTL_SOURCE_IGNR   PTL_SOURCE_MASK
#define PTL_TAG_IGNR      0x000000007FFFFFFFULL

#define PTL_SHORT_MSG     0x1000000000000000ULL
#define PTL_LONG_MSG      0x2000000000000000ULL
#define PTL_READY_MSG     0x4000000000000000ULL

/* send posting */
#define PTL_SET_SEND_BITS(match_bits, contextid, source, tag, type)     \
{                                                                       \
    match_bits = contextid;                                             \
    match_bits = (match_bits << 16);                                    \
    match_bits |= source;                                               \
    match_bits = (match_bits << 32);                                    \
    match_bits |= (PTL_TAG_MASK & tag) | type;                          \
}

/* receive posting */
#define PTL_SET_RECV_BITS(match_bits, ignore_bits, contextid, source, tag) \
{                                                                   \
    match_bits = 0;                                                 \
    ignore_bits = PTL_PROTOCOL_IGNR;                                \
                                                                    \
    match_bits = contextid;                                         \
    match_bits = (match_bits << 16);                                \
                                                                    \
    if (MPI_ANY_SOURCE == source) {                                 \
        match_bits = (match_bits << 32);                            \
        ignore_bits |= PTL_SOURCE_IGNR;                             \
    } else {                                                        \
        match_bits |= source;                                       \
        match_bits = (match_bits << 32);                            \
    }                                                               \
                                                                    \
    if (MPI_ANY_TAG == tag) {                                       \
        ignore_bits |= PTL_TAG_IGNR;                                \
    } else {                                                        \
        match_bits |= (PTL_TAG_MASK & tag);                         \
    }                                                               \
}

#define PTL_IS_SHORT_MSG(match_bits)            \
    (0 != (PTL_SHORT_MSG & match_bits))
#define PTL_IS_READY_MSG(match_bits)            \
    (0 != (PTL_READY_MSG & match_bits))
#define PTL_IS_SYNC_MSG(event) \
    (0 != event.hdr_data)

#define PTL_GET_TAG(match_bits) ((int)(match_bits & PTL_TAG_MASK))
#define PTL_GET_SOURCE(match_bits) ((int)((match_bits & PTL_SOURCE_MASK) >> 32))

/* MTL interface functions */
extern int ompi_mtl_portals_finalize(struct mca_mtl_base_module_t *mtl);

extern int ompi_mtl_portals_add_procs(struct mca_mtl_base_module_t* mtl, 
                          size_t nprocs,
                          struct ompi_proc_t** procs, 
                          struct mca_mtl_base_endpoint_t **mtl_peer_data);

extern int ompi_mtl_portals_del_procs(struct mca_mtl_base_module_t* mtl, 
                          size_t nprocs,
                          struct ompi_proc_t** procs, 
                          struct mca_mtl_base_endpoint_t **mtl_peer_data);

extern int ompi_mtl_portals_send(struct mca_mtl_base_module_t* mtl,
                          struct ompi_communicator_t* comm,
                          int dest,
                          int tag,
                          struct ompi_convertor_t *convertor,
                          mca_pml_base_send_mode_t mode);

extern int ompi_mtl_portals_isend(struct mca_mtl_base_module_t* mtl,
                          struct ompi_communicator_t* comm,
                          int dest,
                          int tag,
                          struct ompi_convertor_t *convertor,
                          mca_pml_base_send_mode_t mode,
                          bool blocking,
                          mca_mtl_request_t *mtl_request);

extern int ompi_mtl_portals_irecv(struct mca_mtl_base_module_t* mtl,
                          struct ompi_communicator_t *comm,
                          int src,
                          int tag,
                          struct ompi_convertor_t *convertor,
                          mca_mtl_request_t *mtl_request);

extern int ompi_mtl_portals_iprobe(struct mca_mtl_base_module_t* mtl,
                          struct ompi_communicator_t *comm,
                          int src,
                          int tag,
                          int *flag,
                          struct ompi_status_public_t *status);

extern int ompi_mtl_portals_cancel(struct mca_mtl_base_module_t* mtl,
                          mca_mtl_request_t *mtl_request,
                          int flag);

extern int ompi_mtl_portals_progress(void);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif  /* MTL_PORTALS_H_HAS_BEEN_INCLUDED */
