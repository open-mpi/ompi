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
 * Copyright (c) 2010      Sandia National Laboratories.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MTL_PORTALS_H_HAS_BEEN_INCLUDED
#define MTL_PORTALS_H_HAS_BEEN_INCLUDED

#include <portals4.h>

#include "ompi_config.h"
#include "opal/class/opal_list.h"
#include "ompi/class/ompi_free_list.h"
#include "ompi/mca/mtl/mtl.h"
#include "ompi/mca/mtl/base/base.h"
#include "opal/datatype/opal_convertor.h"

#include "mtl_portals4_request.h"

BEGIN_C_DECLS

struct mca_mtl_portals4_module_t {
    mca_mtl_base_module_t base;

    /* configuration */
    size_t eager_limit;
    size_t recv_short_size;
    int recv_short_num;
    int queue_size;

    ptl_pt_index_t send_idx;
    ptl_pt_index_t read_idx;

    /* global handles */
    ptl_handle_ni_t ni_h;
    ptl_handle_eq_t eq_h;

    /* for zero-length sends and acks */
    ptl_handle_md_t zero_md_h;
    /* long message receive overflow */
    ptl_handle_me_t long_overflow_me_h;
    ompi_mtl_portals4_request_t long_overflow_request;

    opal_list_t recv_short_blocks;

    /* note: this isn't entirely accurate... it's really long op count */
    uint32_t opcount;

    enum { eager, rndv, triggered } protocol;
};
typedef struct mca_mtl_portals4_module_t mca_mtl_portals4_module_t;

extern mca_mtl_portals4_module_t ompi_mtl_portals4;

#define REQ_SEND_TABLE_ID 2
#define REQ_READ_TABLE_ID 3


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
    {                                                                   \
        match_bits = contextid;                                         \
        match_bits = (match_bits << 16);                                \
        match_bits |= source;                                           \
        match_bits = (match_bits << 32);                                \
        match_bits |= (PTL_TAG_MASK & tag) | type;                      \
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
#define PTL_IS_LONG_MSG(match_bits)             \
    (0 != (PTL_LONG_MSG & match_bits))
#define PTL_IS_READY_MSG(match_bits)            \
    (0 != (PTL_READY_MSG & match_bits))
#define PTL_IS_SYNC_MSG(ev)                  \
    (0 != ev->hdr_data)

#define PTL_GET_TAG(match_bits) ((int)(match_bits & PTL_TAG_MASK))
#define PTL_GET_SOURCE(match_bits) ((int)((match_bits & PTL_SOURCE_MASK) >> 32))

#define MTL_PORTALS4_SET_HDR_DATA(hdr_data, opcount, length) \
    {                                                        \
        hdr_data = ((length << 16) | (opcount & 0xFFFF));    \
    }

#define MTL_PORTALS4_GET_LENGTH(hdr_data) (hdr_data >> 16)


/* MTL interface functions */
extern int ompi_mtl_portals4_finalize(struct mca_mtl_base_module_t *mtl);

extern int ompi_mtl_portals4_add_procs(struct mca_mtl_base_module_t* mtl, 
                                       size_t nprocs,
                                       struct ompi_proc_t** procs, 
                                       struct mca_mtl_base_endpoint_t **mtl_peer_data);

extern int ompi_mtl_portals4_del_procs(struct mca_mtl_base_module_t* mtl, 
                                       size_t nprocs,
                                       struct ompi_proc_t** procs, 
                                       struct mca_mtl_base_endpoint_t **mtl_peer_data);

extern int ompi_mtl_portals4_isend(struct mca_mtl_base_module_t* mtl,
                                   struct ompi_communicator_t* comm,
                                   int dest,
                                   int tag,
                                   struct opal_convertor_t *convertor,
                                   mca_pml_base_send_mode_t mode,
                                   bool blocking,
                                   mca_mtl_request_t *mtl_request);

extern int ompi_mtl_portals4_irecv(struct mca_mtl_base_module_t* mtl,
                                   struct ompi_communicator_t *comm,
                                   int src,
                                   int tag,
                                   struct opal_convertor_t *convertor,
                                   mca_mtl_request_t *mtl_request);

extern int ompi_mtl_portals4_iprobe(struct mca_mtl_base_module_t* mtl,
                                    struct ompi_communicator_t *comm,
                                    int src,
                                    int tag,
                                    int *flag,
                                    struct ompi_status_public_t *status);

extern int ompi_mtl_portals4_cancel(struct mca_mtl_base_module_t* mtl,
                                    mca_mtl_request_t *mtl_request,
                                    int flag);

extern int ompi_mtl_portals4_progress(void);

extern int ompi_mtl_portals4_get_error(int ptl_error);

END_C_DECLS

#endif  /* MTL_PORTALS_H_HAS_BEEN_INCLUDED */
