/*
 * Copyright (c) 2010      Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MTL_PORTALS4_H
#define MTL_PORTALS4_H

#include "ompi/mca/mtl/mtl.h"

#include "mtl_portals4_endpoint.h"

BEGIN_C_DECLS

struct mca_mtl_portals4_module_t {
    mca_mtl_base_module_t base;

    ptl_handle_ni_t ptl_ni_h;
    mca_mtl_base_endpoint_t *endpoints;
};
typedef struct mca_mtl_portals4_module_t mca_mtl_portals4_module_t;

extern mca_mtl_portals4_module_t ompi_mtl_portals4;

OMPI_DECLSPEC extern mca_mtl_base_component_2_0_0_t mca_mtl_portals4_component;


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
#define PTL_IS_LONG_MSG(match_bits)             \
    (0 != (PTL_LONG_MSG & match_bits))
#define PTL_IS_READY_MSG(match_bits)            \
    (0 != (PTL_READY_MSG & match_bits))
#define PTL_IS_SYNC_MSG(event)                  \
    (0 != event.hdr_data)

#define PTL_GET_TAG(match_bits) ((int)(match_bits & PTL_TAG_MASK))
#define PTL_GET_SOURCE(match_bits) ((int)((match_bits & PTL_SOURCE_MASK) >> 32))

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

extern int ompi_mtl_portals4_send(struct mca_mtl_base_module_t* mtl,
                                  struct ompi_communicator_t* comm,
                                  int dest,
                                  int tag,
                                  struct opal_convertor_t *convertor,
                                  mca_pml_base_send_mode_t mode);

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

END_C_DECLS

#endif  /* MTL_PORTALS4_H_HAS_BEEN_INCLUDED */
