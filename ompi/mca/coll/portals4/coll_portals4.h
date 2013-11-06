/*
 * Copyright (c) 2013      Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_COLL_PORTALS4_EXPORT_H
#define MCA_COLL_PORTALS4_EXPORT_H

#include "ompi_config.h"

#include <portals4.h>

#include "mpi.h"
#include "opal/mca/mca.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/request/request.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/mtl/portals4/mtl_portals4_endpoint.h"

BEGIN_C_DECLS


struct mca_coll_portals4_component_t {
    mca_coll_base_component_t super;

    /** Network interface handle for matched interface */
    ptl_handle_ni_t ni_h;
    ptl_uid_t uid;
    ptl_pt_index_t pt_idx;
    ptl_pt_index_t finish_pt_idx;
    ptl_handle_eq_t eq_h;
    ptl_handle_me_t barrier_unex_me_h;
    ptl_handle_me_t finish_me_h;
    /** Send MD handle(s).  Use ompi_coll_portals4_get_md() to get the right md */
#if OMPI_PORTALS4_MAX_MD_SIZE < OMPI_PORTALS4_MAX_VA_SIZE
    ptl_handle_md_t *md_hs;
#else
    ptl_handle_md_t md_h;
#endif

    ompi_free_list_t requests; /* request free list for the i collectives */
};
typedef struct mca_coll_portals4_component_t mca_coll_portals4_component_t;
OMPI_MODULE_DECLSPEC extern mca_coll_portals4_component_t mca_coll_portals4_component;

struct mca_coll_portals4_module_t {
    mca_coll_base_module_t super;

    size_t barrier_count;
};
typedef struct mca_coll_portals4_module_t mca_coll_portals4_module_t;
OBJ_CLASS_DECLARATION(mca_coll_portals4_module_t);

struct ompi_coll_portals4_request_t;

/* match/ignore bit manipulation
 *
 * 01234567 0123 4 567 012 34567 01234567 01234567 01234567 01234567 01234567
 *              | |       |
 *  context id  |^| type  | op count
 *              |||       |
 *               +- eager switch
*/

#define COLL_PORTALS4_CID_MASK      0xFFF0000000000000ULL
#define COLL_PORTALS4_OP_COUNT_MASK 0x00001FFFFFFFFFFFULL

#define COLL_PORTALS4_BARRIER 0x01

#define COLL_PORTALS4_SET_BITS(match_bits, contextid, eager, type, op_count) \
    {                                                                   \
        match_bits = contextid;                                         \
        match_bits = (match_bits << 1);                                 \
        match_bits |= (eager & 0x1);                                    \
        match_bits = (match_bits << 6);                                 \
        match_bits |= (type & 0x3F);                                    \
        match_bits = (match_bits << 45);                                \
        match_bits |= (op_count & 0x1FFFFFFFFFFF);                      \
    }

int ompi_coll_portals4_barrier_intra(struct ompi_communicator_t *comm,
                                     mca_coll_base_module_t *module);
int ompi_coll_portals4_ibarrier_intra(struct ompi_communicator_t *comm, 
                                      ompi_request_t ** request,
                                      mca_coll_base_module_t *module);
int ompi_coll_portals4_ibarrier_intra_fini(struct ompi_coll_portals4_request_t *request);


static inline ptl_process_t
ompi_coll_portals4_get_peer(struct ompi_communicator_t *comm, int rank)
{
    ompi_proc_t *proc = ompi_comm_peer_lookup(comm, rank);
    return *((ptl_process_t*) proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PORTALS4]);
}

static inline int
ompi_coll_portals4_get_nchildren(int cube_dim, int hibit, int rank, int size)
{
    int guess = cube_dim - (hibit + 1);
    if ((rank | (1 << (cube_dim - 1))) >= size) guess--;
    if (guess < 0) return 0;
    return guess;
}

/*
 * See note in mtl/portals4/mtl_portals4.h for why this exists.
 */
static inline void
ompi_coll_portals4_get_md(const void *ptr, ptl_handle_md_t *md_h, void **base_ptr)
{
#if OMPI_PORTALS4_MAX_MD_SIZE < OMPI_PORTALS4_MAX_VA_SIZE
    int mask = (1ULL << (OMPI_PORTALS4_MAX_VA_SIZE - OMPI_PORTALS4_MAX_MD_SIZE + 1)) - 1;
    int which = (((uintptr_t) ptr) >> (OMPI_PORTALS4_MAX_MD_SIZE - 1)) & mask;
    *md_h = mca_coll_portals4_component.md_hs[which];
    *base_ptr = (void*) (which * (1ULL << (OMPI_PORTALS4_MAX_MD_SIZE - 1)));
#else
    *md_h = mca_coll_portals4_component.md_h;
    *base_ptr = 0;
#endif
}


static inline int
ompi_coll_portals4_get_num_mds(void)
{
#if OMPI_PORTALS4_MAX_MD_SIZE < OMPI_PORTALS4_MAX_VA_SIZE
    return (1 << (OMPI_PORTALS4_MAX_VA_SIZE - OMPI_PORTALS4_MAX_MD_SIZE + 1));
#else
    return 1;
#endif
}


END_C_DECLS

#endif /* MCA_COLL_PORTALS4_EXPORT_H */
