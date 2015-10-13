/*
 * Copyright (c) 2013-2015 Sandia National Laboratories. All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015      Bull SAS.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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
#include "ompi/constants.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/datatype/ompi_datatype_internal.h"
#include "ompi/op/op.h"
#include "ompi/mca/mca.h"
#include "opal/datatype/opal_convertor.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/request/request.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/mca/mtl/portals4/mtl_portals4_endpoint.h"

#include "ompi/mca/mtl/portals4/mtl_portals4.h"

#define MAXTREEFANOUT 32

BEGIN_C_DECLS

#define COLL_PORTALS4_NO_OP ((ptl_op_t)-1)
extern ptl_op_t ompi_coll_portals4_atomic_op[];

#define COLL_PORTALS4_NO_DTYPE ((ptl_datatype_t)-1)
extern ptl_datatype_t ompi_coll_portals4_atomic_datatype[];

struct mca_coll_portals4_component_t {
    mca_coll_base_component_t super;

    /** Network interface handle for matched interface */
    ptl_handle_ni_t ni_h;
    ptl_uid_t uid;
    ptl_process_t id;
    ptl_pt_index_t pt_idx;
    ptl_pt_index_t finish_pt_idx;
    ptl_handle_eq_t eq_h;
    ptl_handle_me_t unex_me_h;
    ptl_handle_me_t finish_me_h;
    bool ev_link;
    opal_mutex_t lock;
    opal_condition_t cond;
    int nb_links;
    ptl_handle_md_t zero_md_h;
    ptl_handle_md_t data_md_h;
    opal_free_list_t requests; /* request free list for the i collectives */

    ptl_ni_limits_t ni_limits;

    int use_binomial_gather_algorithm;

};
typedef struct mca_coll_portals4_component_t mca_coll_portals4_component_t;
OMPI_MODULE_DECLSPEC extern mca_coll_portals4_component_t mca_coll_portals4_component;


/*
 * Borrowed with thanks from the coll-tuned component, then modified for Portals4.
 */
typedef struct ompi_coll_portals4_tree_t {
    int32_t tree_root;
    int32_t tree_fanout;
    int32_t tree_bmtree;
    int32_t tree_prev;
    int32_t tree_next[MAXTREEFANOUT];
    int32_t tree_nextsize;
    int32_t tree_numdescendants;
} ompi_coll_portals4_tree_t;


struct mca_coll_portals4_module_t {
    mca_coll_base_module_t super;
    size_t coll_count;

    /* record handlers dedicated to fallback if offloaded operations are not supported */
    mca_coll_base_module_reduce_fn_t previous_reduce;
    mca_coll_base_module_t *previous_reduce_module;
    mca_coll_base_module_ireduce_fn_t previous_ireduce;
    mca_coll_base_module_t *previous_ireduce_module;

    mca_coll_base_module_allreduce_fn_t previous_allreduce;
    mca_coll_base_module_t *previous_allreduce_module;
    mca_coll_base_module_iallreduce_fn_t previous_iallreduce;
    mca_coll_base_module_t *previous_iallreduce_module;

    /* binomial tree */
    ompi_coll_portals4_tree_t *cached_in_order_bmtree;
    int                        cached_in_order_bmtree_root;
};
typedef struct mca_coll_portals4_module_t mca_coll_portals4_module_t;
OBJ_CLASS_DECLARATION(mca_coll_portals4_module_t);

struct ompi_coll_portals4_request_t;

#define COLL_PORTALS4_MAX_BW                         4096
#define COLL_PORTALS4_MAX_SEGMENT                    32


/* match/ignore bit manipulation
 *
 * 01234567 01234567 012 3 4 567 012 3 4567 01234567 01234567 01234567 01234567
                        | | |       |      |
 *  context id          |^|^| type  | int  | op count
 *                      |||||       |      |
 *                      |||+--------------- is a RTR message
 *                      |+----------------- is a data ACK message
 */

#define COLL_PORTALS4_CID_MASK      0xFFE0000000000000ULL
#define COLL_PORTALS4_ACK_MASK      0x0010000000000000ULL
#define COLL_PORTALS4_RTR_MASK      0x0008000000000000ULL
#define COLL_PORTALS4_TYPE_MASK     0x0007E00000000000ULL
#define COLL_PORTALS4_INTERNAL_MASK 0x00001F0000000000ULL
#define COLL_PORTALS4_OP_COUNT_MASK 0x000000FFFFFFFFFFULL

#define COLL_PORTALS4_BARRIER       0x01
#define COLL_PORTALS4_BCAST         0x02
#define COLL_PORTALS4_SCATTER       0x03
#define COLL_PORTALS4_GATHER        0x04
#define COLL_PORTALS4_REDUCE        0x05
#define COLL_PORTALS4_ALLREDUCE     0x06

#define PTL_INVALID_RANK ((ptl_rank_t)-1)
#define PTL_FIRST_RANK   ((ptl_rank_t)0)

#define COLL_PORTALS4_SET_BITS(match_bits, contextid, ack, rtr, type, internal, op_count) \
{                                                                   \
    match_bits = contextid;                                         \
    match_bits = (match_bits << 1);                                 \
    match_bits |= (ack & 0x1);                                      \
    match_bits = (match_bits << 1);                                 \
    match_bits |= (rtr & 0x1);                                      \
    match_bits = (match_bits << 6);                                 \
    match_bits |= (type & 0x3F);                                    \
    match_bits = (match_bits << 5);                                 \
    match_bits |= (internal & 0x1F);                                \
    match_bits = (match_bits << 40);                                \
    match_bits |= (op_count & 0xFFFFFFFFFF);                        \
}

int
opal_stderr(const char *msg, const char *file,
        const int line, const int ret);

/*
 * Borrowed with thanks from the coll-tuned component.
 */
#define COLL_PORTALS4_UPDATE_IN_ORDER_BMTREE( OMPI_COMM, PORTALS4_MODULE, ROOT ) \
do {                                                                                         \
    if( !( ((PORTALS4_MODULE)->cached_in_order_bmtree)                                               \
           && ((PORTALS4_MODULE)->cached_in_order_bmtree_root == (ROOT)) ) ) {                       \
        if( (PORTALS4_MODULE)->cached_in_order_bmtree ) { /* destroy previous binomial if defined */ \
            ompi_coll_portals4_destroy_tree( &((PORTALS4_MODULE)->cached_in_order_bmtree) );       \
        }                                                                                    \
        (PORTALS4_MODULE)->cached_in_order_bmtree = ompi_coll_portals4_build_in_order_bmtree( (OMPI_COMM), (ROOT) ); \
        (PORTALS4_MODULE)->cached_in_order_bmtree_root = (ROOT);                                     \
    }                                                                                        \
} while (0)


int ompi_coll_portals4_barrier_intra(struct ompi_communicator_t *comm,
        mca_coll_base_module_t *module);
int ompi_coll_portals4_ibarrier_intra(struct ompi_communicator_t *comm,
        ompi_request_t ** request,
        mca_coll_base_module_t *module);
int ompi_coll_portals4_ibarrier_intra_fini(struct ompi_coll_portals4_request_t *request);

int ompi_coll_portals4_bcast_intra(void *buff, int count,
        struct ompi_datatype_t *datatype, int root,
        struct ompi_communicator_t *comm,mca_coll_base_module_t *module);
int ompi_coll_portals4_ibcast_intra(void *buff, int count,
        struct ompi_datatype_t *datatype, int root,
        struct ompi_communicator_t *comm,
        ompi_request_t **request,
        mca_coll_base_module_t *module);
int ompi_coll_portals4_ibcast_intra_fini(struct ompi_coll_portals4_request_t *request);

int ompi_coll_portals4_reduce_intra(const void *sbuf, void *rbuf, int count,
        MPI_Datatype dtype, MPI_Op op,
        int root,
        struct ompi_communicator_t *comm,
        mca_coll_base_module_t *module);
int ompi_coll_portals4_ireduce_intra(const void* sendbuf, void* recvbuf, int count,
        MPI_Datatype dype, MPI_Op op,
        int root,
        struct ompi_communicator_t *comm,
        ompi_request_t ** ompi_request,
        struct mca_coll_base_module_2_1_0_t *module);
int ompi_coll_portals4_ireduce_intra_fini(struct ompi_coll_portals4_request_t *request);

int ompi_coll_portals4_allreduce_intra(const void* sendbuf, void* recvbuf, int count,
        MPI_Datatype dtype, MPI_Op op,
        struct ompi_communicator_t *comm,
        struct mca_coll_base_module_2_1_0_t *module);
int ompi_coll_portals4_iallreduce_intra(const void* sendbuf, void* recvbuf, int count,
        MPI_Datatype dtype, MPI_Op op,
        struct ompi_communicator_t *comm,
        ompi_request_t ** ompi_request,
        struct mca_coll_base_module_2_1_0_t *module);
int
ompi_coll_portals4_iallreduce_intra_fini(struct ompi_coll_portals4_request_t *request);

int ompi_coll_portals4_gather_intra(const void *sbuf, int scount, struct ompi_datatype_t *sdtype,
                                    void *rbuf, int rcount, struct ompi_datatype_t *rdtype,
                                    int root,
                                    struct ompi_communicator_t *comm,
                                    mca_coll_base_module_t *module);
int ompi_coll_portals4_igather_intra(const void *sbuf, int scount, struct ompi_datatype_t *sdtype,
                                     void *rbuf, int rcount, struct ompi_datatype_t *rdtype,
                                     int root,
                                     struct ompi_communicator_t *comm,
                                     ompi_request_t **request,
                                     mca_coll_base_module_t *module);
int ompi_coll_portals4_igather_intra_fini(struct ompi_coll_portals4_request_t *request);

int ompi_coll_portals4_scatter_intra(const void *sbuf, int scount, struct ompi_datatype_t *sdtype,
                                     void *rbuf, int rcount, struct ompi_datatype_t *rdtype,
                                     int root,
                                     struct ompi_communicator_t *comm,
                                     mca_coll_base_module_t *module);
int ompi_coll_portals4_iscatter_intra(const void *sbuf, int scount, struct ompi_datatype_t *sdtype,
                                      void *rbuf, int rcount, struct ompi_datatype_t *rdtype,
                                      int root,
                                      struct ompi_communicator_t *comm,
                                      ompi_request_t **request,
                                      mca_coll_base_module_t *module);
int ompi_coll_portals4_iscatter_intra_fini(struct ompi_coll_portals4_request_t *request);


static inline ptl_process_t
ompi_coll_portals4_get_peer(struct ompi_communicator_t *comm, int rank)
{
    return ompi_mtl_portals4_get_peer(comm, rank);
}


static inline bool
is_reduce_optimizable(struct ompi_datatype_t *dtype, size_t length, struct ompi_op_t *op,
        ptl_datatype_t *ptl_dtype, ptl_op_t *ptl_op) {

    /* first check the type of operation and
     * map it to the corresponding portals4 one */

    if (!(op->o_flags & OMPI_OP_FLAGS_COMMUTE)) {
        opal_output_verbose(50, ompi_coll_base_framework.framework_output,
                "atomic op %d is not commutative, deactivate the optimization\n",
                op->op_type);
        return false;
    }

    if (!(op->o_flags & OMPI_OP_FLAGS_ASSOC)) {
        opal_output_verbose(50, ompi_coll_base_framework.framework_output,
                "atomic op %d is not float associative, deactivate the optimization\n",
                op->op_type);
        return false;
    }

    if (op->op_type >= OMPI_OP_NUM_OF_TYPES)  {
        opal_output_verbose(50, ompi_coll_base_framework.framework_output,
                "unknown atomic op %d\n",
                op->op_type);
        return false;
    }

    *ptl_op = ompi_coll_portals4_atomic_op[op->op_type];
    if (*ptl_op == COLL_PORTALS4_NO_OP) {
        opal_output_verbose(50, ompi_coll_base_framework.framework_output,
                "unsupported atomic op %d\n",
                op->op_type);
        return false;
    }

    /* then check the data type and map it
     * to the corresponding portals4 one */

    if (!ompi_datatype_is_valid(dtype)) {
        opal_output_verbose(50, ompi_coll_base_framework.framework_output,
                "not a valid datatype %d\n",
                dtype->id);
        return false;
    }

    if (dtype->id >= OMPI_DATATYPE_MPI_MAX_PREDEFINED) {
        opal_output_verbose(50, ompi_coll_base_framework.framework_output,
                "not a valid datatype %d\n",
                dtype->id);
        return false;
    }

    if (length > mca_coll_portals4_component.ni_limits.max_atomic_size) {
        opal_output_verbose(50, ompi_coll_base_framework.framework_output,
                "length (%ld) > ni.max_atomic_size (%ld)\n",
                length, mca_coll_portals4_component.ni_limits.max_atomic_size);
        return false;
    }

    *ptl_dtype = ompi_coll_portals4_atomic_datatype[dtype->id];
    if (*ptl_dtype == COLL_PORTALS4_NO_DTYPE){
        opal_output_verbose(50, ompi_coll_base_framework.framework_output,
                "datatype %d not supported\n",
                dtype->id);
        return false;
    }

    return true;
}


static inline int
get_nchildren(int cube_dim, int hibit, int rank, int size)
{
    int guess = cube_dim - (hibit + 1);

    if ((rank | (1 << (cube_dim - 1))) >= size) {
        guess--;
    }
    if (guess < 0) {
        return 0;
    }

    return guess;
}

static inline
void get_pipeline(ptl_rank_t rank, ptl_rank_t np, ptl_rank_t root,
        ptl_rank_t *prev, ptl_rank_t *next)
{
    *prev = (rank == root) ?
            PTL_INVALID_RANK:
            ((rank == PTL_FIRST_RANK) ? (np - 1) : (rank - 1));
    *next = (rank == (np - 1)) ?
            ((root == PTL_FIRST_RANK) ? PTL_INVALID_RANK : PTL_FIRST_RANK):
            ((rank == (root - 1)) ? PTL_INVALID_RANK : (rank + 1));
    return;
}

#define div(a,b) (((a)+(b)-1) / (b))
#define min(a,b) (((a) < (b)) ? (a) : (b))
#define min_zero(a) (((a) < 0) ? 0 : (a))

static inline
void get_k_ary_tree(const unsigned int k_ary,
        ptl_rank_t rank, ptl_rank_t np, ptl_rank_t root,
        ptl_rank_t *father, ptl_rank_t *children, unsigned int *child_nb) {

    bool should_continue = true;
    unsigned int cnt;
    ptl_rank_t first, last, dist, up, my;

    if ((!father)   ||
        (!children) ||
        (!child_nb)) {
        return;
    }

    /* initialization and checks */
    *father = PTL_INVALID_RANK;
    *child_nb = 0;

    if (!k_ary) {
        return;
    }

    for (cnt = 0 ; cnt < k_ary ; cnt++) {
        children[cnt] = PTL_INVALID_RANK;
    }

    if ((np <= 0)    ||
        (rank < 0)   ||
        (rank >= np) ||
        (root < 0 )  ||
        (root >= np)) {
        return;
    }

    my = (np + rank - root) % np;

    /* start the loop */
    up = PTL_INVALID_RANK;
    first = PTL_FIRST_RANK;
    last = np - 1;

    while (should_continue) {
        if (my == first) {
            first++;
            dist = div(last - first + 1, k_ary);
            should_continue = false;
        }
        else {
            up = first;
            first++;
            dist = div(last - first + 1, k_ary);
            while (my >= (first + dist)) {
                first += dist;
            }
            last = min(first + dist - 1, last);
        }
    }
    *father = (up == PTL_INVALID_RANK) ? PTL_INVALID_RANK : ((up + root) % np);
    *child_nb = min(k_ary, min_zero(last - first + 1));

    for (cnt = 0 ; cnt < *child_nb ; cnt++) {
        children[cnt] = (root +
                first + cnt * dist) % np;
    }

    return;
}


static inline void
ompi_coll_portals4_create_recv_converter (opal_convertor_t *converter,
                                          void *target,
                                          ompi_proc_t *proc,
                                          int count,
                                          ompi_datatype_t *datatype)
{
    /* create converter */
    OBJ_CONSTRUCT(converter, opal_convertor_t);

    /* initialize converter */
    opal_convertor_copy_and_prepare_for_recv(proc->super.proc_convertor,
                                             &datatype->super,
                                             count,
                                             target,
                                             0,
                                             converter);
}

static inline void
ompi_coll_portals4_create_send_converter (opal_convertor_t *converter,
                                          const void *source,
                                          ompi_proc_t *proc,
                                          int count,
                                          ompi_datatype_t *datatype)
{
    OBJ_CONSTRUCT(converter, opal_convertor_t);

    opal_convertor_copy_and_prepare_for_send(proc->super.proc_convertor,
                                             &datatype->super,
                                             count,
                                             source,
                                             0,
                                             converter);
}

END_C_DECLS

#endif /* MCA_COLL_PORTALS4_EXPORT_H */
