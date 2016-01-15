/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2015      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#if !defined(OSC_RDMA_SYNC_H)
#define OSC_RDMA_SYNC_H

#include "osc_rdma_types.h"
#include "opal/class/opal_object.h"
#include "opal/threads/threads.h"

/**
 * @brief synchronization types
 */
enum ompi_osc_rdma_sync_type_t {
    /** default value */
    OMPI_OSC_RDMA_SYNC_TYPE_NONE,
    /** lock access epoch */
    OMPI_OSC_RDMA_SYNC_TYPE_LOCK,
    /** fence access epoch */
    OMPI_OSC_RDMA_SYNC_TYPE_FENCE,
    /* post-start-complete-wait access epoch */
    OMPI_OSC_RDMA_SYNC_TYPE_PSCW,
};
typedef enum ompi_osc_rdma_sync_type_t ompi_osc_rdma_sync_type_t;

struct ompi_osc_rdma_module_t;

/**
 * @brief synchronization object
 *
 * This structure holds information about an access epoch.
 */
struct ompi_osc_rdma_sync_t {
    opal_object_t super;

    /** osc rdma module */
    struct ompi_osc_rdma_module_t *module;

    /** synchronization type */
    ompi_osc_rdma_sync_type_t type;

    /** synchronization data */
    union {
        /** lock specific synchronization data */
        struct {
            /** lock target rank (-1 for all) */
            int target;

            /** lock type: MPI_LOCK_SHARED, MPI_LOCK_EXCLUSIVE */
            int16_t type;

            /** assert specified at lock acquire time. at this time Open MPI
             * only uses 5-bits for asserts. if this number goes over 16 this
             * will need to be changed to accomodate. */
            int16_t assert;
        } lock;

        /** post/start/complete/wait specific synchronization data */
        struct {
            /** group passed to ompi_osc_rdma_start */
            ompi_group_t *group;
        } pscw;
    } sync;

    /** array of peers for this sync */
    union {
        /** multiple peers (lock all, pscw, fence) */
	struct ompi_osc_rdma_peer_t **peers;
        /** single peer (targeted lock) */
	struct ompi_osc_rdma_peer_t *peer;
    } peer_list;

    /** number of peers */
    int num_peers;

    /** communication has started on this epoch */
    bool epoch_active;

    /** outstanding rdma operations on epoch */
    osc_rdma_counter_t outstanding_rdma;

    /** aggregated operations in this epoch */
    opal_list_t aggregations;

    /** lock to protect sync structure members */
    opal_mutex_t lock;
};
typedef struct ompi_osc_rdma_sync_t ompi_osc_rdma_sync_t;

OBJ_CLASS_DECLARATION(ompi_osc_rdma_sync_t);

/**
 * @brief allocate a new synchronization object
 *
 * @param[in] module   osc rdma module
 *
 * @returns NULL on failure
 * @returns a new synchronization object on success
 */
ompi_osc_rdma_sync_t *ompi_osc_rdma_sync_allocate (struct ompi_osc_rdma_module_t *module);

/**
 * @brief release a synchronization object
 *
 * @param[in] rdma_sync   synchronization object allocated by ompi_osc_rdma_sync_allocate()
 */
void ompi_osc_rdma_sync_return (ompi_osc_rdma_sync_t *rdma_sync);

/**
 * Check if the target is part of a PSCW access epoch
 *
 * @param[in] module   osc rdma module
 * @param[in] target   target rank
 * @param[out] peer    peer object
 *
 * @returns false if the window is not in a PSCW access epoch or the peer is not
 *          in the group passed to MPI_Win_start
 * @returns true otherwise
 *
 * This functions verifies the target is part of an active PSCW access epoch.
 */
bool ompi_osc_rdma_sync_pscw_peer (struct ompi_osc_rdma_module_t *module, int target, struct ompi_osc_rdma_peer_t **peer);

/**
 * @brief increment the outstanding rdma operation counter (atomic)
 *
 * @param[in] rdma_sync         osc rdma synchronization object
 */
static inline void ompi_osc_rdma_sync_rdma_inc (ompi_osc_rdma_sync_t *rdma_sync)
{
    ompi_osc_rdma_counter_add (&rdma_sync->outstanding_rdma, 1);

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_INFO, "inc: there are %ld outstanding rdma operations",
                     (unsigned long) rdma_sync->outstanding_rdma);
}

/**
 * @brief decrement the outstanding rdma operation counter (atomic)
 *
 * @param[in] rdma_sync         osc rdma synchronization object
 */
static inline void ompi_osc_rdma_sync_rdma_dec (ompi_osc_rdma_sync_t *rdma_sync)
{
    ompi_osc_rdma_counter_add (&rdma_sync->outstanding_rdma, -1);

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_INFO, "dec: there are %ld outstanding rdma operations",
                     (unsigned long) rdma_sync->outstanding_rdma);
}

#endif /* OSC_RDMA_SYNC_H */
