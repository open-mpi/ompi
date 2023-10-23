/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2023 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2017 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2006-2017 University of Houston.  All rights reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc. All rights reserved.
 * Copyright (c) 2010-2012 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2011-2013 Inria.  All rights reserved.
 * Copyright (c) 2011-2013 Universite Bordeaux 1
 * Copyright (c) 2012-2018 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2014-2015 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016-2017 IBM Corporation. All rights reserved.
 * Copyright (c) 2018-2022 Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2023      Advanced Micro Devices, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_COMMUNICATOR_H
#define OMPI_COMMUNICATOR_H

#include "ompi_config.h"
#include "opal/class/opal_object.h"
#include "opal/class/opal_hash_table.h"
#include "opal/util/info_subscriber.h"
#include "ompi/errhandler/errhandler.h"
#include "opal/mca/threads/mutex.h"
#include "ompi/communicator/comm_request.h"
#include "ompi/mca/coll/base/coll_tags.h"

#include "mpi.h"
#include "ompi/group/group.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/info/info.h"
#include "ompi/proc/proc.h"

#include "opal/util/printf.h"

BEGIN_C_DECLS

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_communicator_t);

#define OMPI_COMM_INTER        0x00000001
#define OMPI_COMM_NAMEISSET    0x00000002
#define OMPI_COMM_INTRINSIC    0x00000004
#define OMPI_COMM_DYNAMIC      0x00000008
#define OMPI_COMM_ISFREED      0x00000010
#define OMPI_COMM_INVALID      0x00000020
#define OMPI_COMM_DISJOINT_SET 0x00000040
#define OMPI_COMM_DISJOINT     0x00000080
#define OMPI_COMM_CART         0x00000100
#define OMPI_COMM_GRAPH        0x00000200
#define OMPI_COMM_DIST_GRAPH   0x00000400
#define OMPI_COMM_PML_ADDED    0x00001000
#define OMPI_COMM_EXTRA_RETAIN 0x00004000
#define OMPI_COMM_MAPBY_NODE   0x00008000
#define OMPI_COMM_GLOBAL_INDEX 0x00010000

/* some utility #defines */
#define OMPI_COMM_IS_INTER(comm) ((comm)->c_flags & OMPI_COMM_INTER)
#define OMPI_COMM_IS_INTRA(comm) (!((comm)->c_flags & OMPI_COMM_INTER))
#define OMPI_COMM_IS_CART(comm) ((comm)->c_flags & OMPI_COMM_CART)
#define OMPI_COMM_IS_GRAPH(comm) ((comm)->c_flags & OMPI_COMM_GRAPH)
#define OMPI_COMM_IS_DIST_GRAPH(comm) ((comm)->c_flags & OMPI_COMM_DIST_GRAPH)
#define OMPI_COMM_IS_INTRINSIC(comm) ((comm)->c_flags & OMPI_COMM_INTRINSIC)
#define OMPI_COMM_IS_FREED(comm) ((comm)->c_flags & OMPI_COMM_ISFREED)
#define OMPI_COMM_IS_DYNAMIC(comm) ((comm)->c_flags & OMPI_COMM_DYNAMIC)
#define OMPI_COMM_IS_INVALID(comm) ((comm)->c_flags & OMPI_COMM_INVALID)
#define OMPI_COMM_IS_DISJOINT_SET(comm) ((comm)->c_flags & OMPI_COMM_DISJOINT_SET)
#define OMPI_COMM_IS_DISJOINT(comm) ((comm)->c_flags & OMPI_COMM_DISJOINT)
#define OMPI_COMM_IS_PML_ADDED(comm) ((comm)->c_flags & OMPI_COMM_PML_ADDED)
#define OMPI_COMM_IS_EXTRA_RETAIN(comm) ((comm)->c_flags & OMPI_COMM_EXTRA_RETAIN)
#define OMPI_COMM_IS_TOPO(comm) (OMPI_COMM_IS_CART((comm)) || \
                                 OMPI_COMM_IS_GRAPH((comm)) || \
                                 OMPI_COMM_IS_DIST_GRAPH((comm)))
#define OMPI_COMM_IS_MAPBY_NODE(comm) ((comm)->c_flags & OMPI_COMM_MAPBY_NODE)
#define OMPI_COMM_IS_GLOBAL_INDEX(comm) ((comm)->c_flags & OMPI_COMM_GLOBAL_INDEX)

#define OMPI_COMM_SET_DYNAMIC(comm) ((comm)->c_flags |= OMPI_COMM_DYNAMIC)
#define OMPI_COMM_SET_INVALID(comm) ((comm)->c_flags |= OMPI_COMM_INVALID)

#define OMPI_COMM_SET_PML_ADDED(comm) ((comm)->c_flags |= OMPI_COMM_PML_ADDED)
#define OMPI_COMM_SET_EXTRA_RETAIN(comm) ((comm)->c_flags |= OMPI_COMM_EXTRA_RETAIN)
#define OMPI_COMM_SET_MAPBY_NODE(comm) ((comm)->c_flags |= OMPI_COMM_MAPBY_NODE)

#define OMPI_COMM_ASSERT_NO_ANY_TAG     0x00000001
#define OMPI_COMM_ASSERT_NO_ANY_SOURCE  0x00000002
#define OMPI_COMM_ASSERT_EXACT_LENGTH   0x00000004
#define OMPI_COMM_ASSERT_ALLOW_OVERTAKE 0x00000008
#define OMPI_COMM_ASSERT_LAZY_BARRIER   0x00000010
#define OMPI_COMM_ASSERT_ACTIVE_POLL    0x00000020

#define OMPI_COMM_CHECK_ASSERT(comm, flag) !!((comm)->c_assertions & flag)
#define OMPI_COMM_CHECK_ASSERT_NO_ANY_TAG(comm)     OMPI_COMM_CHECK_ASSERT(comm, OMPI_COMM_ASSERT_NO_ANY_TAG)
#define OMPI_COMM_CHECK_ASSERT_NO_ANY_SOURCE(comm)  OMPI_COMM_CHECK_ASSERT(comm, OMPI_COMM_ASSERT_NO_ANY_SOURCE)
#define OMPI_COMM_CHECK_ASSERT_EXACT_LENGTH(comm)   OMPI_COMM_CHECK_ASSERT(comm, OMPI_COMM_ASSERT_EXACT_LENGTH)
#define OMPI_COMM_CHECK_ASSERT_ALLOW_OVERTAKE(comm) OMPI_COMM_CHECK_ASSERT(comm, OMPI_COMM_ASSERT_ALLOW_OVERTAKE)
#define OMPI_COMM_CHECK_ASSERT_LAZY_BARRIER(comm) OMPI_COMM_CHECK_ASSERT(comm, OMPI_COMM_ASSERT_LAZY_BARRIER)
#define OMPI_COMM_CHECK_ASSERT_ACTIVE_POLL(comm) OMPI_COMM_CHECK_ASSERT(comm, OMPI_COMM_ASSERT_ACTIVE_POLL)

/**
 * Modes required for acquiring the new comm-id.
 * The first (INTER/INTRA) indicates whether the
 * input comm was an inter/intra-comm, the second
 * whether the new communicator will be an inter/intra
 * comm
 */
#define OMPI_COMM_CID_INTRA        0x00000020
#define OMPI_COMM_CID_INTER        0x00000040
#define OMPI_COMM_CID_INTRA_BRIDGE 0x00000080
#define OMPI_COMM_CID_INTRA_PMIX   0x00000100
#define OMPI_COMM_CID_GROUP        0x00000200
#define OMPI_COMM_CID_GROUP_NEW    0x00000400
#if OPAL_ENABLE_FT_MPI
#define OMPI_COMM_CID_INTRA_FT        0x00000800
#define OMPI_COMM_CID_INTER_FT        0x00001000
#define OMPI_COMM_CID_INTRA_PMIX_FT   0x00002000
#endif /* OPAL_ENABLE_FT_MPI */

/**
 * The block of CIDs allocated for MPI_COMM_WORLD
 * and other communicators
 */
#define OMPI_COMM_BLOCK_WORLD      16
#define OMPI_COMM_BLOCK_OTHERS     8

/* A macro comparing two CIDs */
#define OMPI_COMM_CID_IS_LOWER(comm1,comm2) ( ((comm1)->c_index < (comm2)->c_index)? 1:0)

OMPI_DECLSPEC extern opal_hash_table_t ompi_comm_hash;
OMPI_DECLSPEC extern opal_pointer_array_t ompi_mpi_communicators;
OMPI_DECLSPEC extern opal_pointer_array_t ompi_comm_f_to_c_table;

struct ompi_comm_extended_cid_t {
    uint64_t  cid_base;
    union {
        uint64_t u64;
	uint8_t  u8[8];
    } cid_sub;
};
typedef struct ompi_comm_extended_cid_t ompi_comm_extended_cid_t;

struct ompi_comm_extended_cid_block_t {
    ompi_comm_extended_cid_t block_cid;
    /** can be used to get a unique string tag for pmix context creation */
    uint64_t                 block_nexttag;
    uint8_t                  block_nextsub;
    uint8_t                  block_level;
};
typedef struct ompi_comm_extended_cid_block_t ompi_comm_extended_cid_block_t;

static inline void ompi_comm_extended_cid_block_initialize (ompi_comm_extended_cid_block_t *block, uint64_t cid_base,
							    uint64_t cid_sub, uint8_t block_level)
{
    block->block_cid.cid_base = cid_base;
    block->block_cid.cid_sub.u64 = cid_sub;
    block->block_level = block_level;
    block->block_nextsub = 0;
    block->block_nexttag = 0;
}

static inline bool ompi_comm_extended_cid_block_available (ompi_comm_extended_cid_block_t *block)
{
    return (4 > block->block_level && 0xff > block->block_nextsub);
}

static inline char *ompi_comm_extended_cid_get_unique_tag (ompi_comm_extended_cid_block_t *block, int tag,
                                                           int leader)
{
    char *id;

    /* create a unique ID for this */
    if (-1 == tag) {
        opal_asprintf (&id, "ALL:%" PRIx64 "-%" PRIx64 "-%" PRIx64, block->block_cid.cid_base,
                       block->block_cid.cid_sub.u64, ++block->block_nexttag);
    } else {
        opal_asprintf (&id, "GROUP:%" PRIx64 "-%" PRIx64 "-%d-%d", block->block_cid.cid_base,
                       block->block_cid.cid_sub.u64, tag, leader);
    }

    return id;
}

/**
 * Create a new sub-block from an existing block
 *
 * @param[in]  block       block
 * @param[out] new_block   new CID block
 * @param[in]  use_current use the current CID of the existing block as the base
 *
 * This function creates a new CID block from an existing block. The use_current flag
 * can be used to indicate that the new block should use the existing CID. This can
 * be used to assign the first CID in a new block.
 */
static inline int ompi_comm_extended_cid_block_new (ompi_comm_extended_cid_block_t *block,
						    ompi_comm_extended_cid_block_t *new_block,
                                                    bool use_current)
{
    if (!ompi_comm_extended_cid_block_available (block)) {
	/* a new block is needed */
	return OMPI_ERR_OUT_OF_RESOURCE;
    }

    new_block->block_cid = block->block_cid;
    if (!use_current) {
        new_block->block_cid.cid_sub.u8[3 - block->block_level] = ++block->block_nextsub;
    }

    new_block->block_level = block->block_level + 1;
    new_block->block_nextsub = 0;

    return OMPI_SUCCESS;
}

struct ompi_comm_cid_t {
    opal_object_t            super;
    ompi_group_t             cid_group;
    ompi_comm_extended_cid_t cid_value;
    uint8_t                  cid_sublevel;
};
typedef struct ompi_comm_cid_t ompi_comm_cid_t;

OBJ_CLASS_DECLARATION(ompi_comm_cid_t);

#if OPAL_ENABLE_FT_MPI
/**
 * This array holds the number of time each id has been used. In the case where a communicator
 * is revoked, this reference count acts as an epoch, and prevents us from revoking newly created
 * communicators that use an id that is similar to others communicators that are still being revoked.
 */
OMPI_DECLSPEC extern opal_pointer_array_t ompi_mpi_comm_epoch;

/*
 * Callback function that should be called when there is a fault.
 *
 * This callback function will be used anytime (other than during finalize) the
 * runtime or BTLs detects and handles a process failure. The function is called
 * once per communicator that possess the failed process, and per process failure.
 *
 * @param[in] comm the communicator to which the failed process belongs
 * @param[in] rank the rank of the failed process in that communicator
 * @param[in] remote is true iff rank is a remote process
 */
typedef void (ompi_comm_rank_failure_callback_t)(struct ompi_communicator_t *comm, int rank, bool remote);

OMPI_DECLSPEC extern ompi_comm_rank_failure_callback_t *ompi_rank_failure_cbfunc;
#endif  /* OPAL_ENABLE_FT_MPI */

struct ompi_communicator_t {
    opal_infosubscriber_t      super;
    opal_mutex_t               c_lock; /* mutex for name and potentially
                                          attributes */
    char                      *c_name;
    ompi_comm_extended_cid_t      c_contextid;
    ompi_comm_extended_cid_block_t c_contextidb;
    uint32_t                      c_index;
    int                           c_my_rank;
    uint32_t                      c_flags; /* flags, e.g. intercomm,
                                              topology, etc. */
    uint32_t                      c_assertions; /* info assertions */
#if OPAL_ENABLE_FT_MPI
    uint32_t c_epoch;  /* Identifier used to differentiate between two communicators
                          using the same c_contextid (not at the same time, obviously) */
#endif
    /* Non-blocking collective tag. These tags might be shared between
     * all non-blocking collective modules (to avoid message collision
     * between them in the case where multiple outstanding non-blocking
     * collective coexists using multiple backends).
     */
    opal_atomic_int32_t c_nbc_tag;

    /**< inscribing cube dimension */
    int c_cube_dim;

    /* index in Fortran <-> C translation array */
    int c_f_to_c_index;

    ompi_group_t        *c_local_group;
    ompi_group_t       *c_remote_group;

    struct ompi_communicator_t *c_local_comm; /* a duplicate of the
                                                 local communicator in
                                                 case the comm is an
                                                 inter-comm*/

    /* Attributes */
    struct opal_hash_table_t       *c_keyhash;

    /* Standard information about the selected topology module (or NULL
       if this is not a cart, graph or dist graph communicator) */
    struct mca_topo_base_module_t* c_topo;

#ifdef OMPI_WANT_PERUSE
    /*
     * Place holder for the PERUSE events.
     */
    struct ompi_peruse_handle_t** c_peruse_handles;
#endif

    /* Error handling.  This field does not have the "c_" prefix so
       that the OMPI_ERRHDL_* macros can find it, regardless of whether
       it's a comm, window, or file. */
    ompi_errhandler_t                  *error_handler;
    ompi_errhandler_type_t             errhandler_type;

    /* Hooks for PML to hang things */
    struct mca_pml_comm_t  *c_pml_comm;

    /* Hooks for MTL to hang things */
    struct mca_mtl_comm_t  *c_mtl_comm;

    /* Collectives module interface and data */
    mca_coll_base_comm_coll_t *c_coll;

    /* instance that this comm belongs to */
    ompi_instance_t* instance;

#if OPAL_ENABLE_FT_MPI
    /** agreement caching info for topology and previous returned decisions */
    opal_object_t           *agreement_specific;
    /** num_acked - OMPI_Comm_ack_failed */
    int                      num_acked;
    /** MPI_ANY_SOURCE Failed Group Offset - OMPI_Comm_failure_get_acked */
    int                      any_source_offset;
    /** Are MPI_ANY_SOURCE operations enabled? - OMPI_Comm_failure_ack */
    bool                     any_source_enabled;
    /** Has this communicator been revoked - OMPI_Comm_revoke() */
    bool                     comm_revoked;
    /** Force errors to collective pt2pt operations? */
    bool                     coll_revoked;
#endif /* OPAL_ENABLE_FT_MPI */
};
typedef struct ompi_communicator_t ompi_communicator_t;

/**
 * Padded struct to maintain back compatibility.
 *
 * The following ompi_predefined_xxx_t structure is used to maintain
 * backwards binary compatibility for MPI applications compiled
 * against one version of OMPI library but dynamically linked at
 * runtime with another.  The issue is between versions the actual
 * structure may change in size (even between debug and optimized
 * compilation -- the structure contents change, and therefore the
 * overall size changes).
 *
 * This is problematic with predefined handles because the storage of
 * the structure ends up being located to an application's BSS.  This
 * causes problems because if one version has the predefined as size X
 * and then the application is dynamically linked with a version that
 * has a size of Y (where X != Y) then the application will
 * unintentionally overrun the memory initially allocated for the
 * structure.
 *
 * The solution we are using below creates a parent structure
 * (ompi_predefined_xxx_t) that contains the base structure
 * (ompi_xxx_t) followed by a character padding that is the size of
 * the total size we choose to preallocate for the structure minus the
 * amount used by the base structure.  In this way, we've normalized
 * the size of each predefined handle across multiple versions and
 * configurations of Open MPI (e.g., MPI_COMM_WORLD will refer to a
 * back-end struct that is X bytes long, even if we change the
 * back-end ompi_communicator_t between version A.B and version C.D in
 * Open MPI).  When we come close to filling up the the padding we can
 * add a pointer at the back end of the base structure to point to an
 * extension of the type.  Or we can just increase the padding and
 * break backwards binary compatibility.
 *
 * The above method was decided after several failed attempts
 * described below.
 *
 * - Original implementation - suffered that the base structure seemed
 *   to always change in size between Open MPI versions and/or
 *   configurations (e.g., optimized vs. debugging build).
 *
 * - Convert all predefined handles to run-time-assigned pointers
 *   (i.e., global variables) - This worked except in cases where an MPI
 *   application wanted to assign the predefined handle value to a
 *   global variable -- we could not guarantee to have the global
 *   variable filled until MPI_INIT was called (recall that MPI
 *   predefined handles must be assignable before MPI_INIT; e.g.,
 *   "MPI_Comm foo = MPI_COMM_WORLD").
 *
 * - union of struct and padding - Similar to current implementation
 *   except using a union for the parent.  This worked except in cases
 *   where the compilers did not support C99 union static initializers.
 *   It would have been a pain to convert a bunch of the code to use
 *   non-static initializers (e.g., MPI datatypes).
 */

/* Define for the preallocated size of the predefined handle.
 * Note that we are using a pointer type as the base memory chunk
 * size so when the bitness changes the size of the handle changes.
 * This is done so we don't end up needing a structure that is
 * incredibly larger than necessary because of the bitness.
 *
 * This padding mechanism works as a (likely) compile time check for when the
 * size of the ompi_communicator_t exceeds the predetermined size of the
 * ompi_predefined_communicator_t. It also allows us to change the size of
 * the ompi_communicator_t without impacting the size of the
 * ompi_predefined_communicator_t structure for some number of additions.
 *
 * Note: we used to define the PAD as a multiple of sizeof(void*).
 * However, this makes a different size PAD, depending on
 * sizeof(void*).  In some cases
 * (https://github.com/open-mpi/ompi/issues/3610), 32 bit builds can
 * run out of space when 64 bit builds are still ok.  So we changed to
 * use just a naked byte size.  As a rule of thumb, however, the size
 * should probably still be a multiple of 8 so that it has the
 * possibility of being nicely aligned.
 *
 * As an example:
 * If the size of ompi_communicator_t is less than the size of the _PAD then
 * the _PAD ensures that the size of the ompi_predefined_communicator_t is
 * whatever size is defined below in the _PAD macro.
 * However, if the size of the ompi_communicator_t grows larger than the _PAD
 * (say by adding a few more function pointers to the structure) then the
 * 'padding' variable will be initialized to a large number often triggering
 * a 'array is too large' compile time error. This signals two things:
 * 1) That the _PAD should be increased.
 * 2) That users need to be made aware of the size change for the
 *    ompi_predefined_communicator_t structure.
 *
 * Q: So you just made a change to communicator structure, do you need to adjust
 * the PREDEFINED_COMMUNICATOR_PAD macro?
 * A: Most likely not, but it would be good to check.
 */
#define PREDEFINED_COMMUNICATOR_PAD 512

struct ompi_predefined_communicator_t {
    struct ompi_communicator_t comm;
    char padding[PREDEFINED_COMMUNICATOR_PAD - sizeof(ompi_communicator_t)];
};
typedef struct ompi_predefined_communicator_t ompi_predefined_communicator_t;

OMPI_DECLSPEC extern ompi_communicator_t *ompi_mpi_comm_parent;
OMPI_DECLSPEC extern ompi_predefined_communicator_t ompi_mpi_comm_world;
OMPI_DECLSPEC extern ompi_predefined_communicator_t ompi_mpi_comm_self;
OMPI_DECLSPEC extern ompi_predefined_communicator_t ompi_mpi_comm_null;

/*
 * These variables are for the MPI F03 bindings (F03 must bind Fortran
 * variables to symbols; it cannot bind Fortran variables to the
 * address of a C variable).
 */
OMPI_DECLSPEC extern ompi_predefined_communicator_t *ompi_mpi_comm_world_addr;
OMPI_DECLSPEC extern ompi_predefined_communicator_t *ompi_mpi_comm_self_addr;
OMPI_DECLSPEC extern ompi_predefined_communicator_t *ompi_mpi_comm_null_addr;


/**
 * Is this a valid communicator?  This is a complicated question.
 * :-)
 *
 * According to MPI-1:5.2.4 (p137):
 *
 * "The predefined constant MPI_COMM_NULL is the value used for
 * invalid communicator handles."
 *
 * Hence, MPI_COMM_NULL is not valid.  However, MPI-2:4.12.4 (p50)
 * clearly states that the MPI_*_C2F and MPI_*_F2C functions
 * should treat MPI_COMM_NULL as a valid communicator -- it
 * distinctly differentiates between "invalid" handles and
 * "MPI_*_NULL" handles.  Some feel that the MPI-1 definition
 * still holds for all other MPI functions; others feel that the
 * MPi-2 definitions trump the MPI-1 definition.  Regardless of
 * who is right, there is ambiguity here.  So we have left
 * ompi_comm_invalid() as originally coded -- per the MPI-1
 * definition, where MPI_COMM_NULL is an invalid communicator.
 * The MPI_Comm_c2f() function, therefore, calls
 * ompi_comm_invalid() but also explicitly checks to see if the
 * handle is MPI_COMM_NULL.
 */
static inline int ompi_comm_invalid (const ompi_communicator_t* comm)
{
    if ((NULL == comm) || (MPI_COMM_NULL == comm) ||
        (OMPI_COMM_IS_FREED(comm)) || (OMPI_COMM_IS_INVALID(comm)) )
        return true;
    else
        return false;
}

/**
 * rank w/in the communicator
 */
static inline int ompi_comm_rank (const ompi_communicator_t* comm)
{
    return comm->c_my_rank;
}

/**
 * size of the communicator
 */
static inline int ompi_comm_size (const ompi_communicator_t* comm)
{
    return comm->c_local_group->grp_proc_count;
}

/**
 * size of the remote group for inter-communicators.
 * returns zero for an intra-communicator
 */
static inline int ompi_comm_remote_size (const ompi_communicator_t* comm)
{
    return (comm->c_flags & OMPI_COMM_INTER ? comm->c_remote_group->grp_proc_count : 0);
}

/**
 * Context ID for the communicator, suitable for passing to
 * ompi_comm_lookup for getting the communicator back
 */
static inline uint32_t ompi_comm_get_local_cid (const ompi_communicator_t* comm)
{
    return comm->c_index;
}

/**
 * Get the extended context ID for the communicator, suitable for passing
 * to ompi_comm_lookup_cid for getting the communicator back
 */
static inline ompi_comm_extended_cid_t ompi_comm_get_extended_cid (const ompi_communicator_t *comm)
{
    return comm->c_contextid;
}

static inline bool ompi_comm_cid_compare (const ompi_communicator_t *comm, const ompi_comm_extended_cid_t cid)
{
    return comm->c_contextid.cid_base == cid.cid_base && comm->c_contextid.cid_sub.u64 == cid.cid_sub.u64;
}

static inline bool ompi_comm_compare_cids (const ompi_communicator_t *comm1, const ompi_communicator_t *comm2)
{
    return comm1->c_contextid.cid_base == comm2->c_contextid.cid_base && comm1->c_contextid.cid_sub.u64 == comm2->c_contextid.cid_sub.u64;
}

/* return pointer to communicator associated with context id cid,
 * No error checking is done*/
static inline ompi_communicator_t *ompi_comm_lookup (const uint32_t c_index)
{
    /* array of pointers to communicators, indexed by context ID */
    return (ompi_communicator_t *) opal_pointer_array_get_item (&ompi_mpi_communicators, c_index);
}

static inline ompi_communicator_t *ompi_comm_lookup_cid (const ompi_comm_extended_cid_t cid)
{
    ompi_communicator_t *comm = NULL;
    (void) opal_hash_table_get_value_ptr (&ompi_comm_hash, &cid, sizeof (cid), (void *) &comm);
    return comm;
}

static inline struct ompi_proc_t* ompi_comm_peer_lookup (const ompi_communicator_t* comm, const int peer_id)
{
#if OPAL_ENABLE_DEBUG
    if(peer_id >= comm->c_remote_group->grp_proc_count) {
        opal_output(0, "ompi_comm_peer_lookup: invalid peer index (%d)", peer_id);
        return (struct ompi_proc_t *) NULL;
    }
#endif
    /*return comm->c_remote_group->grp_proc_pointers[peer_id];*/
    return ompi_group_peer_lookup(comm->c_remote_group,peer_id);
}

static inline bool ompi_comm_instances_same(const ompi_communicator_t *comm1, const ompi_communicator_t *comm2)
{
    return comm1->instance == comm2->instance;
}

#if OPAL_ENABLE_FT_MPI
/*
 * Support for MPI_ANY_SOURCE point-to-point operations
 */
static inline bool ompi_comm_is_any_source_enabled(ompi_communicator_t* comm)
{
    return (comm->any_source_enabled);
}

/*
 * Are collectives still active on this communicator?
 */
static inline bool ompi_comm_coll_revoked(ompi_communicator_t* comm)
{
    return (comm->coll_revoked);
}

/*
 * Has this communicator been revoked?
 */
static inline bool ompi_comm_is_revoked(ompi_communicator_t* comm)
{
    return (comm->comm_revoked);
}

/*
 * Obtain the group of locally known failed processes on comm
 */
OMPI_DECLSPEC int ompi_comm_get_failed_internal(ompi_communicator_t* comm, ompi_group_t** failed_group);

/*
 * Acknowledge failures and re-enable MPI_ANY_SOURCE
 * Related to OMPI_Comm_failure_ack() and OMPI_Comm_failure_get_acked()
 */
OMPI_DECLSPEC int ompi_comm_ack_failed_internal(ompi_communicator_t* comm, int num_to_ack, int *num_acked);

/*
 * Acknowledge failures and re-enable MPI_ANY_SOURCE
 * Related to OMPI_Comm_failure_ack() and OMPI_Comm_failure_get_acked()
 */
OMPI_DECLSPEC int ompi_comm_failure_ack_internal(ompi_communicator_t* comm);

/*
 * Return the acknowledged group of failures
 * Related to OMPI_Comm_failure_ack() and OMPI_Comm_failure_get_acked()
 */
OMPI_DECLSPEC int ompi_comm_failure_get_acked_internal(ompi_communicator_t* comm, ompi_group_t **group );

/*
 * Revoke the communicator
 */
OMPI_DECLSPEC int ompi_comm_revoke_internal(ompi_communicator_t* comm);

/*
 * Shrink the communicator
 */
OMPI_DECLSPEC int ompi_comm_shrink_internal(ompi_communicator_t* comm, ompi_communicator_t** newcomm);

/*
 * Shrink the communicator
 */
OMPI_DECLSPEC int ompi_comm_ishrink_internal(ompi_communicator_t* comm, ompi_communicator_t** newcomm, ompi_request_t** request);

/*
 * Check if the process is active
 */
OMPI_DECLSPEC bool ompi_comm_is_proc_active(ompi_communicator_t *comm, int peer_id, bool remote);

/*
 * Register a new process failure
 */
OMPI_DECLSPEC int ompi_comm_set_rank_failed(ompi_communicator_t *comm, int peer_id, bool remote);

/*
 * Returns true if point-to-point communications with the target process
 * are supported (this means if the process is a valid peer, if the
 * communicator is not revoked and if the peer is not already marked as
 * a dead process).
 */
static inline bool ompi_comm_iface_p2p_check_proc(ompi_communicator_t *comm, int peer_id, int *err)
{
    if( OPAL_UNLIKELY(ompi_comm_is_revoked(comm)) ) {
        *err = MPI_ERR_REVOKED;
        return false;
    }
    if( OPAL_UNLIKELY(!ompi_comm_is_proc_active(comm, peer_id, OMPI_COMM_IS_INTER(comm))) ) {
        /* make sure to progress the revoke engine */
        opal_progress();
        *err = MPI_ERR_PROC_FAILED;
        return false;
    }
    return true;
}

/*
 * Returns true if the communicator is locally valid for collective communications
 */
static inline bool ompi_comm_iface_coll_check(ompi_communicator_t *comm, int *err)
{
    if( OPAL_UNLIKELY(ompi_comm_is_revoked(comm)) ) {
        *err = MPI_ERR_REVOKED;
        return false;
    }
    if( OPAL_UNLIKELY(ompi_comm_coll_revoked(comm)) ) {
        /* make sure to progress the revoke engine */
        opal_progress();
        *err = MPI_ERR_PROC_FAILED;
        return false;
    }
    return true;
}

/*
 * Returns true if the communicator can be used by traditional MPI functions
 * as an underlying communicator to create new communicators. The only
 * communicator creation function that can help if this function returns
 * false is MPI_Comm_shrink.
 */
static inline bool ompi_comm_iface_create_check(ompi_communicator_t *comm, int *err)
{
    return ompi_comm_iface_coll_check(comm, err);
}

/*
 * Communicator creation support collectives
 * - Agreement style allreduce
 */
int ompi_comm_allreduce_intra_ft( int *inbuf, int* outbuf,
                                  int count, struct ompi_op_t *op,
                                  ompi_communicator_t *comm,
                                  ompi_communicator_t *bridgecomm,
                                  void* local_leader,
                                  void* remote_ledaer,
                                  int send_first, char *tag, int iter );
int ompi_comm_allreduce_inter_ft( int *inbuf, int* outbuf,
                                  int count, struct ompi_op_t *op,
                                  ompi_communicator_t *comm,
                                  ompi_communicator_t *bridgecomm,
                                  void* local_leader,
                                  void* remote_ledaer,
                                  int send_first, char *tag, int iter );
int ompi_comm_allreduce_intra_pmix_ft( int *inbuf, int* outbuf,
                                  int count, struct ompi_op_t *op,
                                  ompi_communicator_t *comm,
                                  ompi_communicator_t *bridgecomm,
                                  void* local_leader,
                                  void* remote_ledaer,
                                  int send_first, char *tag, int iter );

/*
 * Reliable Bcast infrastructure
 */
OMPI_DECLSPEC int ompi_comm_rbcast_register_params(void);
OMPI_DECLSPEC int ompi_comm_rbcast_init(void);
OMPI_DECLSPEC int ompi_comm_rbcast_finalize(void);

typedef struct ompi_comm_rbcast_message_t {
    uint32_t cid;
    uint32_t epoch;
    uint8_t  type;
} ompi_comm_rbcast_message_t;

typedef int (*ompi_comm_rbcast_cb_t)(ompi_communicator_t* comm, ompi_comm_rbcast_message_t* msg);

OMPI_DECLSPEC int ompi_comm_rbcast_register_cb_type(ompi_comm_rbcast_cb_t callback);
OMPI_DECLSPEC int ompi_comm_rbcast_unregister_cb_type(int type);

extern int (*ompi_comm_rbcast)(ompi_communicator_t* comm, ompi_comm_rbcast_message_t* msg, size_t size);
int ompi_comm_rbcast_send_msg(
        ompi_proc_t* proc,
        ompi_comm_rbcast_message_t* msg,
        size_t size);

/*
 * Setup/Shutdown 'failure propagator' handler
 */
OMPI_DECLSPEC int ompi_comm_failure_propagator_register_params(void);
OMPI_DECLSPEC int ompi_comm_failure_propagator_init(void);
OMPI_DECLSPEC int ompi_comm_failure_propagator_finalize(void);
OMPI_DECLSPEC int ompi_comm_failure_propagate(ompi_communicator_t* comm, ompi_proc_t* proc, int state);

/*
 * Setup/Shutdown 'failure detector' handler
 */
OMPI_DECLSPEC int ompi_comm_failure_detector_register_params(void);
OMPI_DECLSPEC int ompi_comm_failure_detector_init(void);
OMPI_DECLSPEC int ompi_comm_failure_detector_start(void);
OMPI_DECLSPEC int ompi_comm_failure_detector_finalize(void);

/*
 * Setup/Shutdown 'revoke' handler
 */
OMPI_DECLSPEC int ompi_comm_revoke_init(void);
OMPI_DECLSPEC int ompi_comm_revoke_finalize(void);

#endif /* OPAL_ENABLE_FT_MPI */

static inline bool ompi_comm_peer_invalid (const ompi_communicator_t* comm, const int peer_id)
{
    if(peer_id < 0 || peer_id >= comm->c_remote_group->grp_proc_count) {
        return true;
    }
    return false;
}

char *ompi_comm_print_cid (const ompi_communicator_t *comm);

/**
 * @brief Initialize the communicator subsystem as well as MPI_COMM_NULL.
 */
int ompi_comm_init(void);

/**
 * Initialise MPI_COMM_WORLD and MPI_COMM_SELF
 */
int ompi_comm_init_mpi3 (void);

/**
 * extract the local group from a communicator
 */
OMPI_DECLSPEC int ompi_comm_group (ompi_communicator_t *comm, ompi_group_t **group);

/**
 * create a communicator based on a group
 */
int ompi_comm_create (ompi_communicator_t* comm, ompi_group_t *group,
                      ompi_communicator_t** newcomm);

int ompi_comm_create_w_info (ompi_communicator_t *comm, ompi_group_t *group, opal_info_t *info,
                             ompi_communicator_t **newcomm);


/**
 * Non-collective create communicator based on a group
 */
int ompi_comm_create_group (ompi_communicator_t *comm, ompi_group_t *group, int tag,
                            ompi_communicator_t **newcomm);

/**
 * Non-collective create communicator based on a group with no base communicator
 */
int ompi_comm_create_from_group (ompi_group_t *group, const char *tag, opal_info_t *info,
                                 ompi_errhandler_t *errhandler, ompi_communicator_t **newcomm);

/**
 * create an intercommunicator
 */
int ompi_intercomm_create (ompi_communicator_t *local_comm, int local_leader, ompi_communicator_t *bridge_comm,
                           int remote_leader, int tag, ompi_communicator_t **newintercomm);

/**
 * Non-collective create intercommunicator based on a group with no base communicator
 */
int ompi_intercomm_create_from_groups (ompi_group_t *local_group, int local_leader,
                                       ompi_group_t *remote_group, int remote_leader, const char *tag,
                                       opal_info_t *info, ompi_errhandler_t *errhandler,
                                       ompi_communicator_t **newintercomm);

/**
 * Take an almost complete communicator and reserve the CID as well
 * as activate it (initialize the collective and the topologies).
 */
int ompi_comm_enable(ompi_communicator_t *old_comm,
                     ompi_communicator_t *new_comm,
                     int new_rank,
                     int num_procs,
                     ompi_proc_t** topo_procs);

/**
 * Back end of MPI_DIST_GRAPH_CREATE_ADJACENT
 */
int ompi_topo_dist_graph_create_adjacent(ompi_communicator_t *old_comm,
                                         int indegree, int sources[],
                                         int sourceweights[], int outdegree,
                                         int destinations[], int destweights[],
                                         MPI_Info info, int reorder,
                                         MPI_Comm *comm_dist_graph);

/**
 * split a communicator based on color and key. Parameters
 * are identical to the MPI-counterpart of the function.
 *
 * @param comm: input communicator
 * @param color
 * @param key
 *
 * @
 */
OMPI_DECLSPEC int ompi_comm_split (ompi_communicator_t *comm, int color, int key,
                                   ompi_communicator_t** newcomm, bool pass_on_topo);

/**
 * split a communicator based on color and key. Parameters
 * are identical to the MPI-counterpart of the function.
 * Similar to \see ompi_comm_split with an additional info parameter.
 *
 * @param comm: input communicator
 * @param color
 * @param key
 *
 * @
 */
OMPI_DECLSPEC int ompi_comm_split_with_info( ompi_communicator_t* comm, int color, int key,
                                             opal_info_t *info,
                                             ompi_communicator_t **newcomm, bool pass_on_topo );

/**
 * split a communicator based on type and key. Parameters
 * are identical to the MPI-counterpart of the function.
 *
 * @param comm: input communicator
 * @param color
 * @param key
 *
 * @
 */
OMPI_DECLSPEC int ompi_comm_split_type(ompi_communicator_t *comm,
                                       int split_type, int key,
                                       struct opal_info_t *info,
                                       ompi_communicator_t** newcomm);

/**
 * Set newcomm's disjoint flags based on oldcomm if provided. In the case where oldcomm
 * is disjoint, the function will short circuit and set newcomm to be disjoint.
 * Otherwise, the function will carry out a collective communication on all processes
 * in newcomm. Therefore this function should only be called **after** the collectives
 * modules are initialized on newcomm.
 *
 * @param newcomm: new communicator
 * @param oldcomm: parent communictator or NULL
 *
 */
OMPI_DECLSPEC int ompi_comm_set_disjointness(ompi_communicator_t *newcomm, ompi_communicator_t *oldcomm);

/**
 * dup a communicator. Parameter are identical to the MPI-counterpart
 * of the function. It has been extracted, since we need to be able
 * to dup a communicator internally as well.
 *
 * @param comm:      input communicator
 * @param newcomm:   the new communicator or MPI_COMM_NULL if any error is detected.
 */
OMPI_DECLSPEC int ompi_comm_dup (ompi_communicator_t *comm, ompi_communicator_t **newcomm);

/**
 * dup a communicator (non-blocking). Parameter are identical to the MPI-counterpart
 * of the function. It has been extracted, since we need to be able
 * to dup a communicator internally as well.
 *
 * @param comm:      input communicator
 * @param newcomm:   the new communicator or MPI_COMM_NULL if any error is detected.
 */
OMPI_DECLSPEC int ompi_comm_idup (ompi_communicator_t *comm, ompi_communicator_t **newcomm, ompi_request_t **request);

/**
 * dup a communicator with info. Parameter are identical to the MPI-counterpart
 * of the function. It has been extracted, since we need to be able
 * to dup a communicator internally as well.
 *
 * @param comm:      input communicator
 * @param newcomm:   the new communicator or MPI_COMM_NULL if any error is detected.
 */
OMPI_DECLSPEC int ompi_comm_dup_with_info (ompi_communicator_t *comm, opal_info_t *info, ompi_communicator_t **newcomm);

/**
 * dup a communicator (non-blocking) with info.
 * of the function. It has been extracted, since we need to be able
 * to dup a communicator internally as well.
 *
 * @param comm:      input communicator
 * @param newcomm:   the new communicator or MPI_COMM_NULL if any error is detected.
 */
OMPI_DECLSPEC int ompi_comm_idup_with_info (ompi_communicator_t *comm, opal_info_t *info, ompi_communicator_t **newcomm, ompi_request_t **req);

/**
 * compare two communicators.
 *
 * @param comm1,comm2: input communicators
 *
 */
int ompi_comm_compare(ompi_communicator_t *comm1, ompi_communicator_t *comm2, int *result);

/**
 * free a communicator
 */
OMPI_DECLSPEC int ompi_comm_free (ompi_communicator_t **comm);

/**
 * allocate new communicator ID
 * @param newcomm:    pointer to the new communicator
 * @param oldcomm:    original comm
 * @param bridgecomm: bridge comm for intercomm_create
 * @param mode: combination of input
 *              OMPI_COMM_CID_INTRA:        intra-comm
 *              OMPI_COMM_CID_INTER:        inter-comm
 *              OMPI_COMM_CID_GROUP:        only decide CID within the ompi_group_t
 *                                          associated with the communicator. arg0
 *                                          must point to an int which will be used
 *                                          as the pml tag for communication.
 *              OMPI_COMM_CID_INTRA_BRIDGE: 2 intracomms connected by
 *                                          a bridge comm. arg0 and arg1 must point
 *                                          to integers representing the local and
 *                                          remote leader ranks. the remote leader rank
 *                                          is a rank in the bridgecomm.
 *              OMPI_COMM_CID_INTRA_PMIX:   2 intracomms, leaders talk
 *                                          through PMIx. arg0 must point to an integer
 *                                          representing the local leader rank. arg1
 *                                          must point to a string representing the
 *                                          port of the remote leader.
 * @param send_first: to avoid a potential deadlock for
 *                    the OOB version.
 * This routine has to be thread safe in the final version.
 */
OMPI_DECLSPEC int ompi_comm_nextcid (ompi_communicator_t *newcomm, ompi_communicator_t *comm,
                                     ompi_communicator_t *bridgecomm, const void *arg0, const void *arg1,
                                     bool send_first, int mode);

/**
 * allocate new communicator ID (non-blocking)
 * @param newcomm:    pointer to the new communicator
 * @param oldcomm:    original comm
 * @param bridgecomm: bridge comm for intercomm_create
 * @param mode: combination of input
 *              OMPI_COMM_CID_INTRA:        intra-comm
 *              OMPI_COMM_CID_INTER:        inter-comm
 * This routine has to be thread safe in the final version.
 */
OMPI_DECLSPEC int ompi_comm_nextcid_nb (ompi_communicator_t *newcomm, ompi_communicator_t *comm,
                                        ompi_communicator_t *bridgecomm, const void *arg0, const void *arg1,
                                        bool send_first, int mode, ompi_request_t **req);

/**
 * This is THE routine, where all the communicator stuff
 * is really set.
 *
 * @param[out] newcomm            new ompi communicator object
 * @param[in]  oldcomm            old communicator
 * @param[in]  local_size         size of local_ranks array
 * @param[in]  local_ranks        local ranks (not used if local_group != NULL)
 * @param[in]  remote_size        size of remote_ranks array
 * @param[in]  remote_ranks       remote ranks (intercomm) (not used if remote_group != NULL)
 * @param[in]  attr               attributes (can be NULL)
 * @param[in]  errh               error handler
 * @param[in]  copy_topocomponent whether to copy the topology
 * @param[in]  local_group        local process group (may be NULL if local_ranks array supplied)
 * @param[in]  remote_group       remote process group (may be NULL)
 * @param[in]  flags              flags to control the behavior of ompi_comm_set_nb
 */
OMPI_DECLSPEC int ompi_comm_set ( ompi_communicator_t** newcomm,
                                  ompi_communicator_t* oldcomm,
                                  int local_size,
                                  int *local_ranks,
                                  int remote_size,
                                  int *remote_ranks,
                                  opal_hash_table_t *attr,
                                  ompi_errhandler_t *errh,
                                  ompi_group_t *local_group,
                                  ompi_group_t *remote_group,
                                  uint32_t flags);

/**
 * @brief Don't duplicate the local communicator. just reference it directly. This
 *        flag passes ownership to the new communicator.
 */
#define OMPI_COMM_SET_FLAG_LOCAL_COMM_NODUP 0x00000001

/**
 * @brief Copy the topology from the old communicator
 */
#define OMPI_COMM_SET_FLAG_COPY_TOPOLOGY    0x00000002

/**
 * This is THE routine, where all the communicator stuff
 * is really set. Non-blocking version.
 *
 * @param[out] newcomm            new ompi communicator object
 * @param[in]  oldcomm            old communicator
 * @param[in]  local_size         size of local_ranks array
 * @param[in]  local_ranks        local ranks (not used if local_group != NULL)
 * @param[in]  remote_size        size of remote_ranks array
 * @param[in]  remote_ranks       remote ranks (intercomm) (not used if remote_group != NULL)
 * @param[in]  attr               attributes (can be NULL)
 * @param[in]  errh               error handler
 * @param[in]  copy_topocomponent whether to copy the topology
 * @param[in]  local_group        local process group (may be NULL if local_ranks array supplied)
 * @param[in]  remote_group       remote process group (may be NULL)
 * @param[in]  flags              flags to control the behavior of ompi_comm_set_nb
 * @param[out] req                ompi_request_t object for tracking completion
 */
OMPI_DECLSPEC int ompi_comm_set_nb ( ompi_communicator_t **ncomm,
                                     ompi_communicator_t *oldcomm,
                                     int local_size,
                                     int *local_ranks,
                                     int remote_size,
                                     int *remote_ranks,
                                     opal_hash_table_t *attr,
                                     ompi_errhandler_t *errh,
                                     ompi_group_t *local_group,
                                     ompi_group_t *remote_group,
                                     uint32_t flags,
                                     ompi_request_t **req);

/**
 * This is a routine determining whether the local or the
 * remote group will be first in the new intra-comm.
 * Just used from within MPI_Intercomm_merge.
 */
int ompi_comm_determine_first ( ompi_communicator_t *intercomm,
                                int high );

/**
 * This is a routine determining whether the local or the
 * remote group will be first in the new intra-comm.
 * It does not communicate to exchange the "high" values; used in Agree
 */
int ompi_comm_determine_first_auto ( ompi_communicator_t* intercomm );

OMPI_DECLSPEC int ompi_comm_activate (ompi_communicator_t **newcomm, ompi_communicator_t *comm,
                                      ompi_communicator_t *bridgecomm, const void *arg0,
                                      const void *arg1, bool send_first, int mode);

/**
 * Non-blocking variant of comm_activate.
 *
 * @param[inout] newcomm    New communicator
 * @param[in]    comm       Parent communicator
 * @param[in]    bridgecomm Bridge communicator (used for PMIX and bridge modes)
 * @param[in]    arg0       Mode argument 0
 * @param[in]    arg1       Mode argument 1
 * @param[in]    send_first Send first from this process (PMIX mode only)
 * @param[in]    mode       Collective mode
 * @param[out]   req        New request object to track this operation
 */
OMPI_DECLSPEC int ompi_comm_activate_nb (ompi_communicator_t **newcomm, ompi_communicator_t *comm,
                                         ompi_communicator_t *bridgecomm, const void *arg0,
                                         const void *arg1, bool send_first, int mode, ompi_request_t **req);

/**
 * a simple function to dump the structure
 */
int ompi_comm_dump ( ompi_communicator_t *comm );

/* setting name */
int ompi_comm_set_name (ompi_communicator_t *comm, const char *name );

/* global variable to save the number od dynamic communicators */
extern int ompi_comm_num_dyncomm;


/* check whether any of the processes has requested support for
   MPI_THREAD_MULTIPLE. Note, that this produces global
   information across MPI_COMM_WORLD, in contrary to the local
   flag ompi_mpi_thread_provided
*/
OMPI_DECLSPEC int ompi_comm_cid_init ( void );


void ompi_comm_assert_subscribe (ompi_communicator_t *comm, int32_t assert_flag);

END_C_DECLS

#endif /* OMPI_COMMUNICATOR_H */

