/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2022 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2014-2020 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2024-2026 NVIDIA CORPORATION. All rights reserved.
 * Copyright (c) 2025      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#ifndef MCA_COLL_BASE_UTIL_EXPORT_H
#define MCA_COLL_BASE_UTIL_EXPORT_H

#include "ompi_config.h"

#include "opal/sys/atomic.h"

#include "mpi.h"
#include "ompi/mca/mca.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/request/request.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/op/op.h"
#include "ompi/mca/pml/pml.h"

BEGIN_C_DECLS

struct ompi_coll_base_nbc_request_t;

/**
 * Drop the reference counts this request holds on user objects.  Installed
 * by the retain helpers, which run from the C bindings, so this is internal
 * bookkeeping independent of the calling language.
 */
typedef void (*ompi_coll_base_nbc_release_fn_t)(struct ompi_coll_base_nbc_request_t *req);

/**
 * Request structure to be returned by non-blocking
 * collective operations.
 *
 * Every collective request handed to the user must be of this type: the
 * language hooks ompi_api_request_t carries are reached by casting an
 * OMPI_REQUEST_COLL request to this.
 */
struct ompi_coll_base_nbc_request_t {
    ompi_api_request_t super;

    /* The component's own dispose hook, displaced so that the release and
     * cleanup hooks can run while the request is still alive. */
    ompi_request_free_fn_t req_dispose;

    ompi_coll_base_nbc_release_fn_t release;

    /* Everything this operation owns.  The variants are mutually
     * exclusive per call, and the hooks above record which one is live.
     * The two-pointer spellings (op, types, objs) deliberately alias: the
     * retain helpers write the typed ones for readability and the release
     * hook reads objs[] generically. */
    union {
        struct {
            ompi_op_t *op;
            ompi_datatype_t *datatype;
        } op;
        struct {
            ompi_datatype_t *stype;
            ompi_datatype_t *rtype;
        } types;
        struct {
            opal_object_t *objs[2];
        } objs;
        /* The *v shapes: retained scalar datatypes plus whatever
         * count/displacement arrays the binding converted. */
        struct {
            opal_object_t *objs[2];
            void *scounts;
            void *sdispls;
            void *rcounts;
            void *rdispls;
        } v;
        /* The *w shapes: no scalar retains, because what is reference
         * counted is the elements of the type vectors.  The language
         * cleanup reuses these same pointers rather than recording a
         * second copy of them. */
        struct {
            void *scounts;
            void *sdispls;
            void *rcounts;
            void *rdispls;
            ompi_datatype_t **stypes;
            ompi_datatype_t **rtypes;
            int scount;
            int rcount;
        } w;
        /* MPIX_Comm_iagree: c_flag is the C-side contribution the binding
         * allocated and handed to the agreement; flag is the user's.
         *
         * MPIX_Comm_ishrink has the same need but hands back an
         * ompi_comm_request_t rather than one of these, so it parks its
         * payload in that request's lang_data instead. */
        struct {
            int *c_flag;
            MPI_Fint *flag;
        } agree;
    } data;
};

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_coll_base_nbc_request_t);

static inline int32_t
ompi_coll_base_nbc_reserve_tags(ompi_communicator_t* comm, int32_t reserve)
{
    int32_t tag, old_tag;
    assert( reserve > 0 );
  reread_tag:  /* In case we fail to atomically update the tag */
    tag = old_tag = comm->c_nbc_tag;
    if ((tag - reserve) < MCA_COLL_BASE_TAG_NONBLOCKING_END) {
        tag = MCA_COLL_BASE_TAG_NONBLOCKING_BASE;
    }
    if( !OPAL_ATOMIC_COMPARE_EXCHANGE_STRONG_32(&comm->c_nbc_tag, &old_tag, tag - reserve) ) {
        goto reread_tag;
    }
    return tag;
}

typedef struct ompi_coll_base_nbc_request_t ompi_coll_base_nbc_request_t;

/*
 * Structure to store an available module
 */
struct mca_coll_base_avail_coll_t {
    opal_list_item_t super;

    int ac_priority;
    mca_coll_base_module_t *ac_module;
    const char * ac_component_name;
};
typedef struct mca_coll_base_avail_coll_t mca_coll_base_avail_coll_t;
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_coll_base_avail_coll_t);

/**
 * A MPI_like function doing a send and a receive simultaneously.
 * Posts a irecv, does a send, then gets irecv completion.
 */
int ompi_coll_base_sendrecv_actual( const void* sendbuf, size_t scount,
                                    ompi_datatype_t* sdatatype,
                                    int dest, int stag,
                                    void* recvbuf, size_t rcount,
                                    ompi_datatype_t* rdatatype,
                                    int source, int rtag,
                                    struct ompi_communicator_t* comm,
                                    ompi_status_public_t* status );


/**
 * A wrapper around ompi_coll_base_sendrecv_actual, with an optimized
 * path for self-directed send/recv.
 */
static inline int
ompi_coll_base_sendrecv( void* sendbuf, size_t scount, ompi_datatype_t* sdatatype,
                          int dest, int stag,
                          void* recvbuf, size_t rcount, ompi_datatype_t* rdatatype,
                          int source, int rtag,
                          struct ompi_communicator_t* comm,
                          ompi_status_public_t* status, int myid )
{
    if ((dest == source) && (source == myid)) {
        return (int) ompi_datatype_sndrcv(sendbuf, (int32_t) scount, sdatatype,
                                          recvbuf, (int32_t) rcount, rdatatype);
    }
    return ompi_coll_base_sendrecv_actual (sendbuf, scount, sdatatype,
                                           dest, stag,
                                           recvbuf, rcount, rdatatype,
                                           source, rtag, comm, status);
}

/**
 * ompi_mirror_perm: Returns mirror permutation of nbits low-order bits
 *                   of x [*].
 * [*] Warren Jr., Henry S. Hacker's Delight (2ed). 2013.
 *     Chapter 7. Rearranging Bits and Bytes.
 */
unsigned int ompi_mirror_perm(unsigned int x, int nbits);

/*
 * ompi_rounddown: Rounds a number down to nearest multiple.
 *     rounddown(10,4) = 8, rounddown(6,3) = 6, rounddown(14,3) = 12
 */
int ompi_rounddown(int num, int factor);

/**
 * If necessary, retain op and store it in the
 * request object, which should be of type ompi_coll_base_nbc_request_t
 * (will be cast internally).
 */
int ompi_coll_base_retain_op( ompi_request_t *request,
                              ompi_op_t *op,
                              ompi_datatype_t *type);

/**
 * If necessary, retain the datatypes and store them in the
 * request object, which should be of type ompi_coll_base_nbc_request_t
 * (will be cast internally).
 */
int ompi_coll_base_retain_datatypes( ompi_request_t *request,
                                     ompi_datatype_t *stype,
                                     ompi_datatype_t *rtype);

/**
 * If necessary, retain the datatypes and store them in the
 * request object, which should be of type ompi_coll_base_nbc_request_t
 * (will be cast internally).
 */
int ompi_coll_base_retain_datatypes_w( ompi_request_t *request,
                                       ompi_datatype_t * const stypes[],
                                       ompi_datatype_t * const rtypes[],
                                       bool use_topo);

/**
 * Install the language cleanup hook: the temporaries recorded in the
 * request's data union are freed at disposal, after the internal release
 * hook has dropped the reference counts that point into them.
 *
 * The request must be of type ompi_coll_base_nbc_request_t.
 */
/**
 * Install the language hooks on a collective request.
 *
 * ompi_api_request_set_lang_hooks() plus the one thing a collective needs on
 * top of it: each component supplies its own req_free, so that has to be
 * displaced by a disposal that reaches the cleanup hook.  Use this rather
 * than the generic setter on anything of type OMPI_REQUEST_COLL.
 */
OMPI_DECLSPEC int ompi_coll_base_nbc_set_lang_hooks(ompi_request_t *request,
                                                    ompi_api_request_hook_fn_t complete,
                                                    ompi_api_request_hook_fn_t cleanup);

/**
 * Record the count/displacement arrays a binding converted for a *v
 * collective and install the cleanup that frees them at disposal.  The
 * whole family of shapes (recvcounts only, one side's counts and
 * displacements, or all four) differs only in which arguments are
 * non-NULL.
 *
 * Only the non-NULL arguments are recorded, so NULL never takes an array
 * back from an earlier call and the two sides can be handed over
 * independently.
 *
 * This takes ownership unconditionally: a request that cannot carry the
 * hook has no disposal to defer to, so the arrays are freed here instead.
 * Either way the caller must not free or touch them again.
 */
int ompi_coll_base_nbc_set_cleanup_v(ompi_request_t *request,
                                     void *scounts, void *sdispls,
                                     void *rcounts, void *rdispls);

/**
 * Same for a *w collective, which additionally owns the converted datatype
 * vectors.  Those are the same pointers ompi_coll_base_retain_datatypes_w()
 * recorded in order to release their elements, so a non-NULL stypes or
 * rtypes here is simply the binding claiming the right to free the array
 * once the release hook has run.
 */
int ompi_coll_base_nbc_set_cleanup_w(ompi_request_t *request,
                                     void *scounts, void *sdispls,
                                     void *rcounts, void *rdispls,
                                     ompi_datatype_t **stypes,
                                     ompi_datatype_t **rtypes);

/* File reading function */
int ompi_coll_base_file_getnext_long(FILE *fptr, int *fileline, long* val);
int ompi_coll_base_file_getnext_size_t(FILE *fptr, int *fileline, size_t* val);
int ompi_coll_base_file_getnext_string(FILE *fptr, int *fileline, char** val);
/* peek at the next valid token to see if it begins with the expected value. If yes
 * eat the value, otherwise put it back into the file.
 */
int ompi_coll_base_file_peek_next_char_is(FILE *fptr, int *fileline, int expected);
int ompi_coll_base_file_peek_next_char_isdigit(FILE *fptr);

/* Miscellaneous function */
const char* mca_coll_base_colltype_to_str(int collid);
int mca_coll_base_name_to_colltype(const char* name);

END_C_DECLS
#endif /* MCA_COLL_BASE_UTIL_EXPORT_H */
