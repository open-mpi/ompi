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
 * Copyright (c) 2024      NVIDIA CORPORATION. All rights reserved.
 * Copyright (c) 2025      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_COLL_BASE_UTIL_EXPORT_H
#define MCA_COLL_BASE_UTIL_EXPORT_H

#include "ompi_config.h"

#include "mpi.h"
#include "ompi/mca/mca.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/request/request.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/op/op.h"
#include "ompi/mca/pml/pml.h"

BEGIN_C_DECLS

/**
 * The largest array we need to track collective temporary memory. Right now
 * the record is for ialltoallw, for the array of send and receive types,
 * count and displacements.
 */
#define OMPI_REQ_NB_RELEASE_ARRAYS 7

/**
 * Request structure to be returned by non-blocking
 * collective operations.
 */
struct ompi_coll_base_nbc_request_t {
    ompi_request_t super;
    union {
        ompi_request_complete_fn_t req_complete_cb;
        ompi_request_free_fn_t req_free;
    } cb;
    void *req_complete_cb_data;
    struct {
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
            struct {
                ompi_datatype_t * const *stypes;
                ompi_datatype_t * const *rtypes;
                int scount;
                int rcount;
            } vecs;
        } refcounted;
        void* release_arrays[OMPI_REQ_NB_RELEASE_ARRAYS];
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

/**
 * Append an array to a request object to be freed upon completion
 * of the associated operation.
 * The request object must be of type ompi_coll_base_nbc_request_t.
 */
__opal_attribute_always_inline__ static inline int
ompi_coll_base_append_array_to_release(struct ompi_request_t *req, void *array_ptr)
{
    int i, ret = OMPI_SUCCESS;
    struct ompi_coll_base_nbc_request_t *request = (struct ompi_coll_base_nbc_request_t *)req;

    /*
     * important sanity check - doing steps below on a non-libnbc request can lead
     * to difficult to debug memory corruption problems
     */
    assert(request->super.req_type == OMPI_REQUEST_COLL);

    for(i = 0; i < OMPI_REQ_NB_RELEASE_ARRAYS; i++ ) {
        if (NULL == request->data.release_arrays[i]) {
            break;
        }
    }

    if (OMPI_REQ_NB_RELEASE_ARRAYS > i) {
        request->data.release_arrays[i] = array_ptr;
        ++i;
        if (OMPI_REQ_NB_RELEASE_ARRAYS > i) {
            request->data.release_arrays[i] = NULL;
        }
    } else {
        ret = OMPI_ERR_OUT_OF_RESOURCE;
    }

    assert(OMPI_SUCCESS == ret);
    return ret;
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
 * If necessary, set callback to free extra memory regions
 * set in release_arrays.  Not set if a callback is already
 * associated with the request.
 */
int ompi_coll_base_add_release_arrays_cb(ompi_request_t *request);

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
