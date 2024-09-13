/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2022 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2014-2020 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.
 *
 * Copyright (c) 2024      NVIDIA CORPORATION. All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "ompi/mca/topo/base/base.h"
#include "ompi/mca/pml/pml.h"
#include "coll_base_util.h"
#include "coll_base_functions.h"
#include <ctype.h>

int ompi_coll_base_sendrecv_actual( const void* sendbuf, size_t scount,
                                    ompi_datatype_t* sdatatype,
                                    int dest, int stag,
                                    void* recvbuf, size_t rcount,
                                    ompi_datatype_t* rdatatype,
                                    int source, int rtag,
                                    struct ompi_communicator_t* comm,
                                    ompi_status_public_t* status )

{ /* post receive first, then send, then wait... should be fast (I hope) */
    int err, line = 0;
    size_t rtypesize, stypesize;
    ompi_request_t *req = MPI_REQUEST_NULL;
    ompi_status_public_t rstatus;

    /* post new irecv */
    ompi_datatype_type_size(rdatatype, &rtypesize);
    err = MCA_PML_CALL(irecv( recvbuf, rcount, rdatatype, source, rtag,
                              comm, &req));
    if (err != MPI_SUCCESS) { line = __LINE__; goto error_handler; }

    /* send data to children */
    ompi_datatype_type_size(sdatatype, &stypesize);
    err = MCA_PML_CALL(send( sendbuf, scount, sdatatype, dest, stag,
                             MCA_PML_BASE_SEND_STANDARD, comm));
    if (err != MPI_SUCCESS) { line = __LINE__; goto error_handler; }

    err = ompi_request_wait( &req, &rstatus);
    if (err != MPI_SUCCESS) { line = __LINE__; goto error_handler; }

    if (MPI_STATUS_IGNORE != status) {
        *status = rstatus;
    }

    return (MPI_SUCCESS);

 error_handler:
    /* Error discovered during the posting of the irecv or send,
     * and no status is available.
     */
    OPAL_OUTPUT ((ompi_coll_base_framework.framework_output, "%s:%d: Error %d occurred\n",
                  __FILE__, line, err));
    (void)line;  // silence compiler warning
    if (MPI_STATUS_IGNORE != status) {
        status->MPI_ERROR = err;
    }
    if( MPI_REQUEST_NULL != req ) {
#if OPAL_ENABLE_FT_MPI
        if( MPI_ERR_PROC_FAILED == req->req_status.MPI_ERROR
         || MPI_ERR_PROC_FAILED_PENDING == req->req_status.MPI_ERROR
         || MPI_ERR_REVOKED == req->req_status.MPI_ERROR ) {
            /* We cannot just 'free' and forget, as the PML/BTLS would still
             * be updating the request buffer after we return from the MPI
             * call!
             * For other errors that do not have a well defined post-error
             * behavior, calling the cancel/wait could deadlock, so we just
             * free, as this is the best that can be done in this case. */
            ompi_request_cancel(req);
            ompi_request_wait(&req, MPI_STATUS_IGNORE);
            if( MPI_ERR_PROC_FAILED_PENDING == err ) {
                err = MPI_ERR_PROC_FAILED;
            }
        } else /* this 'else' intentionally spills outside the ifdef */
#endif /* OPAL_ENABLE_FT_MPI */
        ompi_request_free(&req);
    }
    return (err);
}

/*
 * ompi_mirror_perm: Returns mirror permutation of nbits low-order bits
 *                   of x [*].
 * [*] Warren Jr., Henry S. Hacker's Delight (2ed). 2013.
 *     Chapter 7. Rearranging Bits and Bytes.
 */
unsigned int ompi_mirror_perm(unsigned int x, int nbits)
{
    x = (((x & 0xaaaaaaaa) >> 1) | ((x & 0x55555555) << 1));
    x = (((x & 0xcccccccc) >> 2) | ((x & 0x33333333) << 2));
    x = (((x & 0xf0f0f0f0) >> 4) | ((x & 0x0f0f0f0f) << 4));
    x = (((x & 0xff00ff00) >> 8) | ((x & 0x00ff00ff) << 8));
    x = ((x >> 16) | (x << 16));
    return x >> (sizeof(x) * CHAR_BIT - nbits);
}

/*
 * ompi_rounddown: Rounds a number down to nearest multiple.
 *     rounddown(10,4) = 8, rounddown(6,3) = 6, rounddown(14,3) = 12
 */
int ompi_rounddown(int num, int factor)
{
    num /= factor;
    return num * factor;    /* floor(num / factor) * factor */
}

/**
 * Release all objects and arrays stored into the nbc_request.
 * The release_arrays are temporary memory to stored the values
 * converted from Fortran, and should disappear in same time as the
 * request itself.
 */
static void
release_objs_callback(struct ompi_coll_base_nbc_request_t *request)
{
    if (NULL != request->data.refcounted.objs.objs[0]) {
        OBJ_RELEASE(request->data.refcounted.objs.objs[0]);
        request->data.refcounted.objs.objs[0] = NULL;
    }
    if (NULL != request->data.refcounted.objs.objs[1]) {
        OBJ_RELEASE(request->data.refcounted.objs.objs[1]);
        request->data.refcounted.objs.objs[1] = NULL;
    }
    for(int i = 0; i < OMPI_REQ_NB_RELEASE_ARRAYS; i++ ) {
        if (NULL == request->data.release_arrays[i]) {
            break;
        }
        free(request->data.release_arrays[i]);
        request->data.release_arrays[i] = NULL;
    }
}

static int complete_objs_callback(struct ompi_request_t *req) {
    struct ompi_coll_base_nbc_request_t *request = (ompi_coll_base_nbc_request_t *)req;
    int rc = OMPI_SUCCESS;
    assert (NULL != request);
    if (NULL != request->cb.req_complete_cb) {
        rc = request->cb.req_complete_cb(request->req_complete_cb_data);
    }
    release_objs_callback(request);
    return rc;
}

static int free_objs_callback(struct ompi_request_t **rptr) {
    struct ompi_coll_base_nbc_request_t *request = *(ompi_coll_base_nbc_request_t **)rptr;
    int rc = OMPI_SUCCESS;
    if (NULL != request->cb.req_free) {
        rc = request->cb.req_free(rptr);
    }
    release_objs_callback(request);
    return rc;
}

int ompi_coll_base_retain_op( ompi_request_t *req, ompi_op_t *op,
                              ompi_datatype_t *type) {
    ompi_coll_base_nbc_request_t *request = (ompi_coll_base_nbc_request_t *)req;
    bool retain = false;
    if (REQUEST_COMPLETE(req)) {
        return OMPI_SUCCESS;
    }
    if (!ompi_op_is_intrinsic(op)) {
        OBJ_RETAIN(op);
        request->data.refcounted.op.op = op;
        retain = true;
    }
    if (!ompi_datatype_is_predefined(type)) {
        OBJ_RETAIN(type);
        request->data.refcounted.op.datatype = type;
        retain = true;
    }
    if (OPAL_UNLIKELY(retain)) {
        /* We need to consider two cases :
         * - non blocking collectives:
         *     the objects can be released when MPI_Wait() completes
         *     and we use the req_complete_cb callback
         * - persistent non blocking collectives:
         *     the objects can only be released when the request is freed
         *     (e.g. MPI_Request_free() completes) and we use req_free callback
         */
        if (req->req_persistent) {
            request->cb.req_free = req->req_free;
            req->req_free = free_objs_callback;
        } else {
            request->cb.req_complete_cb = req->req_complete_cb;
            request->req_complete_cb_data = req->req_complete_cb_data;
            req->req_complete_cb = complete_objs_callback;
            req->req_complete_cb_data = request;
        }
    }
    return OMPI_SUCCESS;
}

int ompi_coll_base_retain_datatypes( ompi_request_t *req, ompi_datatype_t *stype,
                                     ompi_datatype_t *rtype) {
    ompi_coll_base_nbc_request_t *request = (ompi_coll_base_nbc_request_t *)req;
    bool retain = false;
    if (REQUEST_COMPLETE(req)) {
        return OMPI_SUCCESS;
    }
    if (NULL != stype && !ompi_datatype_is_predefined(stype)) {
        OBJ_RETAIN(stype);
        request->data.refcounted.types.stype = stype;
        retain = true;
    }
    if (NULL != rtype && !ompi_datatype_is_predefined(rtype)) {
        OBJ_RETAIN(rtype);
        request->data.refcounted.types.rtype = rtype;
        retain = true;
    }
    if (OPAL_UNLIKELY(retain)) {
        if (req->req_persistent) {
            request->cb.req_free = req->req_free;
            req->req_free = free_objs_callback;
        } else {
            request->cb.req_complete_cb = req->req_complete_cb;
            request->req_complete_cb_data = req->req_complete_cb_data;
            req->req_complete_cb = complete_objs_callback;
            req->req_complete_cb_data = request;
        }
    }
    return OMPI_SUCCESS;
}

static void release_vecs_callback(ompi_coll_base_nbc_request_t *request)
{
    if (NULL != request->data.refcounted.vecs.stypes) {
        for (int i = 0; i < request->data.refcounted.vecs.scount; i++) {
            if (NULL != request->data.refcounted.vecs.stypes[i] &&
                !ompi_datatype_is_predefined(request->data.refcounted.vecs.stypes[i])) {
                OMPI_DATATYPE_RELEASE_NO_NULLIFY(request->data.refcounted.vecs.stypes[i]);
            }
        }
        request->data.refcounted.vecs.stypes = NULL;
    }
    if (NULL != request->data.refcounted.vecs.rtypes) {
        for (int i = 0; i < request->data.refcounted.vecs.rcount; i++) {
            if (NULL != request->data.refcounted.vecs.rtypes[i] &&
                !ompi_datatype_is_predefined(request->data.refcounted.vecs.rtypes[i])) {
                OMPI_DATATYPE_RELEASE_NO_NULLIFY(request->data.refcounted.vecs.rtypes[i]);
            }
        }
        request->data.refcounted.vecs.rtypes = NULL;
    }
}

static int complete_vecs_callback(struct ompi_request_t *req) {
    ompi_coll_base_nbc_request_t *request = (ompi_coll_base_nbc_request_t *)req;
    int rc = OMPI_SUCCESS;
    assert (NULL != request);
    if (NULL != request->cb.req_complete_cb) {
        rc = request->cb.req_complete_cb(request->req_complete_cb_data);
    }
    release_vecs_callback(request);
    return rc;
}

static int free_vecs_callback(struct ompi_request_t **rptr) {
    struct ompi_coll_base_nbc_request_t *request = *(ompi_coll_base_nbc_request_t **)rptr;
    int rc = OMPI_SUCCESS;
    if (NULL != request->cb.req_free) {
        rc = request->cb.req_free(rptr);
    }
    release_vecs_callback(request);
    return rc;
}

int ompi_coll_base_retain_datatypes_w( ompi_request_t *req,
                                       ompi_datatype_t * const stypes[],
                                       ompi_datatype_t * const rtypes[],
                                       bool use_topo)
{
    ompi_coll_base_nbc_request_t *request = (ompi_coll_base_nbc_request_t *)req;
    ompi_communicator_t *comm = request->super.req_mpi_object.comm;
    int scount, rcount;

    if (REQUEST_COMPLETE(req)) {
        return OMPI_SUCCESS;
    }

    if (use_topo && OMPI_COMM_IS_TOPO(comm)) {
        (void)mca_topo_base_neighbor_count (comm, &rcount, &scount);
    } else {
        scount = rcount = OMPI_COMM_IS_INTER(comm)?ompi_comm_remote_size(comm):ompi_comm_size(comm);
    }

    request->data.refcounted.vecs.scount = 0;  /* default value */
    if (NULL != stypes) {
        for (int i = 0; i < scount; i++) {
            if (NULL != stypes[i] && !ompi_datatype_is_predefined(stypes[i])) {
                OBJ_RETAIN(stypes[i]);
                request->data.refcounted.vecs.scount = i;  /* last valid type */
            }
        }
    }
    request->data.refcounted.vecs.rcount = 0;  /* default value */
    if (NULL != rtypes) {
        for (int i = 0; i < rcount; i++) {
            if (NULL != rtypes[i] && !ompi_datatype_is_predefined(rtypes[i])) {
                OBJ_RETAIN(rtypes[i]);
                request->data.refcounted.vecs.rcount = i;  /* last valid type */
            }
        }
    }
    if (OPAL_LIKELY(request->data.refcounted.vecs.scount | request->data.refcounted.vecs.rcount) ) {
        request->data.refcounted.vecs.stypes = (ompi_datatype_t **) stypes;
        request->data.refcounted.vecs.rtypes = (ompi_datatype_t **) rtypes;
        request->data.refcounted.vecs.scount = scount;
        request->data.refcounted.vecs.rcount = rcount;
        if (req->req_persistent) {
            request->cb.req_free = req->req_free;
            req->req_free = free_vecs_callback;
        } else {
            request->cb.req_complete_cb = req->req_complete_cb;
            request->req_complete_cb_data = req->req_complete_cb_data;
            req->req_complete_cb = complete_vecs_callback;
            req->req_complete_cb_data = request;
        }
    }
    return OMPI_SUCCESS;
}

static void nbc_req_constructor(ompi_coll_base_nbc_request_t *req)
{
    req->cb.req_complete_cb = NULL;
    req->req_complete_cb_data = NULL;
    req->data.refcounted.objs.objs[0] = NULL;
    req->data.refcounted.objs.objs[1] = NULL;
    req->data.release_arrays[0] = NULL;
}

OBJ_CLASS_INSTANCE(ompi_coll_base_nbc_request_t, ompi_request_t, nbc_req_constructor, NULL);

/* File reading functions */
static void skiptonewline (FILE *fptr, int *fileline)
{
    char val;
    int rc;

    do {
        rc = fread(&val, 1, 1, fptr);
        if (0 == rc) {
            return;
        }
        if ('\n' == val) {
            (*fileline)++;
            return;
        }
    } while (1);
}

int ompi_coll_base_file_getnext_long(FILE *fptr, int *fileline, long* val)
{
    char trash;
    int rc;

    do {
        rc = fscanf(fptr, "%li", val);
        if (rc == EOF) {
            return -1;
        }
        if (1 == rc) {
            return 0;
        }
        /* in all other cases, skip to the end of the token */
        rc = fread(&trash, sizeof(char), 1, fptr);
        if (rc == EOF) {
            return -1;
        }
        if ('\n' == trash) (*fileline)++;
        if ('#' == trash) {
            skiptonewline (fptr, fileline);
        }
    } while (1);
}

int ompi_coll_base_file_getnext_string(FILE *fptr, int *fileline, char** val)
{
    char trash, token[33];
    int rc;

    *val = NULL;  /* security in case we fail */
    do {
        rc = fscanf(fptr, "%32s", token);
        if (rc == EOF) {
            return -1;
        }
        if (1 == rc) {
            if( '#' == token[0] ) {
                skiptonewline(fptr, fileline);
                continue;
            }
            *val = (char*)malloc(strlen(token) + 1);
            strcpy(*val, token);
            return 0;
        }
        /* in all other cases, skip to the end of the token */
        rc = fread(&trash, sizeof(char), 1, fptr);
        if (rc == EOF) {
            return -1;
        }
        if ('\n' == trash) (*fileline)++;
        if ('#' == trash) {
            skiptonewline (fptr, fileline);
        }
    } while (1);
}

int ompi_coll_base_file_getnext_size_t(FILE *fptr, int *fileline, size_t* val)
{
    char trash;
    int rc;

    do {
        rc = fscanf(fptr, "%" PRIsize_t, val);
        if (rc == EOF) {
            return -1;
        }
        if (1 == rc) {
            return 0;
        }
        /* in all other cases, skip to the end of the token */
        rc = fread(&trash, sizeof(char), 1, fptr);
        if (rc == EOF) {
            return -1;
        }
        if ('\n' == trash) (*fileline)++;
        if ('#' == trash) {
            skiptonewline (fptr, fileline);
        }
    } while (1);
}

int ompi_coll_base_file_peek_next_char_is(FILE *fptr, int *fileline, int expected)
{
    char trash;
    int rc;

    do {
        rc = fread(&trash, sizeof(char), 1, fptr);
        if (0 == rc) {  /* hit the end of the file */
            return -1;
        }
        if ('\n' == trash) {
            (*fileline)++;
            continue;
        }
        if ('#' == trash) {
            skiptonewline (fptr, fileline);
            continue;
        }
        if( trash == expected )
            return 1;  /* return true and eat the char */
        if( isblank(trash) )  /* skip all spaces if that's not what we were looking for */
            continue;
        if( 0 != fseek(fptr, -1, SEEK_CUR) )
            return -1;
        return 0;
    } while (1);
}

/**
 * return non-zero if the next non-space to read on the current line is a digit.
 * otherwise return 0.
 */
int ompi_coll_base_file_peek_next_char_isdigit(FILE *fptr)
{
    do {
        int next = fgetc(fptr);

        if ((' ' == next) || ('\t' == next)) {
            continue; /* discard space and tab. keep everything else */
        }

        ungetc(next, fptr); /* put the char back into the stream */

        return isdigit(next); /* report back whether or not next is a digit */

    } while (1);
}

/**
 * There are certainly simpler implementation for this function when performance
 * is not a critical point. But, as this function is used during the collective
 * configuration, and we can do this configurations once for each communicator,
 * I would rather have a more complex but faster implementation.
 * The approach here is to search for the largest common denominators, to create
 * something similar to a dichotomic search.
 */
int mca_coll_base_name_to_colltype(const char* name)
{
    if( 'n' == name[0] ) {
        if( 0 == strncmp(name, "neighbor_all", 12) ) {
            if( 't' != name[12] ) {
                if( 0 == strncmp(name+12, "gather", 6) ) {
                    if('\0' == name[18]) return NEIGHBOR_ALLGATHER;
                    if( 'v' == name[18]) return NEIGHBOR_ALLGATHERV;
                }
            } else {
                if( 0 == strncmp(name+12, "toall", 5) ) {
                    if( '\0' == name[17] ) return NEIGHBOR_ALLTOALL;
                    if( 'v' == name[17] ) return NEIGHBOR_ALLTOALLV;
                    if( 'w' == name[17] ) return NEIGHBOR_ALLTOALLW;
                }
            }
        }
        return -1;
    }
    if( 'a' == name[0] ) {
        if( 0 != strncmp(name, "all", 3) ) {
            return -1;
        }
        if( 't' != name[3] ) {
            if( 'r' == name[3] ) {
                if( 0 == strcmp(name+3, "reduce") )
                    return ALLREDUCE;
            } else {
                if( 0 == strncmp(name+3, "gather", 6) ) {
                    if( '\0' == name[9] ) return ALLGATHER;
                    if( 'v'  == name[9] ) return ALLGATHERV;
                }
            }
        } else {
            if( 0 == strncmp(name+3, "toall", 5) ) {
                if( '\0' == name[8] ) return ALLTOALL;
                if( 'v' == name[8] ) return ALLTOALLV;
                if( 'w' == name[8] ) return ALLTOALLW;
            }
        }
        return -1;
    }
    if( 'r' > name[0] ) {
        if( 'b' == name[0] ) {
            if( 0 == strcmp(name, "barrier") )
                return BARRIER;
            if( 0 == strcmp(name, "bcast") )
                return BCAST;
        } else if( 'g'== name[0] ) {
            if( 0 == strncmp(name, "gather", 6) ) {
                if( '\0' == name[6] ) return GATHER;
                if( 'v' == name[6] ) return GATHERV;
            }
        }
        if( 0 == strcmp(name, "exscan") )
            return EXSCAN;
        return -1;
    }
    if( 's' > name[0] ) {
        if( 0 == strncmp(name, "reduce", 6) ) {
            if( '\0' == name[6] ) return REDUCE;
            if( '_' == name[6] ) {
                if( 0 == strncmp(name+7, "scatter", 7) ) {
                    if( '\0' == name[14] ) return REDUCESCATTER;
                    if( 0 == strcmp(name+14, "_block") ) return REDUCESCATTERBLOCK;
                }
            }
        }
        return -1;
    }
    if( 0 == strcmp(name, "scan") )
        return SCAN;
    if( 0 == strcmp(name, "scatterv") )
        return SCATTERV;
    if( 0 == strcmp(name, "scatter") )
        return SCATTER;
    return -1;
}

/* conversion table for all COLLTYPE_T values defined in ompi/mca/coll/base/coll_base_functions.h */
static const char* colltype_translation_table[] = {
    [ALLGATHER] = "allgather",
    [ALLGATHERV] = "allgatherv",
    [ALLREDUCE] = "allreduce",
    [ALLTOALL] = "alltoall",
    [ALLTOALLV] = "alltoallv",
    [ALLTOALLW] = "alltoallw",
    [BARRIER] = "barrier",
    [BCAST] = "bcast",
    [EXSCAN] = "exscan",
    [GATHER] = "gather",
    [GATHERV] = "gatherv",
    [REDUCE] = "reduce",
    [REDUCESCATTER] = "reduce_scatter",
    [REDUCESCATTERBLOCK] = "reduce_scatter_block",
    [SCAN] = "scan",
    [SCATTER] = "scatter",
    [SCATTERV] = "scatterv",
    [NEIGHBOR_ALLGATHER] = "neighbor_allgather",
    [NEIGHBOR_ALLGATHERV] = "neighbor_allgatherv",
    [NEIGHBOR_ALLTOALL] = "neighbor_alltoall",
    [NEIGHBOR_ALLTOALLV] = "neighbor_alltoallv",
    [NEIGHBOR_ALLTOALLW] = "neighbor_alltoallw",
    [COLLCOUNT] = NULL
};

const char* mca_coll_base_colltype_to_str(int collid)
{
    if( (collid < 0) || (collid >= COLLCOUNT) ) {
        return NULL;
    }
    return colltype_translation_table[collid];
}
