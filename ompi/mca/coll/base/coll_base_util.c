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
 * Copyright (c) 2024-2026 NVIDIA CORPORATION. All rights reserved.
 * Copyright (c) 2025      Triad National Security, LLC. All rights
 *                         reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
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
#include <string.h>

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
 * Drop the reference counts this request holds on the caller's objects.
 * The retain helpers write the typed spellings of the two-pointer union for
 * readability; this reads them back generically.
 */
static void nbc_release_objs(ompi_coll_base_nbc_request_t *request)
{
    for (int i = 0; i < 2; i++) {
        if (NULL != request->data.objs.objs[i]) {
            OBJ_RELEASE(request->data.objs.objs[i]);
            request->data.objs.objs[i] = NULL;
        }
    }
}

/**
 * Same, for the *w shapes, where what is reference counted is the elements
 * of the type vectors rather than a pair of scalar datatypes.  The vectors
 * themselves are left in place: a language cleanup may still have to free
 * them, and it runs after this.
 */
static void nbc_release_vecs(ompi_coll_base_nbc_request_t *request)
{
    if (NULL != request->data.w.stypes) {
        for (int i = 0; i < request->data.w.scount; i++) {
            if (NULL != request->data.w.stypes[i] &&
                !ompi_datatype_is_predefined(request->data.w.stypes[i])) {
                OMPI_DATATYPE_RELEASE_NO_NULLIFY(request->data.w.stypes[i]);
            }
        }
    }
    if (NULL != request->data.w.rtypes) {
        for (int i = 0; i < request->data.w.rcount; i++) {
            if (NULL != request->data.w.rtypes[i] &&
                !ompi_datatype_is_predefined(request->data.w.rtypes[i])) {
                OMPI_DATATYPE_RELEASE_NO_NULLIFY(request->data.w.rtypes[i]);
            }
        }
    }
    request->data.w.scount = 0;
    request->data.w.rcount = 0;
}

static void nbc_cleanup_v(ompi_api_request_t *req)
{
    ompi_coll_base_nbc_request_t *request = (ompi_coll_base_nbc_request_t *) req;

    free(request->data.v.scounts);
    free(request->data.v.sdispls);
    free(request->data.v.rcounts);
    free(request->data.v.rdispls);
}

static void nbc_cleanup_w(ompi_api_request_t *req)
{
    ompi_coll_base_nbc_request_t *request = (ompi_coll_base_nbc_request_t *) req;

    free(request->data.w.scounts);
    free(request->data.w.sdispls);
    free(request->data.w.rcounts);
    free(request->data.w.rdispls);
    free(request->data.w.stypes);
    free(request->data.w.rtypes);
}

/**
 * Disposal.  Runs the internal release, then the language cleanup, then the
 * component's own dispose.  That order is mandatory: the *w cleanup frees
 * the very array whose elements the release hook just dropped, and the
 * component has to come last because it hands the request back to a free
 * list, after which none of these fields may be read.
 */
static int nbc_dispose_callback(struct ompi_request_t **rptr)
{
    ompi_coll_base_nbc_request_t *request = *(ompi_coll_base_nbc_request_t **) rptr;
    ompi_request_free_fn_t dispose;

    /* Freeing a request with an operation still outstanding is erroneous
     * for a collective, and the temporaries below are still being read by
     * the component.  Refuse before touching any of them.  libnbc and
     * portals4 check this themselves; doing it here makes it uniform. */
    if (!REQUEST_COMPLETE(&request->super.super)) {
        return MPI_ERR_REQUEST;
    }

    if (NULL != request->release) {
        request->release(request);
        request->release = NULL;
    }
    ompi_api_request_dispose(&request->super);

    /* These requests are recycled through component free lists, which do
     * not re-run the constructor, so leave the payload zeroed for the next
     * user rather than relying on each hook to tidy up after itself. */
    memset(&request->data, 0, sizeof(request->data));

    dispose = request->req_dispose;
    request->req_dispose = NULL;
    request->super.super.req_free = dispose;
    if (NULL != dispose) {
        return dispose(rptr);
    }
    *rptr = MPI_REQUEST_NULL;
    return OMPI_SUCCESS;
}

/**
 * Displace the component's dispose hook so the two hooks above get to run
 * while the request is still alive.  Idempotent.
 */
static inline void nbc_hook_disposal(ompi_coll_base_nbc_request_t *request)
{
    if (nbc_dispose_callback != request->super.super.req_free) {
        request->req_dispose = request->super.super.req_free;
        request->super.super.req_free = nbc_dispose_callback;
    }
}

int ompi_coll_base_nbc_set_lang_hooks(ompi_request_t *req,
                                      ompi_api_request_hook_fn_t complete,
                                      ompi_api_request_hook_fn_t cleanup)
{
    int rc = ompi_api_request_set_lang_hooks(req, complete, cleanup);

    if (OMPI_SUCCESS == rc) {
        nbc_hook_disposal((ompi_coll_base_nbc_request_t *) req);
    }
    return rc;
}

int ompi_coll_base_nbc_set_cleanup_v(ompi_request_t *req,
                                     void *scounts, void *sdispls,
                                     void *rcounts, void *rdispls)
{
    ompi_coll_base_nbc_request_t *request = (ompi_coll_base_nbc_request_t *) req;

    if (!ompi_api_request_is(req)) {
        free(scounts);
        free(sdispls);
        free(rcounts);
        free(rdispls);
        return OMPI_SUCCESS;
    }

    if (NULL != scounts) { request->data.v.scounts = scounts; }
    if (NULL != sdispls) { request->data.v.sdispls = sdispls; }
    if (NULL != rcounts) { request->data.v.rcounts = rcounts; }
    if (NULL != rdispls) { request->data.v.rdispls = rdispls; }

    return ompi_coll_base_nbc_set_lang_hooks(req, NULL, nbc_cleanup_v);
}

int ompi_coll_base_nbc_set_cleanup_w(ompi_request_t *req,
                                     void *scounts, void *sdispls,
                                     void *rcounts, void *rdispls,
                                     ompi_datatype_t **stypes,
                                     ompi_datatype_t **rtypes)
{
    ompi_coll_base_nbc_request_t *request = (ompi_coll_base_nbc_request_t *) req;

    if (!ompi_api_request_is(req)) {
        /* ompi_coll_base_retain_datatypes_w() bailed out on the same
         * request, so nothing was retained and the vectors are just
         * memory. */
        free(scounts);
        free(sdispls);
        free(rcounts);
        free(rdispls);
        free(stypes);
        free(rtypes);
        return OMPI_SUCCESS;
    }

    if (NULL != scounts) { request->data.w.scounts = scounts; }
    if (NULL != sdispls) { request->data.w.sdispls = sdispls; }
    if (NULL != rcounts) { request->data.w.rcounts = rcounts; }
    if (NULL != rdispls) { request->data.w.rdispls = rdispls; }
    if (NULL != stypes)  { request->data.w.stypes  = stypes;  }
    if (NULL != rtypes)  { request->data.w.rtypes  = rtypes;  }

    return ompi_coll_base_nbc_set_lang_hooks(req, NULL, nbc_cleanup_w);
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
        request->data.op.op = op;
        retain = true;
    }
    if (!ompi_datatype_is_predefined(type)) {
        OBJ_RETAIN(type);
        request->data.op.datatype = type;
        retain = true;
    }
    if (OPAL_UNLIKELY(retain)) {
        /* The references are dropped at disposal, not at completion: a
         * persistent request reuses them on every MPI_Start, and for a
         * non-persistent one disposal follows completion within the same
         * MPI call anyway. */
        request->release = nbc_release_objs;
        nbc_hook_disposal(request);
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
        request->data.types.stype = stype;
        retain = true;
    }
    if (NULL != rtype && !ompi_datatype_is_predefined(rtype)) {
        OBJ_RETAIN(rtype);
        request->data.types.rtype = rtype;
        retain = true;
    }
    if (OPAL_UNLIKELY(retain)) {
        request->release = nbc_release_objs;
        nbc_hook_disposal(request);
    }
    return OMPI_SUCCESS;
}

int ompi_coll_base_retain_datatypes_w( ompi_request_t *req,
                                       ompi_datatype_t * const stypes[],
                                       ompi_datatype_t * const rtypes[],
                                       bool use_topo)
{
    ompi_coll_base_nbc_request_t *request = (ompi_coll_base_nbc_request_t *)req;
    ompi_communicator_t *comm = request->super.super.req_mpi_object.comm;
    int scount, rcount;

    if (REQUEST_COMPLETE(req)) {
        return OMPI_SUCCESS;
    }

    if (use_topo && OMPI_COMM_IS_TOPO(comm)) {
        (void)mca_topo_base_neighbor_count (comm, &rcount, &scount);
    } else {
        scount = rcount = OMPI_COMM_IS_INTER(comm)?ompi_comm_remote_size(comm):ompi_comm_size(comm);
    }

    bool retain = false;
    if (NULL != stypes) {
        for (int i = 0; i < scount; i++) {
            if (NULL != stypes[i] && !ompi_datatype_is_predefined(stypes[i])) {
                OBJ_RETAIN(stypes[i]);
                retain = true;
            }
        }
    }
    if (NULL != rtypes) {
        for (int i = 0; i < rcount; i++) {
            if (NULL != rtypes[i] && !ompi_datatype_is_predefined(rtypes[i])) {
                OBJ_RETAIN(rtypes[i]);
                retain = true;
            }
        }
    }
    if (OPAL_LIKELY(retain)) {
        /* Record the vectors themselves so the release hook can walk them
         * again at disposal.  A Fortran binding that converted these
         * arrays reuses these same pointers for its cleanup rather than
         * recording a second copy of them. */
        request->data.w.stypes = (ompi_datatype_t **) stypes;
        request->data.w.rtypes = (ompi_datatype_t **) rtypes;
        request->data.w.scount = scount;
        request->data.w.rcount = rcount;
        request->release = nbc_release_vecs;
        nbc_hook_disposal(request);
    }
    return OMPI_SUCCESS;
}

static void nbc_req_constructor(ompi_coll_base_nbc_request_t *req)
{
    req->req_dispose = NULL;
    req->release = NULL;
    memset(&req->data, 0, sizeof(req->data));
}

OBJ_CLASS_INSTANCE(ompi_coll_base_nbc_request_t, ompi_api_request_t, nbc_req_constructor, NULL);

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
