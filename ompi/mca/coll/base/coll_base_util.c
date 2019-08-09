/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2016 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2014-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
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
    ompi_request_t *req;
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

static void release_objs_callback(struct ompi_coll_base_nbc_request_t *request) {
    if (NULL != request->data.objs.objs[0]) {
        OBJ_RELEASE(request->data.objs.objs[0]);
        request->data.objs.objs[0] = NULL;
    }
    if (NULL != request->data.objs.objs[1]) {
        OBJ_RELEASE(request->data.objs.objs[1]);
        request->data.objs.objs[1] = NULL;
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
        request->data.op.op = op;
        retain = true;
    }
    if (!ompi_datatype_is_predefined(type)) {
        OBJ_RETAIN(type);
        request->data.op.datatype = type;
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
        request->data.types.stype = stype;
        retain = true;
    }
    if (NULL != rtype && !ompi_datatype_is_predefined(rtype)) {
        OBJ_RETAIN(rtype);
        request->data.types.rtype = rtype;
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

static void release_vecs_callback(ompi_coll_base_nbc_request_t *request) {
    ompi_communicator_t *comm = request->super.req_mpi_object.comm;
    int scount, rcount;
    if (OMPI_COMM_IS_TOPO(comm)) {
        (void)mca_topo_base_neighbor_count (comm, &rcount, &scount);
    } else {
        scount = rcount = OMPI_COMM_IS_INTER(comm)?ompi_comm_remote_size(comm):ompi_comm_size(comm);
    }
    if (NULL != request->data.vecs.stypes) {
        for (int i=0; i<scount; i++) {
            if (NULL != request->data.vecs.stypes[i]) {
                OMPI_DATATYPE_RELEASE(request->data.vecs.stypes[i]);
            }
        }
        request->data.vecs.stypes = NULL;
    }
    if (NULL != request->data.vecs.rtypes) {
        for (int i=0; i<rcount; i++) {
            if (NULL != request->data.vecs.rtypes[i]) {
                OMPI_DATATYPE_RELEASE(request->data.vecs.rtypes[i]);
            }
        }
        request->data.vecs.rtypes = NULL;
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
                                       ompi_datatype_t *stypes[], ompi_datatype_t *rtypes[]) {
    ompi_coll_base_nbc_request_t *request = (ompi_coll_base_nbc_request_t *)req;
    bool retain = false;
    ompi_communicator_t *comm = request->super.req_mpi_object.comm;
    int scount, rcount;
    if (REQUEST_COMPLETE(req)) {
        return OMPI_SUCCESS;
    }
    if (OMPI_COMM_IS_TOPO(comm)) {
        (void)mca_topo_base_neighbor_count (comm, &rcount, &scount);
    } else {
        scount = rcount = OMPI_COMM_IS_INTER(comm)?ompi_comm_remote_size(comm):ompi_comm_size(comm);
    }
   
    for (int i=0; i<scount; i++) {
        if (NULL != stypes && NULL != stypes[i] && !ompi_datatype_is_predefined(stypes[i])) {
            OBJ_RETAIN(stypes[i]);
            retain = true;
        }
    }
    for (int i=0; i<rcount; i++) {
        if (NULL != rtypes && NULL != rtypes[i] && !ompi_datatype_is_predefined(rtypes[i])) {
            OBJ_RETAIN(rtypes[i]);
            retain = true;
        }
    }
    if (OPAL_UNLIKELY(retain)) {
        request->data.vecs.stypes = stypes;
        request->data.vecs.rtypes = rtypes;
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

static void nbc_req_cons(ompi_coll_base_nbc_request_t *req) {
    req->cb.req_complete_cb = NULL;
    req->req_complete_cb_data = NULL;
    req->data.objs.objs[0] = NULL;
    req->data.objs.objs[1] = NULL;
}

OBJ_CLASS_INSTANCE(ompi_coll_base_nbc_request_t, ompi_request_t, nbc_req_cons, NULL);
