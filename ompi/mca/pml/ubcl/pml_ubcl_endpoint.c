/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2019-2025 Bull SAS.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file pml_ubcl_endpoint.c
 *
 * UBCL PML
 *
 * Contains functions related to ubcl endpoints
 */

#include "ompi/mca/pml/ubcl/pml_ubcl.h"
#include "ompi/constants.h"
#include "ompi/mca/common/ubcl/common_ubcl.h"
#include "ompi/mca/pml/ubcl/pml_ubcl_utils.h"
#include "ompi/mca/pml/ubcl/pml_ubcl_request.h"
#include "ompi/proc/proc.h"
#include "opal/class/opal_object.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/mca/hwloc/hwloc-internal.h"
#include "opal/mca/common/ubcl/common_ubcl.h"
#include "opal/prefetch.h"
#include "opal/util/proc.h"
#include "ubcl_api.h"

/* UBCL rank is on 61 bits, ompi jobid is 32bits, vpid must be truncated to 29bits */
#define PML_UBCL_VPID_MAX 	(((1 << 29) - 1)) /* We need 3 bits for UBCL rank */
#define PML_UBCL_JOBID_MAX	(OPAL_JOBID_MAX)

static void mca_pml_ubcl_forge_modex_key(char *keyname, size_t size, const int type)
{
    int ret;

    switch (type) {
        case UBCL_ENDPOINT_TYPE_BXI:
            ret = snprintf(keyname, size - 1, "OMPI_UBCL_BXI_ID");
            break;
        case UBCL_ENDPOINT_TYPE_SHMEM:
            ret = snprintf(keyname, size - 1, "OMPI_UBCL_SHM_ID");
            break;
        /* SELF endpoints don't need to forge modex keys */
        case UBCL_ENDPOINT_TYPE_SELF:
        default:
            ret = 0;
    }

    if (0 >= ret || ((size_t) ret) > size - 1) {
        mca_pml_ubcl_error(OMPI_ERROR, "Failed to forge modex keyname");
    }

    /* paranoiac */
    keyname[size - 1] = '\0';
}

static uint64_t mca_pml_forge_rank(ompi_proc_t *proc)
{
    uint64_t jobid, rank;

    if (ompi_proc_is_sentinel(proc)) {
        mca_pml_ubcl_error(OMPI_ERROR,
                           "PML/UBCL proc sentinel are not supported");
        return 0;
    }

    jobid = proc->super.proc_name.jobid;
    rank = proc->super.proc_name.vpid;

    if (rank > (uint32_t) PML_UBCL_VPID_MAX) {
        mca_pml_ubcl_error(OMPI_ERROR,
            "PML/UBCL RANK failed: vpid to high (%d)", rank);
    }

    return  (rank | (jobid << 29));
}

/**
 * Init time: init transports and commit ubcl handles to pmix
 */

static int mca_pml_ubcl_endpoint_modex_put(const int type, void *endpoint_h, size_t size)
{
    int ret;
    char keyname[256];

    mca_pml_ubcl_forge_modex_key(keyname, sizeof(keyname), type);
    OPAL_MODEX_SEND_STRING(ret, PMIX_GLOBAL, keyname, endpoint_h, size);
    if (0 > ret) {
        mca_pml_ubcl_error(OMPI_ERROR, "Failed to modex send string: %s (%d)",
            opal_strerror(ret), ret);
    }

    return OMPI_SUCCESS;
}

static int mca_pml_ubcl_export_local_endpoint_handle(const int type)
{
    int err;
    uint64_t remote_rank_u64;
    char endpoint_h[UBCL_HANDLE_SIZE];
    const size_t size = sizeof(endpoint_h);

    /* dummy valued for ANY_RANK */
    remote_rank_u64 = UBCL_ANY_RANK;

    err = ubcl_export_local_endpoint_handle(type, endpoint_h, &remote_rank_u64);
    if (UBCL_SUCCESS != err) {
        return OMPI_ERROR;
    }

    mca_pml_ubcl_endpoint_modex_put(type, (void *) endpoint_h, size);

    /* We were just interested in the handle.
     * The actual recv rank will be allocated during add_procs calls */
    err = ubcl_close_local_endpoint_channel(type, remote_rank_u64);
    if (UBCL_SUCCESS != err) {
        mca_pml_ubcl_warn(OMPI_ERROR,
                          "PML/UBCL failed to clean local endpoint (very unlikely error)."
                          " For safety reason PML will be disabled.");
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

int mca_pml_ubcl_create_local_endpoint(void)
{
    int type;
    ubcl_error_t err;
    int ompi_error;

    type = UBCL_ENDPOINT_TYPE_SELF;
    err = ubcl_create_local_endpoint(type);
    if (UBCL_SUCCESS != err) {
        mca_pml_ubcl_error(OMPI_ERROR, "Failed ubcl_create_local_endpoint %d (%d)", type, err);
    }

    /* UBCL_ENDPOINT_SHM */
    if (!mca_pml_ubcl_component.force_intranode_bxi) {
        type = UBCL_ENDPOINT_TYPE_SHMEM;
        err = ubcl_create_local_endpoint(type);
        if (UBCL_SUCCESS != err) {
            mca_pml_ubcl_error(OMPI_ERROR, "Failed ubcl_create_local_endpoint %d (%d)", type, err);
        }
        ompi_error = mca_pml_ubcl_export_local_endpoint_handle(type);
        if (OMPI_SUCCESS != ompi_error) {
            return ompi_error;
        }
    }

    type = UBCL_ENDPOINT_TYPE_BXI;
    err = ubcl_create_local_endpoint(type);
    if (UBCL_SUCCESS != err) {
        mca_pml_ubcl_error(OMPI_ERROR, "Failed ubcl_create_local_endpoint %d (%d)", type, err);
    }
    ompi_error = mca_pml_ubcl_export_local_endpoint_handle(type);
    if (OMPI_SUCCESS != ompi_error) {
        return ompi_error;
    }

    return OMPI_SUCCESS;
}

int mca_pml_ubcl_free_local_endpoints()
{
    int ret;
    /* Finalize BXI */
    ret = ubcl_free_local_endpoint(UBCL_ENDPOINT_TYPE_BXI);
    if (UBCL_SUCCESS != ret) {
        return OMPI_ERROR;
    }
    if (!mca_pml_ubcl_component.force_intranode_bxi) {
        ret = ubcl_free_local_endpoint(UBCL_ENDPOINT_TYPE_SHMEM);
        if (UBCL_SUCCESS != ret) {
            return OMPI_ERROR;
        }
    }
    ret = ubcl_free_local_endpoint(UBCL_ENDPOINT_TYPE_SELF);
    if (UBCL_SUCCESS != ret) {
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}
/**
 * Add_proce time: create send and recv endpoint for each peer
 */

static int mca_pml_ubcl_recv_endpoint_modex_get(ompi_proc_t *proc, const int type,
    endp_handle_t endpoint_h, size_t size)
{
    char keyname[256];
    size_t received_size;
    void *received_buffer;
    int ret;

    received_size = 0;
    received_buffer = NULL;

    mca_pml_ubcl_forge_modex_key(keyname, sizeof(keyname), type);
    OPAL_MODEX_RECV_STRING(ret, keyname, &proc->super.proc_name,
                    (void**) &received_buffer, &received_size);
    if (0 > ret) {
        mca_pml_ubcl_error(OMPI_ERROR, "Failed to modex recv string: %s (%d)",
            opal_strerror(ret), ret);
    }

    if (received_size != size) {
        mca_pml_ubcl_error(OMPI_ERROR, "Modex value is truncated (expected: %zu, receiced: %zu)",
            size, received_size);
    }

    memcpy(endpoint_h, received_buffer, size);

    free(received_buffer);

    return OMPI_SUCCESS;
}

static int mca_pml_ubcl_create_send_endpoint(ompi_proc_t *proc, size_t remote_rank, int type)
{
    ubcl_error_t err;
    char endpoint_h[UBCL_HANDLE_SIZE];
    uint64_t ubcl_rank;
    ompi_proc_t *self;

    self = ompi_proc_local();
    ubcl_rank = mca_pml_forge_rank(self);

    mca_pml_ubcl_recv_endpoint_modex_get(proc, type, (endp_handle_t) endpoint_h, sizeof(endpoint_h));
    err = ubcl_create_remote_endpoint(ubcl_rank, remote_rank, type, (endp_handle_t) endpoint_h);

    if (UBCL_SUCCESS != err) {
        return OMPI_ERROR;
    }

    ubcl_get_endpoint_type_capabilities(type, &mca_pml_ubcl_component.endpoint_capabilities[type]);

    return OMPI_SUCCESS;
}

static int mca_pml_ubcl_create_recv_endpoint(uint64_t sender_rank, const int type)
{
    ubcl_error_t err;
    uint64_t remote_rank_u64;
    endp_handle_t endpoint_h[UBCL_HANDLE_SIZE];

    remote_rank_u64 = sender_rank;

    err = ubcl_export_local_endpoint_handle(type, endpoint_h, &remote_rank_u64);
    if (UBCL_SUCCESS != err) {
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

static int mca_pml_ubcl_create_self_endpoints(uint64_t remote_rank)
{
    ubcl_error_t err;
    int type = UBCL_ENDPOINT_TYPE_SELF;
    char endpoint_h[UBCL_HANDLE_SIZE];
    uint64_t my_rank = remote_rank;

    err = ubcl_export_local_endpoint_handle(type, endpoint_h, &my_rank);
    if (UBCL_SUCCESS != err) {
        return OMPI_ERROR;
    }
    err = ubcl_create_remote_endpoint(my_rank, my_rank, type, endpoint_h);
    if (UBCL_SUCCESS != err) {
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

static int get_endpoint_type(ompi_proc_t *proc)
{
    if (ompi_proc_local() == proc) {
        return UBCL_ENDPOINT_TYPE_SELF;
    }

    /* Known limitation: proc_flags are invalid when jobid is different */
    if (proc->super.proc_name.jobid == ompi_proc_local()->super.proc_name.jobid
        && OPAL_PROC_ON_LOCAL_NODE(proc->super.proc_flags)
        && !mca_pml_ubcl_component.force_intranode_bxi) {
        return UBCL_ENDPOINT_TYPE_SHMEM;
    } else {
        return UBCL_ENDPOINT_TYPE_BXI;
    }
}

void mca_pml_ubcl_endpoint_retain(ompi_proc_t *proc)
{
    mca_common_ubcl_endpoint_t *endpoint = NULL;
    assert(NULL != proc);

    endpoint = (proc)->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML];
    assert(NULL != endpoint);

    opal_atomic_fetch_add_32(&endpoint->refcount, 1);
    mca_pml_ubcl_component.nprocs++;
    OBJ_RETAIN(proc);
}

int mca_pml_ubcl_create_endpoints(ompi_proc_t *proc)
{
    int err = OMPI_SUCCESS;
    mca_common_ubcl_endpoint_t *new_endpoint;

    new_endpoint = malloc(sizeof(mca_common_ubcl_endpoint_t));
    if (NULL == new_endpoint) {
        mca_pml_ubcl_error(OMPI_ERR_OUT_OF_RESOURCE,
            "PML/UBCL BXI EP Malloc: not enough memory");
    }

    new_endpoint->refcount = 0; //we increment it to 1 in endpoint_retain
    new_endpoint->rank = mca_pml_forge_rank(proc);
    new_endpoint->type = get_endpoint_type(proc);

    if (UBCL_ENDPOINT_TYPE_SELF == new_endpoint->type) {
        err = mca_pml_ubcl_create_self_endpoints((uint64_t) new_endpoint->rank);
        goto end;
    }

    err = mca_pml_ubcl_create_recv_endpoint(new_endpoint->rank, new_endpoint->type);
    if (OMPI_SUCCESS != err) {
        mca_pml_ubcl_error(err, "Failed to create recv endpoint for rank %zu\n",
                           new_endpoint->rank);
    }

    err = mca_pml_ubcl_create_send_endpoint(proc, new_endpoint->rank, new_endpoint->type);
    if (OMPI_SUCCESS != err) {
        mca_pml_ubcl_error(err, "Failed to create send endpoint for rank %zu\n",
                           new_endpoint->rank);
    }

end:
    (proc)->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML] = new_endpoint;
    mca_pml_ubcl_endpoint_retain(proc);

    return err;
}

int mca_pml_ubcl_add_procs(ompi_proc_t **procs, size_t nprocs)
{
    OPAL_OUTPUT_VERBOSE((50, mca_pml_ubcl_component.output, "UBCL_MODULE_ADD_PROCS\n"));
    OPAL_OUTPUT_VERBOSE((50, mca_pml_ubcl_component.output, "UBCL ADD PROCS: %lu to add", nprocs));

    /* Initialize all endpoint with remote rank */
    for (size_t i = 0; i < nprocs; i++) {
        /* Let's not create endpoints or increment refcount multiple times */
        if (NULL == procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML]) {
            int ret = mca_pml_ubcl_create_endpoints(procs[i]);
            if (OMPI_SUCCESS != ret) {
                mca_pml_ubcl_error(ret, "Failed mca_ubcl_create_remote_endpoint");
            }
        }
    }

    OPAL_OUTPUT_VERBOSE((50, mca_pml_ubcl_component.output, "PML/UBCL ADD_PROCS called"));

    return OMPI_SUCCESS;
}

int mca_pml_ubcl_endpoint_release(ompi_proc_t *proc)
{
    uint32_t endpoint_refcount;
    ubcl_error_t ret = UBCL_SUCCESS;
    int ompi_error = OMPI_SUCCESS;
    mca_common_ubcl_endpoint_t *endpoint = NULL;
    assert(NULL != proc);

    endpoint = (proc)->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML];
    assert(NULL != endpoint);

    endpoint_refcount = opal_atomic_sub_fetch_32(&endpoint->refcount, 1);
    if (0 == endpoint_refcount) {
        ret = ubcl_free_remote_endpoint(endpoint->rank);
        if (UBCL_SUCCESS != ret) {
            ompi_error = ubcl_error_to_ompi(ret);
            mca_pml_ubcl_warn(ompi_error, "PML/UBCL failed to free remote endpoint");
        }
        ret = ubcl_close_local_endpoint_channel(endpoint->type, endpoint->rank);
        if (UBCL_SUCCESS != ret) {
            ompi_error = ubcl_error_to_ompi(ret);
            mca_pml_ubcl_warn(ompi_error, "PML/UBCL failed to close local endpoint channel");
        }
        free(endpoint);
        proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML] = NULL;
        mca_pml_ubcl_component.nprocs -= 1;
        OBJ_RELEASE(proc);
    }

    return ompi_error;
}

int mca_pml_ubcl_del_procs(ompi_proc_t **procs, size_t nprocs)
{
    int ret = OMPI_SUCCESS;
    OPAL_OUTPUT_VERBOSE((50, mca_pml_ubcl_component.output, "UBCL_MODULE_DEL_PROCS\n"));

    for (uint32_t i = 0; i < nprocs; i++) {
        if (OMPI_SUCCESS != mca_pml_ubcl_endpoint_release(procs[i])) {
            ret = OMPI_ERROR;
        }
    }

    OPAL_OUTPUT_VERBOSE((50, mca_pml_ubcl_component.output, "PML/UBCL DEL_PROCS called"));

    return ret;
}
