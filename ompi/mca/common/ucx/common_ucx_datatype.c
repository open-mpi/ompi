/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2020      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2020      Huawei Technologies Co., Ltd.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "common_ucx_datatype.h"

#include "ompi/proc/proc.h"
#include "ompi/runtime/mpiruntime.h"
#include "ompi/attribute/attribute.h"

#include <inttypes.h>
#include <math.h>

#ifdef HAVE_UCP_REQUEST_PARAM_T
#define COMMON_UCX_DATATYPE_SET_VALUE(_datatype, _val) \
    (_datatype)->op_param.send._val; \
    (_datatype)->op_param.bsend._val; \
    (_datatype)->op_param.recv._val;
#endif

static void* common_ucx_generic_datatype_start_pack(void *context, const void *buffer,
                                                    size_t count)
{
    ompi_datatype_t *datatype = context;
    mca_common_ucx_convertor_t *convertor;

    convertor = (mca_common_ucx_convertor_t *)
            COMMON_UCX_FREELIST_GET(&ompi_common_ucx.datatype_ctx.convs);

    OMPI_DATATYPE_RETAIN(datatype);
    convertor->datatype = datatype;
    opal_convertor_copy_and_prepare_for_send(ompi_proc_local_proc->super.proc_convertor,
                                             &datatype->super, count, buffer, 0,
                                             &convertor->opal_conv);
    return convertor;
}

static void* common_ucx_generic_datatype_start_unpack(void *context, void *buffer,
                                                      size_t count)
{
    ompi_datatype_t *datatype = context;
    mca_common_ucx_convertor_t *convertor;

    convertor = (mca_common_ucx_convertor_t *)
            COMMON_UCX_FREELIST_GET(&ompi_common_ucx.datatype_ctx.convs);

    OMPI_DATATYPE_RETAIN(datatype);
    convertor->datatype = datatype;
    convertor->offset = 0;
    opal_convertor_copy_and_prepare_for_recv(ompi_proc_local_proc->super.proc_convertor,
                                             &datatype->super, count, buffer, 0,
                                             &convertor->opal_conv);
    return convertor;
}

static size_t common_ucx_generic_datatype_packed_size(void *state)
{
    mca_common_ucx_convertor_t *convertor = state;
    size_t size;

    opal_convertor_get_packed_size(&convertor->opal_conv, &size);
    return size;
}

static size_t common_ucx_generic_datatype_pack(void *state, size_t offset,
                                               void *dest, size_t max_length)
{
    mca_common_ucx_convertor_t *convertor = state;
    uint32_t iov_count;
    struct iovec iov;
    size_t length;

    iov_count    = 1;
    iov.iov_base = dest;
    iov.iov_len  = max_length;

    opal_convertor_set_position(&convertor->opal_conv, &offset);
    length = max_length;
    opal_convertor_pack(&convertor->opal_conv, &iov, &iov_count, &length);
    return length;
}

static ucs_status_t common_ucx_generic_datatype_unpack(void *state, size_t offset,
                                                       const void *src, size_t length)
{
    mca_common_ucx_convertor_t *convertor = state;

    uint32_t iov_count;
    struct iovec iov;
    opal_convertor_t conv;

    iov_count    = 1;
    iov.iov_base = (void*)src;
    iov.iov_len  = length;

    /* in case if unordered message arrived - create separate convertor to
     * unpack data. */
    if (offset != convertor->offset) {
        OBJ_CONSTRUCT(&conv, opal_convertor_t);
        opal_convertor_copy_and_prepare_for_recv(ompi_proc_local_proc->super.proc_convertor,
                                                 &convertor->datatype->super,
                                                 convertor->opal_conv.count,
                                                 convertor->opal_conv.pBaseBuf, 0,
                                                 &conv);
        opal_convertor_set_position(&conv, &offset);
        opal_convertor_unpack(&conv, &iov, &iov_count, &length);
        opal_convertor_cleanup(&conv);
        OBJ_DESTRUCT(&conv);
        /* permanently switch to un-ordered mode */
        convertor->offset = 0;
    } else {
        opal_convertor_unpack(&convertor->opal_conv, &iov, &iov_count, &length);
        convertor->offset += length;
    }
    return UCS_OK;
}

static void common_ucx_generic_datatype_finish(void *state)
{
    mca_common_ucx_convertor_t *convertor = state;

    opal_convertor_cleanup(&convertor->opal_conv);
    OMPI_DATATYPE_RELEASE(convertor->datatype);
    COMMON_UCX_FREELIST_RETURN(&convertor->ctx->convs, &convertor->super);
}

static ucp_generic_dt_ops_t common_ucx_generic_datatype_ops = {
    .start_pack   = common_ucx_generic_datatype_start_pack,
    .start_unpack = common_ucx_generic_datatype_start_unpack,
    .packed_size  = common_ucx_generic_datatype_packed_size,
    .pack         = common_ucx_generic_datatype_pack,
    .unpack       = common_ucx_generic_datatype_unpack,
    .finish       = common_ucx_generic_datatype_finish
};

int mca_common_ucx_datatype_attr_del_fn(ompi_datatype_t* datatype, int keyval,
                                     void *attr_val, void *extra)
{
    ucp_datatype_t ucp_datatype = (ucp_datatype_t)attr_val;

#ifdef HAVE_UCP_REQUEST_PARAM_T
    free((void*)datatype->pml_data);
#else
    COMMON_UCX_ASSERT((uint64_t)ucp_datatype == datatype->pml_data);
#endif
    ucp_dt_destroy(ucp_datatype);
    datatype->pml_data = COMMON_UCX_DATATYPE_INVALID;
    return OMPI_SUCCESS;
}

__opal_attribute_always_inline__
static inline int mca_common_ucx_datatype_is_contig(ompi_datatype_t *datatype)
{
    ptrdiff_t lb;

    ompi_datatype_type_lb(datatype, &lb);

    return (datatype->super.flags & OPAL_DATATYPE_FLAG_CONTIGUOUS) &&
           (datatype->super.flags & OPAL_DATATYPE_FLAG_NO_GAPS) &&
           (lb == 0);
}

#ifdef HAVE_UCP_REQUEST_PARAM_T
__opal_attribute_always_inline__ static inline
mca_common_ucx_datatype_t *mca_common_ucx_init_nbx_datatype(ompi_datatype_t *datatype,
                                                            ucp_datatype_t ucp_datatype,
                                                            size_t size)
{
    mca_common_ucx_datatype_t *ucx_datatype;
    int is_contig_pow2;

    ucx_datatype = malloc(sizeof(*ucx_datatype));
    if (ucx_datatype == NULL) {
        int err = MPI_ERR_INTERN;
        MCA_COMMON_UCX_ERROR("Failed to allocate datatype structure");
        /* TODO: this error should return to the caller and invoke an error
         * handler from the MPI API call.
         * For now, it is fatal. */
        ompi_mpi_errors_are_fatal_comm_handler(NULL, &err, "Failed to allocate datatype structure");
    }

    /* clone an initial template of the datatype */
    memcpy(ucx_datatype, &ompi_common_ucx.datatype_init, sizeof(*ucx_datatype));

    ucx_datatype->datatype = ucp_datatype;

    is_contig_pow2 = mca_common_ucx_datatype_is_contig(datatype) &&
                     (size && !(size & (size - 1))); /* is_pow2(size) */
    if (is_contig_pow2) {
        ucx_datatype->size_shift = (int)(log(size) / log(2.0)); /* log2(size) */
    } else {
        ucx_datatype->size_shift = 0;
        COMMON_UCX_DATATYPE_SET_VALUE(ucx_datatype, op_attr_mask |= UCP_OP_ATTR_FIELD_DATATYPE);
        COMMON_UCX_DATATYPE_SET_VALUE(ucx_datatype, datatype = ucp_datatype);
    }

    return ucx_datatype;
}
#endif

ucp_datatype_t mca_common_ucx_init_datatype(ompi_datatype_t *datatype)
{
    size_t size = 0; /* init to suppress compiler warning */
    ucp_datatype_t ucp_datatype;
    ucs_status_t status;
    int ret;

    if (mca_common_ucx_datatype_is_contig(datatype)) {
        ompi_datatype_type_size(datatype, &size);
        ucp_datatype = ucp_dt_make_contig(size);
        goto out;
    }

    status = ucp_dt_create_generic(&common_ucx_generic_datatype_ops,
                                   datatype, &ucp_datatype);
    if (status != UCS_OK) {
        int err = MPI_ERR_INTERN;
        MCA_COMMON_UCX_ERROR("Failed to create UCX datatype for %s", datatype->name);
        /* TODO: this error should return to the caller and invoke an error
         * handler from the MPI API call.
         * For now, it is fatal. */
        ompi_mpi_errors_are_fatal_comm_handler(NULL, &err, "Failed to allocate datatype structure");
    }

    /* Add custom attribute, to clean up UCX resources when OMPI datatype is
     * released.
     */
    if (ompi_datatype_is_predefined(datatype)) {
        MCA_COMMON_UCX_ASSERT(datatype->id < OMPI_DATATYPE_MAX_PREDEFINED);
        ompi_common_ucx.datatype_ctx.predefined_types[datatype->id] = ucp_datatype;
    } else {
        ret = ompi_attr_set_c(TYPE_ATTR, datatype, &datatype->d_keyhash,
                              ompi_common_ucx.datatype_ctx.datatype_attr_keyval,
                              (void*)ucp_datatype, false);
        if (ret != OMPI_SUCCESS) {
            int err = MPI_ERR_INTERN;
            MCA_COMMON_UCX_ERROR("Failed to add UCX datatype attribute for %s: %d",
                                 datatype->name, ret);
            /* TODO: this error should return to the caller and invoke an error
             * handler from the MPI API call.
             * For now, it is fatal. */
            ompi_mpi_errors_are_fatal_comm_handler(NULL, &err, "Failed to allocate datatype structure");
        }
    }
out:
    MCA_COMMON_UCX_VERBOSE(7, "created generic UCX datatype 0x%"PRIx64, ucp_datatype)

#ifdef HAVE_UCP_REQUEST_PARAM_T
    UCS_STATIC_ASSERT(sizeof(datatype->pml_data) >= sizeof(mca_common_ucx_datatype_t*));
    datatype->pml_data = (uint64_t)mca_common_ucx_init_nbx_datatype(datatype,
                                                                    ucp_datatype,
                                                                    size);
#else
    datatype->pml_data = ucp_datatype;
#endif

    return ucp_datatype;
}

static void mca_common_ucx_convertor_construct(mca_common_ucx_convertor_t *convertor)
{
    OBJ_CONSTRUCT(&convertor->opal_conv, opal_convertor_t);
}

static void mca_common_ucx_convertor_destruct(mca_common_ucx_convertor_t *convertor)
{
    OBJ_DESTRUCT(&convertor->opal_conv);
}

OBJ_CLASS_INSTANCE(mca_common_ucx_convertor_t,
                   opal_free_list_item_t,
                   mca_common_ucx_convertor_construct,
                   mca_common_ucx_convertor_destruct);


static void mca_common_ucx_datatype_ctx_construct(mca_common_ucx_datatype_ctx_t *ctx)
{
    ompi_attribute_fn_ptr_union_t copy_fn;
    ompi_attribute_fn_ptr_union_t del_fn;
    int ret, i;

    /* Create a key for adding custom attributes to datatypes */
    copy_fn.attr_datatype_copy_fn  =
                    (MPI_Type_internal_copy_attr_function*)MPI_TYPE_NULL_COPY_FN;
    del_fn.attr_datatype_delete_fn = mca_common_ucx_datatype_attr_del_fn;
    ret = ompi_attr_create_keyval(TYPE_ATTR, copy_fn, del_fn,
                                  &ctx->datatype_attr_keyval, NULL, 0,
                                  NULL);
    if (ret != OMPI_SUCCESS) {
        MCA_COMMON_UCX_ERROR("Failed to create keyval for UCX datatypes: %d", ret);
        return;
    }

    ctx->datatype_attr_keyval = MPI_KEYVAL_INVALID;
    for (i = 0; i < OMPI_DATATYPE_MAX_PREDEFINED; ++i) {
        ctx->predefined_types[i] = COMMON_UCX_DATATYPE_INVALID;
    }

    OBJ_CONSTRUCT(&ctx->convs, mca_common_ucx_freelist_t);

    COMMON_UCX_FREELIST_INIT(&ctx->convs, mca_common_ucx_convertor_t, 0, 128, -1, 128);
}

static void mca_common_ucx_datatype_ctx_destruct(mca_common_ucx_datatype_ctx_t *ctx)
{
    int i;

    if (ctx->datatype_attr_keyval != MPI_KEYVAL_INVALID) {
        ompi_attr_free_keyval(TYPE_ATTR, &ctx->datatype_attr_keyval, false);
    }

    for (i = 0; i < OMPI_DATATYPE_MAX_PREDEFINED; ++i) {
        if (ctx->predefined_types[i] != COMMON_UCX_DATATYPE_INVALID) {
            ucp_dt_destroy(ctx->predefined_types[i]);
            ctx->predefined_types[i] = COMMON_UCX_DATATYPE_INVALID;
        }
    }

    OBJ_DESTRUCT(&ctx->convs);
}

OBJ_CLASS_INSTANCE(mca_common_ucx_datatype_ctx_t,
                   opal_object_t,
                   mca_common_ucx_datatype_ctx_construct,
                   mca_common_ucx_datatype_ctx_destruct);
