/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "pml_ucx_datatype.h"

#include "ompi/runtime/mpiruntime.h"

#include <inttypes.h>


static void* pml_ucx_generic_datatype_start_pack(void *context, const void *buffer,
                                                 size_t count)
{
    ompi_datatype_t *datatype = context;
    mca_pml_ucx_convertor_t *convertor;

    convertor = (mca_pml_ucx_convertor_t *)PML_UCX_FREELIST_GET(&ompi_pml_ucx.convs);

    OBJ_RETAIN(datatype);
    convertor->datatype = datatype;
    opal_convertor_copy_and_prepare_for_send(ompi_proc_local_proc->super.proc_convertor,
                                             &datatype->super, count, buffer, 0,
                                             &convertor->opal_conv);
    return convertor;
}

static void* pml_ucx_generic_datatype_start_unpack(void *context, void *buffer,
                                                   size_t count)
{
    ompi_datatype_t *datatype = context;
    mca_pml_ucx_convertor_t *convertor;

    convertor = (mca_pml_ucx_convertor_t *)PML_UCX_FREELIST_GET(&ompi_pml_ucx.convs);

    OBJ_RETAIN(datatype);
    convertor->datatype = datatype;
    opal_convertor_copy_and_prepare_for_recv(ompi_proc_local_proc->super.proc_convertor,
                                             &datatype->super, count, buffer, 0,
                                             &convertor->opal_conv);
    return convertor;
}

static size_t pml_ucx_generic_datatype_packed_size(void *state)
{
    mca_pml_ucx_convertor_t *convertor = state;
    size_t size;

    opal_convertor_get_packed_size(&convertor->opal_conv, &size);
    return size;
}

static size_t pml_ucx_generic_datatype_pack(void *state, size_t offset,
                                            void *dest, size_t max_length)
{
    mca_pml_ucx_convertor_t *convertor = state;
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

static ucs_status_t pml_ucx_generic_datatype_unpack(void *state, size_t offset,
                                                    const void *src, size_t length)
{
    mca_pml_ucx_convertor_t *convertor = state;

    uint32_t iov_count;
    struct iovec iov;

    iov_count    = 1;
    iov.iov_base = (void*)src;
    iov.iov_len  = length;

    opal_convertor_set_position(&convertor->opal_conv, &offset);
    opal_convertor_unpack(&convertor->opal_conv, &iov, &iov_count, &length);
    return UCS_OK;
}

static void pml_ucx_generic_datatype_finish(void *state)
{
    mca_pml_ucx_convertor_t *convertor = state;

    opal_convertor_cleanup(&convertor->opal_conv);
    OBJ_RELEASE(convertor->datatype);
    PML_UCX_FREELIST_RETURN(&ompi_pml_ucx.convs, &convertor->super);
}

static ucp_generic_dt_ops_t pml_ucx_generic_datatype_ops = {
    .start_pack   = pml_ucx_generic_datatype_start_pack,
    .start_unpack = pml_ucx_generic_datatype_start_unpack,
    .packed_size  = pml_ucx_generic_datatype_packed_size,
    .pack         = pml_ucx_generic_datatype_pack,
    .unpack       = pml_ucx_generic_datatype_unpack,
    .finish       = pml_ucx_generic_datatype_finish
};

ucp_datatype_t mca_pml_ucx_init_datatype(ompi_datatype_t *datatype)
{
    ucp_datatype_t ucp_datatype;
    ucs_status_t status;
    ptrdiff_t lb;
    size_t size;

    ompi_datatype_type_lb(datatype, &lb);

    if ((datatype->super.flags & OPAL_DATATYPE_FLAG_CONTIGUOUS) &&
        (datatype->super.flags & OPAL_DATATYPE_FLAG_NO_GAPS) &&
        (lb == 0))
    {
        ompi_datatype_type_size(datatype, &size);
        PML_UCX_ASSERT(size > 0);
        datatype->pml_data = ucp_dt_make_contig(size);
        return datatype->pml_data;
    }

    status = ucp_dt_create_generic(&pml_ucx_generic_datatype_ops,
                                         datatype, &ucp_datatype);
    if (status != UCS_OK) {
        PML_UCX_ERROR("Failed to create UCX datatype for %s", datatype->name);
        ompi_mpi_abort(&ompi_mpi_comm_world.comm, 1);
    }

    PML_UCX_VERBOSE(7, "created generic UCX datatype 0x%"PRIx64, ucp_datatype)
    // TODO put this on a list to be destroyed later

    datatype->pml_data = ucp_datatype;
    return ucp_datatype;
}

static void mca_pml_ucx_convertor_construct(mca_pml_ucx_convertor_t *convertor)
{
    OBJ_CONSTRUCT(&convertor->opal_conv, opal_convertor_t);
}

static void mca_pml_ucx_convertor_destruct(mca_pml_ucx_convertor_t *convertor)
{
    OBJ_DESTRUCT(&convertor->opal_conv);
}

OBJ_CLASS_INSTANCE(mca_pml_ucx_convertor_t,
                   opal_free_list_item_t,
                   mca_pml_ucx_convertor_construct,
                   mca_pml_ucx_convertor_destruct);
