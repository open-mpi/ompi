/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/include/opal/types.h"
#include "opal/datatype/opal_convertor.h"

#include "bcol_iboffload.h"
#include "bcol_iboffload_frag.h"
#include "bcol_iboffload_device.h"
#include "bcol_iboffload_collreq.h"
#include "bcol_iboffload_endpoint.h"

static void frag_constructor(mca_bcol_iboffload_frag_t *frag)
{
    mca_bcol_iboffload_reg_t* reg =
        (mca_bcol_iboffload_reg_t*) frag->super.registration;

    memset(&frag->sg_entry, 0, sizeof(struct ibv_sge));
    frag->sg_entry.addr = (uint64_t) (uintptr_t) frag->super.ptr;

    frag->registration = reg;

    if (NULL != reg) {
        frag->sg_entry.lkey = reg->mr->lkey;
    }

    frag->next = NULL;
    frag->type = MCA_BCOL_IBOFFLOAD_NONE_OWNER;
    frag->ref_counter = 0;
    frag->qp_index = -1;
}

OBJ_CLASS_INSTANCE(
        mca_bcol_iboffload_frag_t,
        ompi_free_list_item_t,
        frag_constructor,
        NULL);


static mca_bcol_iboffload_frag_t*
    mca_bcol_iboffload_get_ml_frag_calc(mca_bcol_iboffload_module_t *iboffload,
                                    mca_bcol_iboffload_collreq_t *coll_request,
                                    size_t len, size_t src_offset)
{
    int rc;

    mca_bcol_iboffload_frag_t *fragment;
    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;

    uint64_t sbuff = (uint64_t) (uintptr_t) coll_request->buffer_info[SBUF].buf +
                                           src_offset;

    /* The buffer was allocated on ML level,
       no need to allocate local buffer */
    rc = pack_data_for_calc(iboffload->device->dev.ib_dev_context,
                            cm->map_ompi_to_ib_calcs[coll_request->op->op_type],
                            cm->map_ompi_to_ib_dt[coll_request->dtype->id],
                            false /* host order */,
                            (void *) sbuff, 0,
                            &coll_request->actual_ib_op,
                            &coll_request->actual_ib_dtype,
                            (void *) sbuff);
    if (OPAL_UNLIKELY(0 != rc)) {
        IBOFFLOAD_VERBOSE(10, ("pack_data_for_calc failed, op: %s, type: %s\n",
                                coll_request->op->o_name, coll_request->dtype->name));
        return NULL;
    }

    fragment = mca_bcol_iboffload_get_ml_frag(
            iboffload, coll_request->qp_index, len,
            coll_request->buffer_info[SBUF].lkey,
            sbuff);

    return fragment;
}

static mca_bcol_iboffload_frag_t *
mca_bcol_iboffload_get_packed_frag(mca_bcol_iboffload_module_t *iboffload,
                                   uint32_t destination, int qp_index, size_t len,
                                   struct opal_convertor_t *convertor)
{
    /* local variables */
    int rc;
    uint32_t out_size;
    size_t max_size = 0;

    struct iovec payload_iovec;

    ompi_free_list_item_t *item;
    mca_bcol_iboffload_frag_t *frag;

    mca_bcol_iboffload_device_t *device = iboffload->device;

    /* Get frag from free list */
    OMPI_FREE_LIST_GET(&device->frags_free[qp_index], item, rc);
    if (OPAL_UNLIKELY(NULL == item)) {
        return NULL;
    }

    frag = (mca_bcol_iboffload_frag_t *) item;

    /* Pack data into the buffer */
    out_size = 1;
    payload_iovec.iov_len = len;

    payload_iovec.iov_base = (void *) (uintptr_t) frag->sg_entry.addr;

    rc = opal_convertor_pack(convertor, &(payload_iovec),
            &out_size, &max_size);
    if (OPAL_UNLIKELY(rc < 0)) {
        /* Error: put the fragment back */
        OMPI_FREE_LIST_RETURN(&device->frags_free[qp_index], item);
        return NULL;
    }

    return frag;
}

static mca_bcol_iboffload_frag_t *
mca_bcol_iboffload_get_calc_frag(mca_bcol_iboffload_module_t *iboffload, int qp_index,
                                 struct mca_bcol_iboffload_collreq_t *coll_request)
{
    int rc;

    ompi_free_list_item_t *item;
    mca_bcol_iboffload_frag_t *frag;

    mca_bcol_iboffload_device_t *device = iboffload->device;
    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;

    IBOFFLOAD_VERBOSE(10, ("Start to pack frag.\n"));

    /* Get frag from free list */
    OMPI_FREE_LIST_GET(&device->frags_free[qp_index], item, rc);
    if (OPAL_UNLIKELY(NULL == item)) {
        return NULL;
    }

    frag = (mca_bcol_iboffload_frag_t *) item;

    /* Pack data into the buffer */
    rc = pack_data_for_calc(device->dev.ib_dev_context,
                            cm->map_ompi_to_ib_calcs[coll_request->op->op_type],
                            cm->map_ompi_to_ib_dt[coll_request->dtype->id], false,
                            coll_request->buffer_info[SBUF].buf, 0,
                            &coll_request->actual_ib_op,
                            &coll_request->actual_ib_dtype,
                            (void *) (uintptr_t) frag->sg_entry.addr);
    if (OPAL_UNLIKELY(0 != rc)) {
        IBOFFLOAD_ERROR(("pack_data_for_calc failed, op: %s, type: %s\n",
                                coll_request->op->o_name, coll_request->dtype->name));
        return NULL;
    }

    return frag;
}

mca_bcol_iboffload_frag_t*
mca_bcol_iboffload_get_send_frag(mca_bcol_iboffload_collreq_t *coll_request,
                                 uint32_t destination, int qp_index, size_t len,
                                 size_t src_offset, int buf_index, int send_frag_type)
{
    /* local variables */
    mca_bcol_iboffload_frag_t *frag;
    mca_bcol_iboffload_module_t *iboffload = coll_request->module;

    mca_bcol_iboffload_endpoint_t *endpoint =
                iboffload->endpoints[destination];

    IBOFFLOAD_VERBOSE(10, ("Calling mca_bcol_iboffload_get_send_frag qp_index %d",
                            qp_index));

    if ((endpoint->qps[qp_index].sd_wqe) <= 0) {
        IBOFFLOAD_VERBOSE(10, ("No send wqe %d",
                    endpoint->qps[qp_index].sd_wqe));
        return NULL;
    }

    --endpoint->qps[qp_index].sd_wqe;

    IBOFFLOAD_VERBOSE(10, ("Endpoint %p: qp_index %d, destination %d, sd_wqe %d",
                            endpoint, qp_index, destination, endpoint->qps[qp_index].sd_wqe));

    switch (send_frag_type) {
        case MCA_BCOL_IBOFFLOAD_SEND_FRAG_DUMMY:
            IBOFFLOAD_VERBOSE(10, ("Getting MCA_BCOL_IBOFFLOAD_SEND_FRAG_DUMMY"));
            assert(NULL != &iboffload->device->dummy_frags[qp_index]);
            return &iboffload->device->dummy_frags[qp_index];

        case MCA_BCOL_IBOFFLOAD_SEND_FRAG:
        {
            int rc;
            ompi_free_list_item_t *item;
            IBOFFLOAD_VERBOSE(10, ("Getting MCA_BCOL_IBOFFLOAD_SEND_FRAG"));

            /* Get frag from free list */
            OMPI_FREE_LIST_GET(&iboffload->device->frags_free[qp_index], item, rc);

            frag = (mca_bcol_iboffload_frag_t *) item;
        }

        break;
        case MCA_BCOL_IBOFFLOAD_SEND_FRAG_CONVERT:
            IBOFFLOAD_VERBOSE(10, ("Getting MCA_BCOL_IBOFFLOAD_SEND_FRAG_CONVERT"));
            frag = mca_bcol_iboffload_get_packed_frag(iboffload, destination,
                         qp_index, len, &coll_request->send_convertor);

        break;
        case MCA_BCOL_IBOFFLOAD_SEND_FRAG_CALC:
            IBOFFLOAD_VERBOSE(10, ("Getting MCA_BCOL_IBOFFLOAD_SEND_FRAG_CALC"));
            frag = mca_bcol_iboffload_get_calc_frag(iboffload, qp_index, coll_request);

        break;
        case MCA_BCOL_IBOFFLOAD_SEND_FRAG_ML:
            IBOFFLOAD_VERBOSE(10, ("Getting MCA_BCOL_IBOFFLOAD_SEND_FRAG_ML"));
            frag = mca_bcol_iboffload_get_ml_frag(
                  iboffload, qp_index, len, coll_request->buffer_info[buf_index].lkey,
                  (uint64_t)(uintptr_t) coll_request->buffer_info[buf_index].buf + src_offset);

        break;
        case MCA_BCOL_IBOFFLOAD_SEND_FRAG_ML_CALC:
            frag = mca_bcol_iboffload_get_ml_frag_calc(iboffload, coll_request, len, src_offset);
            IBOFFLOAD_VERBOSE(10, ("Getting MCA_BCOL_IBOFFLOAD_SEND_FRAG_ML_CALC"));

        break;
        default:
            IBOFFLOAD_VERBOSE(10, ("Getting default"));
            frag = NULL;
            IBOFFLOAD_ERROR(("Unknown send frag type %d for QP index %d",
                              send_frag_type, qp_index));
    }

    if (OPAL_UNLIKELY(NULL == frag)) {
        IBOFFLOAD_VERBOSE(10, ("Getting NULL"));
        return NULL;
    }

    frag->sg_entry.length = len;
    frag->next = NULL;

    return frag;
}

void
mca_bcol_iboffload_frag_init(ompi_free_list_item_t* item, void* ctx)
{
    int qp_index = *(int *) ctx;
    mca_bcol_iboffload_frag_t *frag = (mca_bcol_iboffload_frag_t *) item;

    frag->qp_index = qp_index;
    frag->type = MCA_BCOL_IBOFFLOAD_BCOL_OWNER;
}

void
mca_bcol_iboffload_ml_frag_init(ompi_free_list_item_t* item, void* ctx)
{
    mca_bcol_iboffload_frag_t *frag = (mca_bcol_iboffload_frag_t *) item;

    frag->qp_index = -1;
    frag->type = MCA_BCOL_IBOFFLOAD_ML_OWNER;
}
