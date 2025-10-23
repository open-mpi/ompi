/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2025 Bull SAS.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 * Bull eXtreme Interconnect OSC API implementation.
 *
 * Implementation of API defined in osc.h. To see parameters and return values
 * of these functions, refer to ompi/mca/osc/osc.h.
 */

#include "ompi/mca/osc/ubcl/osc_ubcl.h"
#include "ompi/mca/osc/ubcl/osc_ubcl_info.h"
#include "ompi/mca/osc/ubcl/osc_ubcl_request.h"
#include "opal/mca/common/ubcl/common_ubcl.h"
#include "ompi/mca/osc/ubcl/osc_ubcl_utils.h"
#include "ompi/mca/osc/ubcl/osc_ubcl_sync.h"
#include "ompi/mca/common/ubcl/common_ubcl.h"

int ompi_osc_ubcl_put(const void *origin_addr, int origin_count,
                      struct ompi_datatype_t *origin_dt, int target,
                      ptrdiff_t target_disp, int target_count,
                      struct ompi_datatype_t *target_dt, struct ompi_win_t *win)
{
    return ompi_osc_ubcl_rput(origin_addr, origin_count, origin_dt,
                              target, target_disp, target_count, target_dt,
                              win, NULL);
}

int ompi_osc_ubcl_rput(const void *origin_addr, int origin_count,
                       struct ompi_datatype_t *origin_dt, int target,
                       ptrdiff_t target_disp, int target_count,
                       struct ompi_datatype_t *target_dt,
                       struct ompi_win_t *win,
                       struct ompi_request_t **ompi_req)
{
    ubcl_error_t err = 0;
    int ret = OMPI_SUCCESS;
    int64_t disp_unit;
    ptrdiff_t gap;
    size_t span;
    size_t target_iov_count;
    struct iovec *target_iov;
    void *target_addr;
    mca_common_ubcl_endpoint_t *endpoint;
    ubcl_memory_descriptor_t sbuf_md;
    mca_osc_ubcl_module_t *module = (mca_osc_ubcl_module_t *) win->w_osc_module;
    mca_osc_ubcl_request_t *osc_req;

    disp_unit = osc_ubcl_get_disp_unit(module, target);
    OPAL_OUTPUT_VERBOSE(
        (50, mca_osc_ubcl_component.output, "UBCL_OSC_PUT to window %lu\n", module->wid));

    /* Get proc */
    ompi_proc_t *proc;
    proc = ompi_group_peer_lookup_existing(win->w_group, target);
    if (OPAL_UNLIKELY(NULL == proc)) {
        ret = OMPI_ERR_BAD_PARAM;
        mca_osc_ubcl_warn(ret, "Unknown rank %d on window %d", target, module->wid);
        goto exit;
    }

    span = opal_datatype_span((const opal_datatype_t *) origin_dt, origin_count, &gap);
    if (0 == span) {
        if (NULL != ompi_req) {
            *ompi_req = &ompi_request_empty;
        }
        return OMPI_SUCCESS;
    }

    /* We retrieve endpoints created by the PML at init */
    endpoint = (mca_common_ubcl_endpoint_t *) proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML];
    if (OMPI_SUCCESS != ompi_osc_ubcl_check_access_epoch(target, win)) {
        return OMPI_ERR_RMA_CONFLICT;
    }

    /* Allocate an OSC request */
    osc_req = (mca_osc_ubcl_request_t *) opal_free_list_get(&mca_osc_ubcl_component.req_free_list);
    if (OPAL_UNLIKELY(NULL == osc_req)) {
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        mca_osc_ubcl_warn(ret, "Not enough memory to allocate an OSC request");
        goto exit;
    }
    if (NULL != ompi_req) {
        MCA_OSC_UBCL_REQUEST_INIT(osc_req, target, origin_dt, target_dt, win, true);
        *ompi_req = &osc_req->ompi_req;
    } else {
        MCA_OSC_UBCL_REQUEST_INIT(osc_req, target, origin_dt, target_dt, win, false);
    }

    /* Init UBCL MD */
    err = ubcl_memory_descriptor_init(&sbuf_md);
    if (UBCL_SUCCESS != err) {
        /* This should never happen: ubcl_memory_descriptor_init just assign values */
        mca_osc_ubcl_error(ubcl_error_to_ompi(err), "Failed to initialize ubcl MD");
    }

    /* If we don't need to pack we can build a contiguous */
    if (ompi_datatype_is_contiguous_memory_layout(origin_dt, origin_count)) {
        err = ubcl_memory_descriptor_build_contiguous(((char *) origin_addr) + gap, span, &sbuf_md);
        if (UBCL_SUCCESS != err) {
            mca_osc_ubcl_error(ubcl_error_to_ompi(err),
                    "Failed to build contiguous memory descriptor for input buffer");
        }
    }

    /* Always build a custom MD representation so that we have a fallback */
    opal_convertor_copy_and_prepare_for_send(proc->super.proc_convertor, &origin_dt->super,
                                             origin_count, origin_addr, 0,
                                             &(osc_req->origin_convertor));

    if (opal_convertor_on_device(&osc_req->origin_convertor)) {
        opal_free_list_return(&mca_osc_ubcl_component.req_free_list, &(osc_req->super));
        mca_osc_ubcl_warn(OPAL_ERR_NOT_SUPPORTED, "GPU buffer not supported by osc/ubcl");
        ret = OPAL_ERR_NOT_SUPPORTED;
        goto exit;
    }

    err = ubcl_memory_descriptor_build_custom((void *) &(osc_req->origin_convertor),
                                              osc_ubcl_datatype_pack, osc_ubcl_datatype_unpack,
                                              osc_ubcl_datatype_mem_size, osc_ubcl_datatype_finish,
                                              &sbuf_md);

    if (UBCL_SUCCESS != err) {
        mca_osc_ubcl_error(ubcl_error_to_ompi(err),
                "Failed to build custom memory descriptor for input buffer");
    }

    /* We need to build the iovec to describe the memory representation at the target */
    target_iov = NULL;
    target_iov_count = 0;
    target_addr = (void *) (uintptr_t) (target_disp * disp_unit);
    if (ompi_datatype_is_contiguous_memory_layout(target_dt, target_count)) {
        target_iov_count = 1;
        target_iov = (struct iovec *) malloc(target_iov_count * sizeof(struct iovec));

        span = opal_datatype_span((const opal_datatype_t *) target_dt, target_count, &gap);
        target_iov[0].iov_base = target_addr + gap;
        target_iov[0].iov_len = span;
    } else {
        int ret = OMPI_SUCCESS;
        ret = osc_ubcl_build_ddt_iov(target_addr, proc, target_count, target_dt, &target_iov,
                                     &target_iov_count);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            return ret;
        }
    }

    err = ubcl_put(sbuf_md, target_iov, target_iov_count, endpoint->rank, module->wid,
                   ubcl_request_complete_cb, osc_req);

    free(target_iov);

    if (UBCL_SUCCESS != err) {
        mca_osc_ubcl_error(ubcl_error_to_ompi(err), "Failed to send data");
    }

exit:
    return ret;
}
