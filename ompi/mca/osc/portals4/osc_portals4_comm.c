/*
 * Copyright (c) 2011-2017 Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2014      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2015-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mca/osc/osc.h"
#include "ompi/mca/osc/base/base.h"
#include "ompi/mca/osc/base/osc_base_obj_convert.h"

#include "osc_portals4.h"
#include "osc_portals4_request.h"


static int
ompi_osc_portals4_get_op(struct ompi_op_t *op, ptl_op_t *ptl_op)
{
    if (MPI_MAX == op) {
        *ptl_op = PTL_MAX;
    } else if (MPI_MIN == op) {
        *ptl_op = PTL_MIN;
    } else if (MPI_SUM == op) {
        *ptl_op = PTL_SUM;
    } else if (MPI_PROD == op) {
        *ptl_op = PTL_PROD;
    } else if (MPI_LAND == op) {
        *ptl_op = PTL_LAND;
    } else if (MPI_BAND == op) {
        *ptl_op = PTL_BAND;
    } else if (MPI_LOR == op) {
        *ptl_op = PTL_LOR;
    } else if (MPI_BOR == op) {
        *ptl_op = PTL_BOR;
    } else if (MPI_LXOR == op) {
        *ptl_op = PTL_LXOR;
    } else if (MPI_BXOR == op) {
        *ptl_op = PTL_BXOR;
    } else {
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}


static int
get_sized_type(bool sign, size_t size, ptl_datatype_t *ptl_dt)
{
    if (sign) {
        switch (size) {
        case 1:
            *ptl_dt = PTL_INT8_T;
            break;
        case 2:
            *ptl_dt = PTL_INT16_T;
            break;
        case 4:
            *ptl_dt = PTL_INT32_T;
            break;
        case 8:
            *ptl_dt = PTL_INT64_T;
            break;
        default:
            return OMPI_ERROR;
        }
    } else {
        switch (size) {
        case 1:
            *ptl_dt = PTL_UINT8_T;
            break;
        case 2:
            *ptl_dt = PTL_UINT16_T;
            break;
        case 4:
            *ptl_dt = PTL_UINT32_T;
            break;
        case 8:
            *ptl_dt = PTL_UINT64_T;
            break;
        default:
            return OMPI_ERROR;
        }
    }

    return OMPI_SUCCESS;
}


static int
ompi_osc_portals4_get_dt(struct ompi_datatype_t *dt, ptl_datatype_t *ptl_dt)
{
    ompi_datatype_t *base_dt = ompi_datatype_get_single_predefined_type_from_args(dt);

    if (MPI_BYTE == base_dt) {
        *ptl_dt = PTL_INT8_T;
    } else if (MPI_CHAR == base_dt) {
        *ptl_dt = PTL_INT8_T;
    } else if (MPI_SHORT == base_dt) {
        return get_sized_type(true, sizeof(short), ptl_dt);
    } else if (MPI_INT == base_dt) {
        return get_sized_type(true, sizeof(int), ptl_dt);
    } else if (MPI_LONG == base_dt) {
        return get_sized_type(true, sizeof(long), ptl_dt);
    } else if (MPI_FLOAT == base_dt) {
        *ptl_dt = PTL_FLOAT;
    } else if (MPI_DOUBLE == base_dt) {
        *ptl_dt = PTL_DOUBLE;
    } else if (MPI_LONG_DOUBLE == base_dt) {
        *ptl_dt = PTL_LONG_DOUBLE;
    } else if (MPI_UNSIGNED_CHAR == base_dt) {
        *ptl_dt = PTL_UINT8_T;
    } else if (MPI_SIGNED_CHAR == base_dt) {
        *ptl_dt = PTL_UINT8_T;
    } else if (MPI_UNSIGNED_SHORT == base_dt) {
        return get_sized_type(false, sizeof(short), ptl_dt);
    } else if (MPI_UNSIGNED_LONG == base_dt) {
        return get_sized_type(false, sizeof(long), ptl_dt);
    } else if (MPI_UNSIGNED == base_dt) {
        return get_sized_type(false, sizeof(int), ptl_dt);
#if OPAL_HAVE_LONG_LONG
    } else if (MPI_LONG_LONG_INT == base_dt) {
        return get_sized_type(true, sizeof(long long int), ptl_dt);
    } else if (MPI_LONG_LONG == base_dt) {
        return get_sized_type(true, sizeof(long long), ptl_dt);
#endif
    } else if (MPI_INT8_T == base_dt) {
        *ptl_dt = PTL_INT8_T;
    } else if (MPI_UINT8_T == base_dt) {
        *ptl_dt = PTL_UINT8_T;
    } else if (MPI_INT16_T == base_dt) {
        *ptl_dt = PTL_INT16_T;
    } else if (MPI_UINT16_T == base_dt) {
        *ptl_dt = PTL_UINT16_T;
    } else if (MPI_INT32_T == base_dt) {
        *ptl_dt = PTL_INT32_T;
    } else if (MPI_UINT32_T == base_dt) {
        *ptl_dt = PTL_UINT32_T;
    } else if (MPI_INT64_T == base_dt) {
        *ptl_dt = PTL_INT64_T;
    } else if (MPI_UINT64_T == base_dt) {
        *ptl_dt = PTL_UINT64_T;
#if HAVE_FLOAT__COMPLEX
    } else if (MPI_C_COMPLEX == base_dt) {
        *ptl_dt = PTL_DOUBLE_COMPLEX;
    } else if (MPI_C_FLOAT_COMPLEX == base_dt) {
        *ptl_dt = PTL_FLOAT_COMPLEX;
#endif
#if HAVE_DOUBLE__COMPLEX
    } else if (MPI_C_DOUBLE_COMPLEX == base_dt) {
        *ptl_dt = PTL_DOUBLE_COMPLEX;
#endif
#if HAVE_LONG_DOUBLE__COMPLEX
    } else if (MPI_C_LONG_DOUBLE_COMPLEX == base_dt) {
        *ptl_dt = PTL_LONG_DOUBLE_COMPLEX;
#endif
    } else if (MPI_AINT == base_dt) {
        if (sizeof(MPI_Aint) == 2) {
            *ptl_dt = PTL_UINT16_T;
        } else if (sizeof(MPI_Aint) == 4) {
            *ptl_dt = PTL_UINT32_T;
        } else if (sizeof(MPI_Aint) == 8) {
            *ptl_dt = PTL_UINT64_T;
        }
    } else {
        return OMPI_ERROR;
    }

    return 0;
}

static  ptl_size_t
number_of_fragments(ptl_size_t length, ptl_size_t maxlength)
{
    ptl_size_t nb_frag = length == 0 ? 1 : (length - 1) / maxlength + 1;
    OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                         "%s,%d : %ld fragment(s)", __FUNCTION__, __LINE__, nb_frag));
    return nb_frag;
}

/* put in segments no larger than segment_length */
static int
segmentedPut(int64_t *opcount,
             ptl_handle_md_t md_h,
             ptl_size_t origin_offset,
             ptl_size_t put_length,
             ptl_size_t segment_length,
             ptl_ack_req_t ack_req,
             ptl_process_t target_id,
             ptl_pt_index_t pt_index,
             ptl_match_bits_t match_bits,
             ptl_size_t target_offset,
             void *user_ptr,
             ptl_hdr_data_t hdr_data)
{
    int ret;
    ptl_size_t bytes_put = 0;

    do {
        opal_atomic_add_fetch_64(opcount, 1);

        ptl_size_t frag_length = MIN(put_length, segment_length);
        OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                             "Put size : %lu/%lu, offset:%lu", frag_length, put_length, bytes_put));
        ret = PtlPut(md_h,
                     origin_offset + bytes_put,
                     frag_length,
                     ack_req,
                     target_id,
                     pt_index,
                     match_bits,
                     target_offset + bytes_put,
                     user_ptr,
                     hdr_data);
        if (PTL_OK != ret) {
            opal_atomic_add_fetch_64(opcount, -1);
            opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                                 "%s:%d PtlPut failed with return value %d",
                                 __FUNCTION__, __LINE__, ret);
            return ret;
        }
        put_length -= frag_length;
        bytes_put += frag_length;
    } while (put_length);
    return PTL_OK;
}

/* get in segments no larger than segment_length */
static int
segmentedGet(int64_t *opcount,
             ptl_handle_md_t md_h,
             ptl_size_t origin_offset,
             ptl_size_t get_length,
             ptl_size_t segment_length,
             ptl_process_t target_id,
             ptl_pt_index_t pt_index,
             ptl_match_bits_t match_bits,
             ptl_size_t target_offset,
             void *user_ptr)
{
    int ret;
    ptl_size_t bytes_gotten = 0;

    do {
        opal_atomic_add_fetch_64(opcount, 1);

        ptl_size_t frag_length = MIN(get_length, segment_length);
        OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                             "Get size : %lu/%lu, offset:%lu", frag_length, get_length, bytes_gotten));

        ret = PtlGet(md_h,
                     (ptl_size_t) origin_offset + bytes_gotten,
                     frag_length,
                     target_id,
                     pt_index,
                     match_bits,
                     target_offset + bytes_gotten,
                     user_ptr);
        if (PTL_OK != ret) {
            opal_atomic_add_fetch_64(opcount, -1);
            opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                                 "%s:%d PtlGet failed with return value %d",
                                 __FUNCTION__, __LINE__, ret);
            return ret;
        }
        get_length -= frag_length;
        bytes_gotten += frag_length;
    } while (get_length);
    return PTL_OK;
}

/* atomic op in segments no larger than segment_length */
static int
segmentedAtomic(int64_t *opcount,
                ptl_handle_md_t md_h,
                ptl_size_t origin_offset,
                ptl_size_t length,
                ptl_size_t segment_length,
                ptl_process_t target_id,
                ptl_pt_index_t pt_index,
                ptl_match_bits_t match_bits,
                ptl_size_t target_offset,
                void *user_ptr,
                ptl_op_t ptl_op,
                ptl_datatype_t ptl_dt)
{
    int ret;
    ptl_size_t sent = 0;

    do {
        opal_atomic_add_fetch_64(opcount, 1);

        ptl_size_t frag_length = MIN(length, segment_length);
        OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                             "Atomic size : %lu/%lu, offset:%lu", frag_length, length, sent));
        ret = PtlAtomic(md_h,
                        (ptl_size_t) origin_offset + sent,
                        frag_length,
                        PTL_ACK_REQ,
                        target_id,
                        pt_index,
                        match_bits,
                        target_offset + sent,
                        user_ptr,
                        0,
                        ptl_op,
                        ptl_dt);
        if (PTL_OK != ret) {
            opal_atomic_add_fetch_64(opcount, -1);
            opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                                 "%s:%d PtlAtomic failed with return value %d",
                                 __FUNCTION__, __LINE__, ret);
            return ret;
        }
        length -= frag_length;
        sent += frag_length;
    } while (length);
    return PTL_OK;
}

/* atomic op in segments no larger than segment_length */
static int
segmentedFetchAtomic(int64_t *opcount,
                     ptl_handle_md_t result_md_h,
                     ptl_size_t result_offset,
                     ptl_handle_md_t origin_md_h,
                     ptl_size_t origin_offset,
                     ptl_size_t length,
                     ptl_size_t segment_length,
                     ptl_process_t target_id,
                     ptl_pt_index_t pt_index,
                     ptl_match_bits_t match_bits,
                     ptl_size_t target_offset,
                     void *user_ptr,
                     ptl_op_t ptl_op,
                     ptl_datatype_t ptl_dt)
{
    int ret;
    ptl_size_t sent = 0;

    do {
        opal_atomic_add_fetch_64(opcount, 1);

        ptl_size_t frag_length = MIN(length, segment_length);
        OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                             "Atomic size : %lu/%lu, offset:%lu", frag_length, length, sent));
        ret = PtlFetchAtomic(result_md_h,
                             result_offset + sent,
                             origin_md_h,
                             origin_offset + sent,
                             frag_length,
                             target_id,
                             pt_index,
                             match_bits,
                             target_offset + sent,
                             user_ptr,
                             0,
                             ptl_op,
                             ptl_dt);
        if (PTL_OK != ret) {
            opal_atomic_add_fetch_64(opcount, -1);
            opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                                 "%s:%d PtlFetchAtomic failed with return value %d",
                                 __FUNCTION__, __LINE__, ret);
            return ret;
        }
        length -= frag_length;
        sent += frag_length;
    } while (length);
    return PTL_OK;
}

/* swap in segments no larger than segment_length */
static int
segmentedSwap(int64_t *opcount,
              ptl_handle_md_t result_md_h,
              ptl_size_t result_offset,
              ptl_handle_md_t origin_md_h,
              ptl_size_t origin_offset,
              ptl_size_t length,
              ptl_size_t segment_length,
              ptl_process_t target_id,
              ptl_pt_index_t pt_index,
              ptl_match_bits_t match_bits,
              ptl_size_t target_offset,
              void *user_ptr,
              ptl_datatype_t ptl_dt)
{
    int ret;
    ptl_size_t sent = 0;

    do {
        opal_atomic_add_fetch_64(opcount, 1);

        ptl_size_t frag_length = MIN(length, segment_length);
        OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                             "Swap size : %lu/%lu, offset:%lu", frag_length, length, sent));
        ret = PtlSwap(result_md_h,
                      result_offset + sent,
                      origin_md_h,
                      (ptl_size_t) origin_offset + sent,
                      frag_length,
                      target_id,
                      pt_index,
                      match_bits,
                      target_offset + sent,
                      user_ptr,
                      0,
                      NULL,
                      PTL_SWAP,
                      ptl_dt);
        if (PTL_OK != ret) {
            opal_atomic_add_fetch_64(opcount, -1);
            opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                                 "%s:%d PtlSwap failed with return value %d",
                                 __FUNCTION__, __LINE__, ret);
            return ret;
        }
        length -= frag_length;
        sent += frag_length;
    } while (length);
    return PTL_OK;
}

static int
create_iov_list(const void       *address,
                int               count,
                ompi_datatype_t  *datatype,
                ptl_iovec_t     **ptl_iovec,
                ptl_size_t       *ptl_iovec_count)
{
    struct iovec iov[OSC_PORTALS4_IOVEC_MAX];
    opal_convertor_t convertor;
    uint32_t iov_count;
    uint32_t iov_index, ptl_iovec_index;
    /* needed for opal_convertor_raw but not used */
    size_t size;
    int ret;
    bool done;

    OBJ_CONSTRUCT(&convertor, opal_convertor_t);
    ret = opal_convertor_copy_and_prepare_for_send (ompi_mpi_local_convertor, &datatype->super, count,
                                                    address, 0, &convertor);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }


    *ptl_iovec_count = 0;
    ptl_iovec_index = 0;
    do {
        /* decode segments of the data */
        iov_count = OSC_PORTALS4_IOVEC_MAX;
        iov_index = 0;

        /* opal_convertor_raw returns done when it has reached the end of the data */
        done = opal_convertor_raw (&convertor, iov, &iov_count, &size);

        *ptl_iovec_count += iov_count;
        *ptl_iovec = (ptl_iovec_t *)realloc(*ptl_iovec, *ptl_iovec_count * sizeof(ptl_iovec_t));

        while (iov_index != iov_count) {
            OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                                 "adding iov[%d].[%p,%lu] to ptl_iovec", iov_index, iov[iov_index].iov_base, iov[iov_index].iov_len));
            (*ptl_iovec)[ptl_iovec_index].iov_base = iov[iov_index].iov_base;
            (*ptl_iovec)[ptl_iovec_index].iov_len  = iov[iov_index].iov_len;

            ptl_iovec_index++;
            iov_index++;
        }

        assert(*ptl_iovec_count == ptl_iovec_index);
    } while (!done);

    return OMPI_SUCCESS;

}

/* get from a contiguous remote to an iovec local */
static int
get_to_iovec(ompi_osc_portals4_module_t *module,
             const void       *origin_address,
             int               origin_count,
             ompi_datatype_t  *origin_datatype,
             ptl_process_t     peer,
             int               target_count,
             ompi_datatype_t  *target_datatype,
             size_t            offset,
             ptl_pt_index_t    pt_index,
             ptl_match_bits_t  match_bits,
             void             *user_ptr)
{
    int ret;
    size_t size;
    ptrdiff_t length, origin_lb, target_lb, extent;
    ptl_md_t md;

    if (module->origin_iovec_md_h != PTL_INVALID_HANDLE) {
        PtlMDRelease(module->origin_iovec_md_h);
        free(module->origin_iovec_list);
        module->origin_iovec_md_h = PTL_INVALID_HANDLE;
        module->origin_iovec_list = NULL;
    }

    ptl_size_t iovec_count=0;
    create_iov_list(
        origin_address,
        origin_count,
        origin_datatype,
        &module->origin_iovec_list,
        &iovec_count);

    ret = ompi_datatype_get_true_extent(origin_datatype, &origin_lb, &extent);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }
    ret = ompi_datatype_get_true_extent(target_datatype, &target_lb, &extent);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }
    ompi_datatype_type_size(origin_datatype, &size);
    length = size * origin_count;

    md.start = module->origin_iovec_list;
    md.length = iovec_count;
    if (user_ptr) {
        md.options = PTL_IOVEC | PTL_MD_EVENT_SEND_DISABLE | PTL_MD_EVENT_CT_REPLY | PTL_MD_EVENT_CT_ACK;
    } else {
        md.options = PTL_IOVEC | PTL_MD_EVENT_SUCCESS_DISABLE | PTL_MD_EVENT_CT_REPLY | PTL_MD_EVENT_CT_ACK;
    }
    md.eq_handle = mca_osc_portals4_component.matching_eq_h;
    md.ct_handle = module->ct_h;
    ret = PtlMDBind(module->ni_h, &md, &module->origin_iovec_md_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                            "%s:%d: PtlMDBind(iovec) failed: %d\n",
                            __FILE__, __LINE__, ret);
        return ret;
    }

    opal_atomic_add_fetch_64(&module->opcount, 1);

    OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                 "%s,%d Get(origin_count=%d, origin_lb=%lu, target_count=%d, target_lb=%lu, size=%lu, length=%lu, offset=%lu, op_count=%ld)",
                 __FUNCTION__, __LINE__, origin_count, origin_lb, target_count, target_lb, size, length, offset, module->opcount));
    ret = PtlGet(module->origin_iovec_md_h,
                 (ptl_size_t) origin_lb,
                 length,
                 peer,
                 module->pt_idx,
                 module->match_bits,
                 offset + target_lb,
                 user_ptr);
    if (PTL_OK != ret) {
        OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                     "%s,%d PtlGet() failed: ret = %d",
                     __FUNCTION__, __LINE__, ret));
        opal_atomic_add_fetch_64(&module->opcount, -1);
        return ret;
    }

    return OMPI_SUCCESS;
}

/* get to an iovec MD from a contiguous target using fragments no larger
 * than max_fetch_atomic_size to guarantee atomic writes at the origin */
static int
atomic_get_to_iovec(ompi_osc_portals4_module_t *module,
                    const void       *origin_address,
                    int               origin_count,
                    ompi_datatype_t  *origin_datatype,
                    ptl_process_t     peer,
                    int               target_count,
                    ompi_datatype_t  *target_datatype,
                    size_t            offset,
                    ptl_pt_index_t    pt_index,
                    ptl_match_bits_t  match_bits,
                    void             *user_ptr)
{
    int ret;
    size_t size;
    ptrdiff_t length, origin_lb, target_lb, extent;
    ptl_md_t md;

    if (module->origin_iovec_md_h != PTL_INVALID_HANDLE) {
        PtlMDRelease(module->origin_iovec_md_h);
        free(module->origin_iovec_list);
        module->origin_iovec_md_h = PTL_INVALID_HANDLE;
        module->origin_iovec_list = NULL;
    }

    ptl_size_t iovec_count=0;
    create_iov_list(
        origin_address,
        origin_count,
        origin_datatype,
        &module->origin_iovec_list,
        &iovec_count);

    ret = ompi_datatype_get_true_extent(origin_datatype, &origin_lb, &extent);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }
    ret = ompi_datatype_get_true_extent(target_datatype, &target_lb, &extent);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }
    ompi_datatype_type_size(origin_datatype, &size);
    length = size * origin_count;

    md.start = module->origin_iovec_list;
    md.length = iovec_count;
    if (user_ptr) {
        md.options = PTL_IOVEC | PTL_MD_EVENT_SEND_DISABLE | PTL_MD_EVENT_CT_REPLY | PTL_MD_EVENT_CT_ACK;
    } else {
        md.options = PTL_IOVEC | PTL_MD_EVENT_SUCCESS_DISABLE | PTL_MD_EVENT_CT_REPLY | PTL_MD_EVENT_CT_ACK;
    }
    md.eq_handle = mca_osc_portals4_component.matching_eq_h;
    md.ct_handle = module->ct_h;
    ret = PtlMDBind(module->ni_h, &md, &module->origin_iovec_md_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                            "%s:%d: PtlMDBind(iovec) failed: %d\n",
                            __FILE__, __LINE__, ret);
        return ret;
    }

    OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                 "%s,%d Get(origin_count=%d, origin_lb=%lu, target_count=%d, target_lb=%lu, size=%lu, length=%lu, offset=%lu, op_count=%ld)",
                 __FUNCTION__, __LINE__, origin_count, origin_lb, target_count, target_lb, size, length, offset, module->opcount));
    ret = segmentedGet(&module->opcount,
                       module->origin_iovec_md_h,
                       (ptl_size_t) origin_lb,
                       length,
                       module->fetch_atomic_max,
                       peer,
                       module->pt_idx,
                       module->match_bits,
                       offset + target_lb,
                       user_ptr);
    if (PTL_OK != ret) {
        return ret;
    }

    return OMPI_SUCCESS;
}

/* put from an iovec MD into a contiguous target */
static int
put_from_iovec(ompi_osc_portals4_module_t *module,
               const void       *origin_address,
               int               origin_count,
               ompi_datatype_t  *origin_datatype,
               ptl_process_t     peer,
               int               target_count,
               ompi_datatype_t  *target_datatype,
               size_t            offset,
               ptl_pt_index_t    pt_index,
               ptl_match_bits_t  match_bits,
               void             *user_ptr)
{
    int ret;
    size_t size;
    ptrdiff_t length, origin_lb, target_lb, extent;
    ptl_md_t md;

    if (module->origin_iovec_md_h != PTL_INVALID_HANDLE) {
        PtlMDRelease(module->origin_iovec_md_h);
        free(module->origin_iovec_list);
        module->origin_iovec_md_h = PTL_INVALID_HANDLE;
        module->origin_iovec_list = NULL;
    }

    ptl_size_t iovec_count=0;
    create_iov_list(
        origin_address,
        origin_count,
        origin_datatype,
        &module->origin_iovec_list,
        &iovec_count);

    ret = ompi_datatype_get_true_extent(origin_datatype, &origin_lb, &extent);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }
    ret = ompi_datatype_get_true_extent(target_datatype, &target_lb, &extent);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }
    ompi_datatype_type_size(origin_datatype, &size);
    length = size * origin_count;

    md.start = module->origin_iovec_list;
    md.length = iovec_count;
    if (user_ptr) {
        md.options = PTL_IOVEC | PTL_MD_EVENT_SEND_DISABLE | PTL_MD_EVENT_CT_REPLY | PTL_MD_EVENT_CT_ACK;
    } else {
        md.options = PTL_IOVEC | PTL_MD_EVENT_SUCCESS_DISABLE | PTL_MD_EVENT_CT_REPLY | PTL_MD_EVENT_CT_ACK;
    }
    md.eq_handle = mca_osc_portals4_component.matching_eq_h;
    md.ct_handle = module->ct_h;
    ret = PtlMDBind(module->ni_h, &md, &module->origin_iovec_md_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                            "%s:%d: PtlMDBind(iovec) failed: %d\n",
                            __FILE__, __LINE__, ret);
        return ret;
    }

    opal_atomic_add_fetch_64(&module->opcount, 1);

    OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                 "%s,%d Put(origin_count=%d, origin_lb=%lu, target_count=%d, target_lb=%lu, size=%lu, length=%lu, offset=%lu, op_count=%ld)",
                 __FUNCTION__, __LINE__, origin_count, origin_lb, target_count, target_lb, size, length, offset, module->opcount));
    ret = PtlPut(module->origin_iovec_md_h,
                 (ptl_size_t) origin_lb,
                 length,
                 PTL_ACK_REQ,
                 peer,
                 module->pt_idx,
                 module->match_bits,
                 offset + target_lb,
                 user_ptr,
                 0);
    if (PTL_OK != ret) {
        OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                     "%s,%d PtlPut() failed: ret = %d",
                     __FUNCTION__, __LINE__, ret));
        opal_atomic_add_fetch_64(&module->opcount, -1);
        return ret;
    }

    return OMPI_SUCCESS;
}

/* put from an iovec MD into a contiguous target using fragments no larger
 * than max_atomic_size to guarantee atomic writes at the target */
static int
atomic_put_from_iovec(ompi_osc_portals4_module_t *module,
                      const void       *origin_address,
                      int               origin_count,
                      ompi_datatype_t  *origin_datatype,
                      ptl_process_t     peer,
                      int               target_count,
                      ompi_datatype_t  *target_datatype,
                      size_t            offset,
                      ptl_pt_index_t    pt_index,
                      ptl_match_bits_t  match_bits,
                      void             *user_ptr)
{
    int ret;
    size_t size;
    ptrdiff_t length, origin_lb, target_lb, extent;
    ptl_md_t md;

    if (module->origin_iovec_md_h != PTL_INVALID_HANDLE) {
        PtlMDRelease(module->origin_iovec_md_h);
        free(module->origin_iovec_list);
        module->origin_iovec_md_h = PTL_INVALID_HANDLE;
        module->origin_iovec_list = NULL;
    }

    ptl_size_t iovec_count=0;
    create_iov_list(
        origin_address,
        origin_count,
        origin_datatype,
        &module->origin_iovec_list,
        &iovec_count);

    ret = ompi_datatype_get_true_extent(origin_datatype, &origin_lb, &extent);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }
    ret = ompi_datatype_get_true_extent(target_datatype, &target_lb, &extent);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }
    ompi_datatype_type_size(origin_datatype, &size);
    length = size * origin_count;

    md.start = module->origin_iovec_list;
    md.length = iovec_count;
    if (user_ptr) {
        md.options = PTL_IOVEC | PTL_MD_EVENT_SEND_DISABLE | PTL_MD_EVENT_CT_REPLY | PTL_MD_EVENT_CT_ACK;
    } else {
        md.options = PTL_IOVEC | PTL_MD_EVENT_SUCCESS_DISABLE | PTL_MD_EVENT_CT_REPLY | PTL_MD_EVENT_CT_ACK;
    }
    md.eq_handle = mca_osc_portals4_component.matching_eq_h;
    md.ct_handle = module->ct_h;
    ret = PtlMDBind(module->ni_h, &md, &module->origin_iovec_md_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                            "%s:%d: PtlMDBind(iovec) failed: %d\n",
                            __FILE__, __LINE__, ret);
        return ret;
    }

    OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                 "%s,%d Put(origin_count=%d, origin_lb=%lu, target_count=%d, target_lb=%lu, length=%lu, op_count=%ld)",
                 __FUNCTION__, __LINE__, origin_count, origin_lb, target_count, target_lb, length, module->opcount));
    ret = segmentedPut(&module->opcount,
                       module->origin_iovec_md_h,
                       (ptl_size_t) origin_lb,
                       length,
                       module->atomic_max,
                       PTL_ACK_REQ,
                       peer,
                       module->pt_idx,
                       module->match_bits,
                       offset + target_lb,
                       NULL,
                       0);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    return OMPI_SUCCESS;
}

/* perform atomic operation on iovec local and contiguous remote */
static int
atomic_from_iovec(ompi_osc_portals4_module_t *module,
                  const void       *origin_address,
                  int               origin_count,
                  ompi_datatype_t  *origin_datatype,
                  ptl_process_t     peer,
                  int               target_count,
                  ompi_datatype_t  *target_datatype,
                  size_t            offset,
                  ptl_pt_index_t    pt_index,
                  ptl_match_bits_t  match_bits,
                  struct ompi_op_t *op,
                  void             *user_ptr)
{
    int ret;
    size_t size;
    ptrdiff_t length, origin_lb, target_lb, extent;
    ptl_md_t md;
    ptl_op_t ptl_op;
    ptl_datatype_t ptl_dt;

    if (module->origin_iovec_md_h != PTL_INVALID_HANDLE) {
        PtlMDRelease(module->origin_iovec_md_h);
        free(module->origin_iovec_list);
        module->origin_iovec_md_h = PTL_INVALID_HANDLE;
        module->origin_iovec_list = NULL;
    }

    ptl_size_t iovec_count=0;
    create_iov_list(
        origin_address,
        origin_count,
        origin_datatype,
        &module->origin_iovec_list,
        &iovec_count);

    ret = ompi_osc_portals4_get_dt(target_datatype, &ptl_dt);
    if (OMPI_SUCCESS != ret) {
        opal_output(ompi_osc_base_framework.framework_output,
                "datatype is not currently supported");
        return OMPI_ERR_NOT_SUPPORTED;
    }
    ret = ompi_osc_portals4_get_op(op, &ptl_op);
    if (OMPI_SUCCESS != ret) {
        opal_output(ompi_osc_base_framework.framework_output,
                "operation is not currently supported");
        return OMPI_ERR_NOT_SUPPORTED;
    }

    ret = ompi_datatype_get_true_extent(origin_datatype, &origin_lb, &extent);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }
    ret = ompi_datatype_get_true_extent(target_datatype, &target_lb, &extent);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }
    ompi_datatype_type_size(origin_datatype, &size);
    length = size * origin_count;

    md.start = module->origin_iovec_list;
    md.length = iovec_count;
    if (user_ptr) {
        md.options = PTL_IOVEC | PTL_MD_EVENT_SEND_DISABLE | PTL_MD_EVENT_CT_REPLY | PTL_MD_EVENT_CT_ACK;
    } else {
        md.options = PTL_IOVEC | PTL_MD_EVENT_SUCCESS_DISABLE | PTL_MD_EVENT_CT_REPLY | PTL_MD_EVENT_CT_ACK;
    }
    md.eq_handle = mca_osc_portals4_component.matching_eq_h;
    md.ct_handle = module->ct_h;
    ret = PtlMDBind(module->ni_h, &md, &module->origin_iovec_md_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                            "%s:%d: PtlMDBind(iovec) failed: %d\n",
                            __FILE__, __LINE__, ret);
        return ret;
    }

    ret = segmentedAtomic(&module->opcount,
                          module->origin_iovec_md_h,
                          (ptl_size_t) origin_lb,
                          length,
                          module->atomic_max,
                          peer,
                          module->pt_idx,
                          module->match_bits,
                          offset + target_lb,
                          user_ptr,
                          ptl_op,
                          ptl_dt);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    return OMPI_SUCCESS;
}

/* perform atomic operation on iovec local and contiguous remote */
static int
swap_to_iovec(ompi_osc_portals4_module_t *module,
              const void       *result_address,
              int               result_count,
              ompi_datatype_t  *result_datatype,
              const void       *origin_address,
              int               origin_count,
              ompi_datatype_t  *origin_datatype,
              ptl_process_t     peer,
              int               target_count,
              ompi_datatype_t  *target_datatype,
              size_t            offset,
              ptl_pt_index_t    pt_index,
              ptl_match_bits_t  match_bits,
              void             *user_ptr)
{
    int ret;
    size_t size;
    ptl_size_t iovec_count=0;
    ptrdiff_t length, result_lb, origin_lb, target_lb, extent;
    ptl_md_t md;
    ptl_datatype_t ptl_dt;

    if (module->result_iovec_md_h != PTL_INVALID_HANDLE) {
        PtlMDRelease(module->result_iovec_md_h);
        free(module->result_iovec_list);
        module->result_iovec_md_h = PTL_INVALID_HANDLE;
        module->result_iovec_list = NULL;
    }

    create_iov_list(
        result_address,
        result_count,
        result_datatype,
        &module->result_iovec_list,
        &iovec_count);

    md.start = module->result_iovec_list;
    md.length = iovec_count;
    if (user_ptr) {
        md.options = PTL_IOVEC | PTL_MD_EVENT_SEND_DISABLE | PTL_MD_EVENT_CT_REPLY | PTL_MD_EVENT_CT_ACK;
    } else {
        md.options = PTL_IOVEC | PTL_MD_EVENT_SUCCESS_DISABLE | PTL_MD_EVENT_CT_REPLY | PTL_MD_EVENT_CT_ACK;
    }
    md.eq_handle = mca_osc_portals4_component.matching_eq_h;
    md.ct_handle = module->ct_h;
    ret = PtlMDBind(module->ni_h, &md, &module->result_iovec_md_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                            "%s:%d: PtlMDBind(iovec) failed: %d\n",
                            __FILE__, __LINE__, ret);
        return ret;
    }

    if (module->origin_iovec_md_h != PTL_INVALID_HANDLE) {
        PtlMDRelease(module->origin_iovec_md_h);
        free(module->origin_iovec_list);
        module->origin_iovec_md_h = PTL_INVALID_HANDLE;
        module->origin_iovec_list = NULL;
    }

    create_iov_list(
        origin_address,
        origin_count,
        origin_datatype,
        &module->origin_iovec_list,
        &iovec_count);

    md.start = module->origin_iovec_list;
    md.length = iovec_count;
    md.options = PTL_IOVEC | PTL_MD_EVENT_SUCCESS_DISABLE | PTL_MD_EVENT_CT_REPLY | PTL_MD_EVENT_CT_ACK;
    md.eq_handle = mca_osc_portals4_component.matching_eq_h;
    md.ct_handle = module->ct_h;
    ret = PtlMDBind(module->ni_h, &md, &module->origin_iovec_md_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                            "%s:%d: PtlMDBind(iovec) failed: %d\n",
                            __FILE__, __LINE__, ret);
        return ret;
    }

    ret = ompi_osc_portals4_get_dt(target_datatype, &ptl_dt);
    if (OMPI_SUCCESS != ret) {
        opal_output(ompi_osc_base_framework.framework_output,
                "datatype is not currently supported");
        return OMPI_ERR_NOT_SUPPORTED;
    }

    ret = ompi_datatype_get_true_extent(result_datatype, &result_lb, &extent);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }
    ret = ompi_datatype_get_true_extent(origin_datatype, &origin_lb, &extent);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }
    ret = ompi_datatype_get_true_extent(target_datatype, &target_lb, &extent);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }
    ompi_datatype_type_size(origin_datatype, &size);
    length = size * origin_count;

    ret = segmentedSwap(&module->opcount,
                        module->result_iovec_md_h,
                        (ptl_size_t) result_lb,
                        module->origin_iovec_md_h,
                        (ptl_size_t) origin_lb,
                        length,
                        module->fetch_atomic_max,
                        peer,
                        module->pt_idx,
                        module->match_bits,
                        offset + target_lb,
                        user_ptr,
                        ptl_dt);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    return OMPI_SUCCESS;
}

/* perform fetch atomic operation on iovec local and contiguous remote */
static int
fetch_atomic_to_iovec(ompi_osc_portals4_module_t *module,
                      const void       *result_address,
                      int               result_count,
                      ompi_datatype_t  *result_datatype,
                      const void       *origin_address,
                      int               origin_count,
                      ompi_datatype_t  *origin_datatype,
                      ptl_process_t     peer,
                      int               target_count,
                      ompi_datatype_t  *target_datatype,
                      size_t            offset,
                      ptl_pt_index_t    pt_index,
                      ptl_match_bits_t  match_bits,
                      struct ompi_op_t *op,
                      void             *user_ptr)
{
    int ret;
    size_t size;
    ptl_size_t iovec_count=0;
    ptrdiff_t length, result_lb, origin_lb, target_lb, extent;
    ptl_md_t md;
    ptl_op_t ptl_op;
    ptl_datatype_t ptl_dt;

    if (module->result_iovec_md_h != PTL_INVALID_HANDLE) {
        PtlMDRelease(module->result_iovec_md_h);
        free(module->result_iovec_list);
        module->result_iovec_md_h = PTL_INVALID_HANDLE;
        module->result_iovec_list = NULL;
    }

    create_iov_list(
        result_address,
        result_count,
        result_datatype,
        &module->result_iovec_list,
        &iovec_count);

    md.start = module->result_iovec_list;
    md.length = iovec_count;
    if (user_ptr) {
        md.options = PTL_IOVEC | PTL_MD_EVENT_SEND_DISABLE | PTL_MD_EVENT_CT_REPLY | PTL_MD_EVENT_CT_ACK;
    } else {
        md.options = PTL_IOVEC | PTL_MD_EVENT_SUCCESS_DISABLE | PTL_MD_EVENT_CT_REPLY | PTL_MD_EVENT_CT_ACK;
    }
    md.eq_handle = mca_osc_portals4_component.matching_eq_h;
    md.ct_handle = module->ct_h;
    ret = PtlMDBind(module->ni_h, &md, &module->result_iovec_md_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                            "%s:%d: PtlMDBind(iovec) failed: %d\n",
                            __FILE__, __LINE__, ret);
        return ret;
    }

    if (module->origin_iovec_md_h != PTL_INVALID_HANDLE) {
        PtlMDRelease(module->origin_iovec_md_h);
        free(module->origin_iovec_list);
        module->origin_iovec_md_h = PTL_INVALID_HANDLE;
        module->origin_iovec_list = NULL;
    }

    create_iov_list(
        origin_address,
        origin_count,
        origin_datatype,
        &module->origin_iovec_list,
        &iovec_count);

    md.start = module->origin_iovec_list;
    md.length = iovec_count;
    md.options = PTL_IOVEC | PTL_MD_EVENT_SUCCESS_DISABLE | PTL_MD_EVENT_CT_REPLY | PTL_MD_EVENT_CT_ACK;
    md.eq_handle = mca_osc_portals4_component.matching_eq_h;
    md.ct_handle = module->ct_h;
    ret = PtlMDBind(module->ni_h, &md, &module->origin_iovec_md_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                            "%s:%d: PtlMDBind(iovec) failed: %d\n",
                            __FILE__, __LINE__, ret);
        return ret;
    }

    ret = ompi_osc_portals4_get_dt(target_datatype, &ptl_dt);
    if (OMPI_SUCCESS != ret) {
        opal_output(ompi_osc_base_framework.framework_output,
                "datatype is not currently supported");
        return OMPI_ERR_NOT_SUPPORTED;
    }
    ret = ompi_osc_portals4_get_op(op, &ptl_op);
    if (OMPI_SUCCESS != ret) {
        opal_output(ompi_osc_base_framework.framework_output,
                "operation is not currently supported");
        return OMPI_ERR_NOT_SUPPORTED;
    }

    ret = ompi_datatype_get_true_extent(result_datatype, &result_lb, &extent);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }
    ret = ompi_datatype_get_true_extent(origin_datatype, &origin_lb, &extent);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }
    ret = ompi_datatype_get_true_extent(target_datatype, &target_lb, &extent);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }
    ompi_datatype_type_size(origin_datatype, &size);
    length = size * origin_count;

    ret = segmentedFetchAtomic(&module->opcount,
                               module->result_iovec_md_h,
                               (ptl_size_t) result_lb,
                               module->origin_iovec_md_h,
                               (ptl_size_t) origin_lb,
                               length,
                               module->fetch_atomic_max,
                               peer,
                               module->pt_idx,
                               module->match_bits,
                               offset + target_lb,
                               user_ptr,
                               ptl_op,
                               ptl_dt);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    return OMPI_SUCCESS;
}

/*
 * Derived from ompi_osc_rdma_master_noncontig()
 */

/* put in the largest chunks possible given the noncontiguous restriction */
static int
put_to_noncontig(int64_t          *opcount,
                 ptl_handle_md_t   md_h,
                 const void       *origin_address,
                 int               origin_count,
                 ompi_datatype_t  *origin_datatype,
                 ptl_process_t     peer,
                 int               target_count,
                 ompi_datatype_t  *target_datatype,
                 size_t            offset,
                 ptl_pt_index_t    pt_index,
                 ptl_match_bits_t  match_bits,
                 void             *user_ptr)
{
    struct iovec origin_iovec[OSC_PORTALS4_IOVEC_MAX], target_iovec[OSC_PORTALS4_IOVEC_MAX];
    opal_convertor_t origin_convertor, target_convertor;
    uint32_t origin_iov_count, target_iov_count;
    uint32_t origin_iov_index, target_iov_index;
    /* needed for opal_convertor_raw but not used */
    size_t origin_size, target_size, rdma_len;
    size_t max_rdma_len = mca_osc_portals4_component.ptl_max_msg_size;
    int ret;
    bool done;

    /* prepare convertors for the source and target. these convertors will be used to determine the
     * contiguous segments within the source and target. */
    OBJ_CONSTRUCT(&origin_convertor, opal_convertor_t);
    ret = opal_convertor_copy_and_prepare_for_send (ompi_mpi_local_convertor, &origin_datatype->super, origin_count,
                                                    (void*)origin_address, 0, &origin_convertor);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    OBJ_CONSTRUCT(&target_convertor, opal_convertor_t);
    ret = opal_convertor_copy_and_prepare_for_send (ompi_mpi_local_convertor, &target_datatype->super, target_count,
                                                    (void *)NULL, 0, &target_convertor);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    origin_iov_index = 0;
    origin_iov_count = 0;

    do {
        /* decode segments of the remote data */
        target_iov_count = OSC_PORTALS4_IOVEC_MAX;
        target_iov_index = 0;

        /* opal_convertor_raw returns done when it has reached the end of the data */
        done = opal_convertor_raw (&target_convertor, target_iovec, &target_iov_count, &target_size);

        /* loop on the target segments until we have exhaused the decoded source data */
        while (target_iov_index != target_iov_count) {
            if (origin_iov_index == origin_iov_count) {
                /* decode segments of the target buffer */
                origin_iov_count = OSC_PORTALS4_IOVEC_MAX;
                origin_iov_index = 0;
                (void) opal_convertor_raw (&origin_convertor, origin_iovec, &origin_iov_count, &origin_size);
            }

            /* we already checked that the target was large enough. this should be impossible */
            assert (0 != origin_iov_count);

            /* determine how much to transfer in this operation */
            rdma_len = MIN(MIN(origin_iovec[origin_iov_index].iov_len, target_iovec[target_iov_index].iov_len), max_rdma_len);

            opal_atomic_add_fetch_64(opcount, 1);

            OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                             "performing rdma on contiguous region. local: %p, remote: %p, len: %lu",
                             origin_iovec[origin_iov_index].iov_base, target_iovec[target_iov_index].iov_base,
                             (unsigned long) target_iovec[target_iov_index].iov_len));

            ret = PtlPut(md_h,
                         (ptl_size_t)origin_iovec[origin_iov_index].iov_base,
                         rdma_len,
                         PTL_ACK_REQ,
                         peer,
                         pt_index,
                         match_bits,
                         offset + (ptl_size_t)target_iovec[target_iov_index].iov_base,
                         user_ptr,
                         0);
            if (OPAL_UNLIKELY(PTL_OK != ret)) {
                opal_atomic_add_fetch_64(opcount, -1);
                return ret;
            }

            /* adjust io vectors */
            origin_iovec[origin_iov_index].iov_len -= rdma_len;
            target_iovec[target_iov_index].iov_len -= rdma_len;
            origin_iovec[origin_iov_index].iov_base = (void *)((intptr_t) origin_iovec[origin_iov_index].iov_base + rdma_len);
            target_iovec[target_iov_index].iov_base = (void *)((intptr_t) target_iovec[target_iov_index].iov_base + rdma_len);

            origin_iov_index += (0 == origin_iovec[origin_iov_index].iov_len);
            target_iov_index += (0 == target_iovec[target_iov_index].iov_len);
        }
    } while (!done);

    /* clean up convertors */
    opal_convertor_cleanup (&origin_convertor);
    OBJ_DESTRUCT(&origin_convertor);
    opal_convertor_cleanup (&target_convertor);
    OBJ_DESTRUCT(&target_convertor);

    return OMPI_SUCCESS;
}

/* put in fragments no larger than max_atomic_size to guarantee atomic writes at the target */
static int
atomic_put_to_noncontig(ompi_osc_portals4_module_t *module,
                        ptl_handle_md_t   md_h,
                        const void       *origin_address,
                        int               origin_count,
                        ompi_datatype_t  *origin_datatype,
                        ptl_process_t     peer,
                        int               target_count,
                        ompi_datatype_t  *target_datatype,
                        size_t            offset,
                        ptl_pt_index_t    pt_index,
                        ptl_match_bits_t  match_bits,
                        void             *user_ptr)
{
    struct iovec origin_iovec[OSC_PORTALS4_IOVEC_MAX], target_iovec[OSC_PORTALS4_IOVEC_MAX];
    opal_convertor_t origin_convertor, target_convertor;
    uint32_t origin_iov_count, target_iov_count;
    uint32_t origin_iov_index, target_iov_index;
    /* needed for opal_convertor_raw but not used */
    size_t origin_size, target_size, rdma_len;
    size_t max_rdma_len = module->atomic_max;
    int ret;
    bool done;

    /* prepare convertors for the source and target. these convertors will be used to determine the
     * contiguous segments within the source and target. */
    OBJ_CONSTRUCT(&origin_convertor, opal_convertor_t);
    ret = opal_convertor_copy_and_prepare_for_send (ompi_mpi_local_convertor, &origin_datatype->super, origin_count,
                                                    (void*)origin_address, 0, &origin_convertor);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    OBJ_CONSTRUCT(&target_convertor, opal_convertor_t);
    ret = opal_convertor_copy_and_prepare_for_send (ompi_mpi_local_convertor, &target_datatype->super, target_count,
                                                    (void *)NULL, 0, &target_convertor);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    origin_iov_index = 0;
    origin_iov_count = 0;

    do {
        /* decode segments of the remote data */
        target_iov_count = OSC_PORTALS4_IOVEC_MAX;
        target_iov_index = 0;

        /* opal_convertor_raw returns done when it has reached the end of the data */
        done = opal_convertor_raw (&target_convertor, target_iovec, &target_iov_count, &target_size);

        /* loop on the target segments until we have exhaused the decoded source data */
        while (target_iov_index != target_iov_count) {
            if (origin_iov_index == origin_iov_count) {
                /* decode segments of the target buffer */
                origin_iov_count = OSC_PORTALS4_IOVEC_MAX;
                origin_iov_index = 0;
                (void) opal_convertor_raw (&origin_convertor, origin_iovec, &origin_iov_count, &origin_size);
            }

            /* we already checked that the target was large enough. this should be impossible */
            assert (0 != origin_iov_count);

            /* determine how much to transfer in this operation */
            rdma_len = MIN(MIN(origin_iovec[origin_iov_index].iov_len, target_iovec[target_iov_index].iov_len), max_rdma_len);

            opal_atomic_add_fetch_64(&module->opcount, 1);

            OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                             "performing rdma on contiguous region. local: %p, remote: %p, len: %lu",
                             origin_iovec[origin_iov_index].iov_base, target_iovec[target_iov_index].iov_base,
                             (unsigned long) target_iovec[target_iov_index].iov_len));

            ret = PtlPut(md_h,
                         (ptl_size_t)origin_iovec[origin_iov_index].iov_base,
                         rdma_len,
                         PTL_ACK_REQ,
                         peer,
                         pt_index,
                         match_bits,
                         offset + (ptl_size_t)target_iovec[target_iov_index].iov_base,
                         user_ptr,
                         0);
            if (OPAL_UNLIKELY(PTL_OK != ret)) {
                opal_atomic_add_fetch_64(&module->opcount, -1);
                return ret;
            }

            /* adjust io vectors */
            origin_iovec[origin_iov_index].iov_len -= rdma_len;
            target_iovec[target_iov_index].iov_len -= rdma_len;
            origin_iovec[origin_iov_index].iov_base = (void *)((intptr_t) origin_iovec[origin_iov_index].iov_base + rdma_len);
            target_iovec[target_iov_index].iov_base = (void *)((intptr_t) target_iovec[target_iov_index].iov_base + rdma_len);

            origin_iov_index += (0 == origin_iovec[origin_iov_index].iov_len);
            target_iov_index += (0 == target_iovec[target_iov_index].iov_len);
        }
    } while (!done);

    return OMPI_SUCCESS;
}

/* perform atomic operation on (non)contiguous local and noncontiguous remote */
static int
atomic_to_noncontig(ompi_osc_portals4_module_t *module,
                    ptl_handle_md_t   md_h,
                    const void       *origin_address,
                    int               origin_count,
                    ompi_datatype_t  *origin_datatype,
                    ptl_process_t     peer,
                    int               target_count,
                    ompi_datatype_t  *target_datatype,
                    size_t            offset,
                    ptl_pt_index_t    pt_index,
                    ptl_match_bits_t  match_bits,
                    struct ompi_op_t *op,
                    void             *user_ptr)
{
    struct iovec origin_iovec[OSC_PORTALS4_IOVEC_MAX], target_iovec[OSC_PORTALS4_IOVEC_MAX];
    opal_convertor_t origin_convertor, target_convertor;
    uint32_t origin_iov_count, target_iov_count;
    uint32_t origin_iov_index, target_iov_index;
    ptl_op_t ptl_op;
    ptl_datatype_t ptl_dt;
    /* needed for opal_convertor_raw but not used */
    size_t origin_size, target_size, atomic_len;
    int ret;
    bool done;

    /* prepare convertors for the source and target. these convertors will be used to determine the
     * contiguous segments within the source and target. */
    OBJ_CONSTRUCT(&origin_convertor, opal_convertor_t);
    ret = opal_convertor_copy_and_prepare_for_send (ompi_mpi_local_convertor, &origin_datatype->super, origin_count,
                                                    (void*)origin_address, 0, &origin_convertor);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    OBJ_CONSTRUCT(&target_convertor, opal_convertor_t);
    ret = opal_convertor_copy_and_prepare_for_send (ompi_mpi_local_convertor, &target_datatype->super, target_count,
                                                    (void *)NULL, 0, &target_convertor);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    ret = ompi_osc_portals4_get_dt(target_datatype, &ptl_dt);
    if (OMPI_SUCCESS != ret) {
        opal_output(ompi_osc_base_framework.framework_output,
                "datatype is not currently supported");
        return OMPI_ERR_NOT_SUPPORTED;
    }
    ret = ompi_osc_portals4_get_op(op, &ptl_op);
    if (OMPI_SUCCESS != ret) {
        opal_output(ompi_osc_base_framework.framework_output,
                "operation is not currently supported");
        return OMPI_ERR_NOT_SUPPORTED;
    }

    origin_iov_index = 0;
    origin_iov_count = 0;

    do {
        /* decode segments of the remote data */
        target_iov_count = OSC_PORTALS4_IOVEC_MAX;
        target_iov_index = 0;

        /* opal_convertor_raw returns done when it has reached the end of the data */
        done = opal_convertor_raw (&target_convertor, target_iovec, &target_iov_count, &target_size);

        /* loop on the target segments until we have exhaused the decoded source data */
        while (target_iov_index != target_iov_count) {
            if (origin_iov_index == origin_iov_count) {
                /* decode segments of the target buffer */
                origin_iov_count = OSC_PORTALS4_IOVEC_MAX;
                origin_iov_index = 0;
                (void) opal_convertor_raw (&origin_convertor, origin_iovec, &origin_iov_count, &origin_size);
            }

            /* we already checked that the target was large enough. this should be impossible */
            assert (0 != origin_iov_count);

            /* determine how much to transfer in this operation */
            atomic_len = MIN(MIN(origin_iovec[origin_iov_index].iov_len, target_iovec[target_iov_index].iov_len), module->atomic_max);

            opal_atomic_add_fetch_64(&module->opcount, 1);

            OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                             "performing rdma on contiguous region. local: %p, remote: %p, len: %lu",
                             origin_iovec[origin_iov_index].iov_base, target_iovec[target_iov_index].iov_base,
                             (unsigned long) target_iovec[target_iov_index].iov_len));

            OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                         "%s,%d Atomic", __FUNCTION__, __LINE__));
            ret = PtlAtomic(md_h,
                            (ptl_size_t)origin_iovec[origin_iov_index].iov_base,
                            atomic_len,
                            PTL_ACK_REQ,
                            peer,
                            pt_index,
                            match_bits,
                            offset + (ptl_size_t)target_iovec[target_iov_index].iov_base,
                            user_ptr,
                            0,
                            ptl_op,
                            ptl_dt);
            if (OPAL_UNLIKELY(PTL_OK != ret)) {
                opal_atomic_add_fetch_64(&module->opcount, -1);
                return ret;
            }

            /* adjust io vectors */
            origin_iovec[origin_iov_index].iov_len -= atomic_len;
            target_iovec[target_iov_index].iov_len -= atomic_len;
            origin_iovec[origin_iov_index].iov_base = (void *)((intptr_t) origin_iovec[origin_iov_index].iov_base + atomic_len);
            target_iovec[target_iov_index].iov_base = (void *)((intptr_t) target_iovec[target_iov_index].iov_base + atomic_len);

            origin_iov_index += (0 == origin_iovec[origin_iov_index].iov_len);
            target_iov_index += (0 == target_iovec[target_iov_index].iov_len);
        }
    } while (!done);

    return OMPI_SUCCESS;
}

/* get from a noncontiguous remote to an (non)contiguous local */
static int
get_from_noncontig(int64_t          *opcount,
                   ptl_handle_md_t   md_h,
                   const void       *origin_address,
                   int               origin_count,
                   ompi_datatype_t  *origin_datatype,
                   ptl_process_t     peer,
                   int               target_count,
                   ompi_datatype_t  *target_datatype,
                   size_t            offset,
                   ptl_pt_index_t    pt_index,
                   ptl_match_bits_t  match_bits,
                   void             *user_ptr)
{
    struct iovec origin_iovec[OSC_PORTALS4_IOVEC_MAX], target_iovec[OSC_PORTALS4_IOVEC_MAX];
    opal_convertor_t origin_convertor, target_convertor;
    uint32_t origin_iov_count, target_iov_count;
    uint32_t origin_iov_index, target_iov_index;
    /* needed for opal_convertor_raw but not used */
    size_t origin_size, target_size, rdma_len;
    size_t max_rdma_len = mca_osc_portals4_component.ptl_max_msg_size;
    int ret;
    bool done;

    /* prepare convertors for the source and target. these convertors will be used to determine the
     * contiguous segments within the source and target. */
    OBJ_CONSTRUCT(&origin_convertor, opal_convertor_t);
    ret = opal_convertor_copy_and_prepare_for_send (ompi_mpi_local_convertor, &origin_datatype->super, origin_count,
                                                    (void*)origin_address, 0, &origin_convertor);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    OBJ_CONSTRUCT(&target_convertor, opal_convertor_t);
    ret = opal_convertor_copy_and_prepare_for_send (ompi_mpi_local_convertor, &target_datatype->super, target_count,
                                                    (void *)NULL, 0, &target_convertor);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    origin_iov_index = 0;
    origin_iov_count = 0;

    do {
        /* decode segments of the remote data */
        target_iov_count = OSC_PORTALS4_IOVEC_MAX;
        target_iov_index = 0;

        /* opal_convertor_raw returns done when it has reached the end of the data */
        done = opal_convertor_raw (&target_convertor, target_iovec, &target_iov_count, &target_size);

        /* loop on the target segments until we have exhaused the decoded source data */
        while (target_iov_index != target_iov_count) {
            if (origin_iov_index == origin_iov_count) {
                /* decode segments of the target buffer */
                origin_iov_count = OSC_PORTALS4_IOVEC_MAX;
                origin_iov_index = 0;
                (void) opal_convertor_raw (&origin_convertor, origin_iovec, &origin_iov_count, &origin_size);
            }

            /* we already checked that the target was large enough. this should be impossible */
            assert (0 != origin_iov_count);

            /* determine how much to transfer in this operation */
            rdma_len = MIN(MIN(origin_iovec[origin_iov_index].iov_len, target_iovec[target_iov_index].iov_len), max_rdma_len);

            opal_atomic_add_fetch_64(opcount, 1);

            OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                             "performing rdma on contiguous region. local: %p, remote: %p, len: %lu",
                             origin_iovec[origin_iov_index].iov_base, target_iovec[target_iov_index].iov_base,
                             (unsigned long) target_iovec[target_iov_index].iov_len));

            ret = PtlGet(md_h,
                         (ptl_size_t)origin_iovec[origin_iov_index].iov_base,
                         rdma_len,
                         peer,
                         pt_index,
                         match_bits,
                         offset + (ptl_size_t)target_iovec[target_iov_index].iov_base,
                         user_ptr);
            if (OPAL_UNLIKELY(PTL_OK != ret)) {
                opal_atomic_add_fetch_64(opcount, -1);
                return ret;
            }

            /* adjust io vectors */
            origin_iovec[origin_iov_index].iov_len -= rdma_len;
            target_iovec[target_iov_index].iov_len -= rdma_len;
            origin_iovec[origin_iov_index].iov_base = (void *)((intptr_t) origin_iovec[origin_iov_index].iov_base + rdma_len);
            target_iovec[target_iov_index].iov_base = (void *)((intptr_t) target_iovec[target_iov_index].iov_base + rdma_len);

            origin_iov_index += (0 == origin_iovec[origin_iov_index].iov_len);
            target_iov_index += (0 == target_iovec[target_iov_index].iov_len);
        }
    } while (!done);

    return OMPI_SUCCESS;
}

/* get from a noncontiguous remote to an (non)contiguous local */
static int
atomic_get_from_noncontig(ompi_osc_portals4_module_t *module,
                          ptl_handle_md_t   md_h,
                          const void       *origin_address,
                          int               origin_count,
                          ompi_datatype_t  *origin_datatype,
                          ptl_process_t     peer,
                          int               target_count,
                          ompi_datatype_t  *target_datatype,
                          size_t            offset,
                          ptl_pt_index_t    pt_index,
                          ptl_match_bits_t  match_bits,
                          void             *user_ptr)
{
    struct iovec origin_iovec[OSC_PORTALS4_IOVEC_MAX], target_iovec[OSC_PORTALS4_IOVEC_MAX];
    opal_convertor_t origin_convertor, target_convertor;
    uint32_t origin_iov_count, target_iov_count;
    uint32_t origin_iov_index, target_iov_index;
    /* needed for opal_convertor_raw but not used */
    size_t origin_size, target_size, rdma_len;
    size_t max_rdma_len = module->fetch_atomic_max;
    int ret;
    bool done;

    /* prepare convertors for the source and target. these convertors will be used to determine the
     * contiguous segments within the source and target. */
    OBJ_CONSTRUCT(&origin_convertor, opal_convertor_t);
    ret = opal_convertor_copy_and_prepare_for_send (ompi_mpi_local_convertor, &origin_datatype->super, origin_count,
                                                    (void*)origin_address, 0, &origin_convertor);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    OBJ_CONSTRUCT(&target_convertor, opal_convertor_t);
    ret = opal_convertor_copy_and_prepare_for_send (ompi_mpi_local_convertor, &target_datatype->super, target_count,
                                                    (void *)NULL, 0, &target_convertor);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    origin_iov_index = 0;
    origin_iov_count = 0;

    do {
        /* decode segments of the remote data */
        target_iov_count = OSC_PORTALS4_IOVEC_MAX;
        target_iov_index = 0;

        /* opal_convertor_raw returns done when it has reached the end of the data */
        done = opal_convertor_raw (&target_convertor, target_iovec, &target_iov_count, &target_size);

        /* loop on the target segments until we have exhaused the decoded source data */
        while (target_iov_index != target_iov_count) {
            if (origin_iov_index == origin_iov_count) {
                /* decode segments of the target buffer */
                origin_iov_count = OSC_PORTALS4_IOVEC_MAX;
                origin_iov_index = 0;
                (void) opal_convertor_raw (&origin_convertor, origin_iovec, &origin_iov_count, &origin_size);
            }

            /* we already checked that the target was large enough. this should be impossible */
            assert (0 != origin_iov_count);

            /* determine how much to transfer in this operation */
            rdma_len = MIN(MIN(origin_iovec[origin_iov_index].iov_len, target_iovec[target_iov_index].iov_len), max_rdma_len);

            opal_atomic_add_fetch_64(&module->opcount, 1);

            OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                             "performing rdma on contiguous region. local: %p, remote: %p, len: %lu",
                             origin_iovec[origin_iov_index].iov_base, target_iovec[target_iov_index].iov_base,
                             (unsigned long) target_iovec[target_iov_index].iov_len));

            ret = PtlGet(md_h,
                         (ptl_size_t)origin_iovec[origin_iov_index].iov_base,
                         rdma_len,
                         peer,
                         pt_index,
                         match_bits,
                         offset + (ptl_size_t)target_iovec[target_iov_index].iov_base,
                         user_ptr);
            if (OPAL_UNLIKELY(PTL_OK != ret)) {
                opal_atomic_add_fetch_64(&module->opcount, -1);
                return ret;
            }

            /* adjust io vectors */
            origin_iovec[origin_iov_index].iov_len -= rdma_len;
            target_iovec[target_iov_index].iov_len -= rdma_len;
            origin_iovec[origin_iov_index].iov_base = (void *)((intptr_t) origin_iovec[origin_iov_index].iov_base + rdma_len);
            target_iovec[target_iov_index].iov_base = (void *)((intptr_t) target_iovec[target_iov_index].iov_base + rdma_len);

            origin_iov_index += (0 == origin_iovec[origin_iov_index].iov_len);
            target_iov_index += (0 == target_iovec[target_iov_index].iov_len);
        }
    } while (!done);

    return OMPI_SUCCESS;
}

/* swap from a noncontiguous remote to an (non)contiguous local */
static int
swap_from_noncontig(ompi_osc_portals4_module_t *module,
                    ptl_handle_md_t   result_md_h,
                    const void       *result_address,
                    int               result_count,
                    ompi_datatype_t  *result_datatype,
                    ptl_handle_md_t   origin_md_h,
                    const void       *origin_address,
                    int               origin_count,
                    ompi_datatype_t  *origin_datatype,
                    ptl_process_t     peer,
                    int               target_count,
                    ompi_datatype_t  *target_datatype,
                    size_t            offset,
                    ptl_pt_index_t    pt_index,
                    ptl_match_bits_t  match_bits,
                    void             *user_ptr)
{
    struct iovec result_iovec[OSC_PORTALS4_IOVEC_MAX], origin_iovec[OSC_PORTALS4_IOVEC_MAX], target_iovec[OSC_PORTALS4_IOVEC_MAX];
    opal_convertor_t result_convertor, origin_convertor, target_convertor;
    uint32_t result_iov_count, origin_iov_count, target_iov_count;
    uint32_t result_iov_index, origin_iov_index, target_iov_index;
    /* needed for opal_convertor_raw but not used */
    size_t result_size, origin_size, target_size, rdma_len;
    size_t max_rdma_len = module->fetch_atomic_max;
    ptl_datatype_t ptl_dt;

    int ret;
    bool done;

    /* prepare convertors for the result, source and target. these convertors will be used to determine the
     * contiguous segments within the source and target. */
    OBJ_CONSTRUCT(&result_convertor, opal_convertor_t);
    ret = opal_convertor_copy_and_prepare_for_send (ompi_mpi_local_convertor, &result_datatype->super, result_count,
                                                    (void*)result_address, 0, &result_convertor);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    OBJ_CONSTRUCT(&origin_convertor, opal_convertor_t);
    ret = opal_convertor_copy_and_prepare_for_send (ompi_mpi_local_convertor, &origin_datatype->super, origin_count,
                                                    (void*)origin_address, 0, &origin_convertor);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    OBJ_CONSTRUCT(&target_convertor, opal_convertor_t);
    ret = opal_convertor_copy_and_prepare_for_send (ompi_mpi_local_convertor, &target_datatype->super, target_count,
                                                    (void *)NULL, 0, &target_convertor);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    ret = ompi_osc_portals4_get_dt(target_datatype, &ptl_dt);
    if (OMPI_SUCCESS != ret) {
        opal_output(ompi_osc_base_framework.framework_output,
                "datatype is not currently supported");
        return OMPI_ERR_NOT_SUPPORTED;
    }

    result_iov_index = 0;
    result_iov_count = 0;
    origin_iov_index = 0;
    origin_iov_count = 0;

    do {
        /* decode segments of the remote data */
        target_iov_count = OSC_PORTALS4_IOVEC_MAX;
        target_iov_index = 0;

        /* opal_convertor_raw returns done when it has reached the end of the data */
        done = opal_convertor_raw (&target_convertor, target_iovec, &target_iov_count, &target_size);

        /* loop on the target segments until we have exhaused the decoded source data */
        while (target_iov_index != target_iov_count) {
            if (result_iov_index == result_iov_count) {
                /* decode segments of the target buffer */
                result_iov_count = OSC_PORTALS4_IOVEC_MAX;
                result_iov_index = 0;
                (void) opal_convertor_raw (&result_convertor, result_iovec, &result_iov_count, &result_size);
            }
            if (origin_iov_index == origin_iov_count) {
                /* decode segments of the target buffer */
                origin_iov_count = OSC_PORTALS4_IOVEC_MAX;
                origin_iov_index = 0;
                (void) opal_convertor_raw (&origin_convertor, origin_iovec, &origin_iov_count, &origin_size);
            }

            /* we already checked that the target was large enough. this should be impossible */
            assert (0 != result_iov_count);
            assert (0 != origin_iov_count);

            /* determine how much to transfer in this operation */
            rdma_len = MIN(MIN(origin_iovec[origin_iov_index].iov_len, target_iovec[target_iov_index].iov_len), max_rdma_len);

            opal_atomic_add_fetch_64(&module->opcount, 1);

            OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                             "performing swap on contiguous region. result: %p origin: %p, target: %p, len: %lu",
                             result_iovec[result_iov_index].iov_base,
                             origin_iovec[origin_iov_index].iov_base,
                             target_iovec[target_iov_index].iov_base,
                             (unsigned long) target_iovec[target_iov_index].iov_len));

            ret = PtlSwap(result_md_h,
                          (ptl_size_t)result_iovec[result_iov_index].iov_base,
                          origin_md_h,
                          (ptl_size_t)origin_iovec[origin_iov_index].iov_base,
                          rdma_len,
                          peer,
                          pt_index,
                          match_bits,
                          offset + (ptl_size_t)target_iovec[target_iov_index].iov_base,
                          user_ptr,
                          0,
                          NULL,
                          PTL_SWAP,
                          ptl_dt);
            if (PTL_OK != ret) {
                opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                                     "%s:%d PtlSwap failed with return value %d",
                                     __FUNCTION__, __LINE__, ret);
                opal_atomic_add_fetch_64(&module->opcount, -1);
                return ret;
            }

            /* adjust io vectors */
            result_iovec[result_iov_index].iov_len -= rdma_len;
            origin_iovec[origin_iov_index].iov_len -= rdma_len;
            target_iovec[target_iov_index].iov_len -= rdma_len;
            result_iovec[result_iov_index].iov_base = (void *)((intptr_t) result_iovec[result_iov_index].iov_base + rdma_len);
            origin_iovec[origin_iov_index].iov_base = (void *)((intptr_t) origin_iovec[origin_iov_index].iov_base + rdma_len);
            target_iovec[target_iov_index].iov_base = (void *)((intptr_t) target_iovec[target_iov_index].iov_base + rdma_len);

            result_iov_index += (0 == result_iovec[result_iov_index].iov_len);
            origin_iov_index += (0 == origin_iovec[origin_iov_index].iov_len);
            target_iov_index += (0 == target_iovec[target_iov_index].iov_len);
        }
    } while (!done);

    return OMPI_SUCCESS;
}

/* swap from a noncontiguous remote to an (non)contiguous local */
static int
fetch_atomic_from_noncontig(ompi_osc_portals4_module_t *module,
                            ptl_handle_md_t   result_md_h,
                            const void       *result_address,
                            int               result_count,
                            ompi_datatype_t  *result_datatype,
                            ptl_handle_md_t   origin_md_h,
                            const void       *origin_address,
                            int               origin_count,
                            ompi_datatype_t  *origin_datatype,
                            ptl_process_t     peer,
                            int               target_count,
                            ompi_datatype_t  *target_datatype,
                            size_t            offset,
                            ptl_pt_index_t    pt_index,
                            ptl_match_bits_t  match_bits,
                            struct ompi_op_t *op,
                            void             *user_ptr)
{
    struct iovec result_iovec[OSC_PORTALS4_IOVEC_MAX], origin_iovec[OSC_PORTALS4_IOVEC_MAX], target_iovec[OSC_PORTALS4_IOVEC_MAX];
    opal_convertor_t result_convertor, origin_convertor, target_convertor;
    uint32_t result_iov_count, origin_iov_count, target_iov_count;
    uint32_t result_iov_index, origin_iov_index, target_iov_index;
    /* needed for opal_convertor_raw but not used */
    size_t result_size, origin_size, target_size, rdma_len;
    size_t max_rdma_len = module->fetch_atomic_max;
    ptl_op_t ptl_op;
    ptl_datatype_t ptl_dt;

    int ret;
    bool done;

    /* prepare convertors for the result, source and target. these convertors will be used to determine the
     * contiguous segments within the source and target. */
    OBJ_CONSTRUCT(&result_convertor, opal_convertor_t);
    ret = opal_convertor_copy_and_prepare_for_send (ompi_mpi_local_convertor, &result_datatype->super, result_count,
                                                    (void*)result_address, 0, &result_convertor);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    OBJ_CONSTRUCT(&origin_convertor, opal_convertor_t);
    ret = opal_convertor_copy_and_prepare_for_send (ompi_mpi_local_convertor, &origin_datatype->super, origin_count,
                                                    (void*)origin_address, 0, &origin_convertor);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    OBJ_CONSTRUCT(&target_convertor, opal_convertor_t);
    ret = opal_convertor_copy_and_prepare_for_send (ompi_mpi_local_convertor, &target_datatype->super, target_count,
                                                    (void *)NULL, 0, &target_convertor);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    ret = ompi_osc_portals4_get_dt(target_datatype, &ptl_dt);
    if (OMPI_SUCCESS != ret) {
        opal_output(ompi_osc_base_framework.framework_output,
                "datatype is not currently supported");
        return OMPI_ERR_NOT_SUPPORTED;
    }
    ret = ompi_osc_portals4_get_op(op, &ptl_op);
    if (OMPI_SUCCESS != ret) {
        opal_output(ompi_osc_base_framework.framework_output,
                "operation is not currently supported");
        return OMPI_ERR_NOT_SUPPORTED;
    }

    result_iov_index = 0;
    result_iov_count = 0;
    origin_iov_index = 0;
    origin_iov_count = 0;

    do {
        /* decode segments of the remote data */
        target_iov_count = OSC_PORTALS4_IOVEC_MAX;
        target_iov_index = 0;

        /* opal_convertor_raw returns done when it has reached the end of the data */
        done = opal_convertor_raw (&target_convertor, target_iovec, &target_iov_count, &target_size);

        /* loop on the target segments until we have exhaused the decoded source data */
        while (target_iov_index != target_iov_count) {
            if (result_iov_index == result_iov_count) {
                /* decode segments of the target buffer */
                result_iov_count = OSC_PORTALS4_IOVEC_MAX;
                result_iov_index = 0;
                (void) opal_convertor_raw (&result_convertor, result_iovec, &result_iov_count, &result_size);
            }
            if (origin_iov_index == origin_iov_count) {
                /* decode segments of the target buffer */
                origin_iov_count = OSC_PORTALS4_IOVEC_MAX;
                origin_iov_index = 0;
                (void) opal_convertor_raw (&origin_convertor, origin_iovec, &origin_iov_count, &origin_size);
            }

            /* we already checked that the target was large enough. this should be impossible */
            assert (0 != result_iov_count);
            assert (0 != origin_iov_count);

            /* determine how much to transfer in this operation */
            rdma_len = MIN(MIN(origin_iovec[origin_iov_index].iov_len, target_iovec[target_iov_index].iov_len), max_rdma_len);

            opal_atomic_add_fetch_64(&module->opcount, 1);

            OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                             "performing swap on contiguous region. result: %p origin: %p, target: %p, len: %lu",
                             result_iovec[result_iov_index].iov_base,
                             origin_iovec[origin_iov_index].iov_base,
                             target_iovec[target_iov_index].iov_base,
                             (unsigned long) target_iovec[target_iov_index].iov_len));

            ret = PtlFetchAtomic(result_md_h,
                                 (ptl_size_t)result_iovec[result_iov_index].iov_base,
                                 origin_md_h,
                                 (ptl_size_t)origin_iovec[origin_iov_index].iov_base,
                                 rdma_len,
                                 peer,
                                 pt_index,
                                 match_bits,
                                 offset + (ptl_size_t)target_iovec[target_iov_index].iov_base,
                                 user_ptr,
                                 0,
                                 ptl_op,
                                 ptl_dt);
            if (PTL_OK != ret) {
                opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                                     "%s:%d PtlFetchAtomic failed with return value %d",
                                     __FUNCTION__, __LINE__, ret);
                opal_atomic_add_fetch_64(&module->opcount, -1);
                return ret;
            }

            /* adjust io vectors */
            result_iovec[result_iov_index].iov_len -= rdma_len;
            origin_iovec[origin_iov_index].iov_len -= rdma_len;
            target_iovec[target_iov_index].iov_len -= rdma_len;
            result_iovec[result_iov_index].iov_base = (void *)((intptr_t) result_iovec[result_iov_index].iov_base + rdma_len);
            origin_iovec[origin_iov_index].iov_base = (void *)((intptr_t) origin_iovec[origin_iov_index].iov_base + rdma_len);
            target_iovec[target_iov_index].iov_base = (void *)((intptr_t) target_iovec[target_iov_index].iov_base + rdma_len);

            result_iov_index += (0 == result_iovec[result_iov_index].iov_len);
            origin_iov_index += (0 == origin_iovec[origin_iov_index].iov_len);
            target_iov_index += (0 == target_iovec[target_iov_index].iov_len);
        }
    } while (!done);

    return OMPI_SUCCESS;
}

int
ompi_osc_portals4_rput(const void *origin_addr,
                       int origin_count,
                       struct ompi_datatype_t *origin_dt,
                       int target,
                       ptrdiff_t target_disp,
                       int target_count,
                       struct ompi_datatype_t *target_dt,
                       struct ompi_win_t *win,
                       struct ompi_request_t **ompi_req)
{
    int ret;
    ompi_osc_portals4_request_t *request;
    ompi_osc_portals4_module_t *module =
        (ompi_osc_portals4_module_t*) win->w_osc_module;
    ptl_process_t peer = ompi_osc_portals4_get_peer(module, target);
    size_t size, offset;
    ptrdiff_t length, origin_lb, target_lb, extent;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "rput: 0x%lx, %d, %s, %d, %lu, %d, %s, 0x%lx",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, target, (unsigned long) target_disp,
                         target_count, target_dt->name,
                         (unsigned long) win));

    OMPI_OSC_PORTALS4_REQUEST_ALLOC(win, request);
    if (NULL == request) return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    *ompi_req = &request->super;

    offset = get_displacement(module, target) * target_disp;

    if (!ompi_datatype_is_contiguous_memory_layout(target_dt, target_count)) {
        ret = put_to_noncontig(&module->opcount,
                               module->req_md_h,
                               origin_addr,
                               origin_count,
                               origin_dt,
                               peer,
                               target_count,
                               target_dt,
                               offset,
                               module->pt_idx,
                               module->match_bits,
                               request);
        if (PTL_OK != ret) {
            OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
            OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                         "%s,%d put_to_noncontig() failed: ret = %d",
                         __FUNCTION__, __LINE__, ret));
            return ret;
        }
    } else if (!ompi_datatype_is_contiguous_memory_layout(origin_dt, origin_count)) {
        ret = put_from_iovec(module,
                             origin_addr,
                             origin_count,
                             origin_dt,
                             peer,
                             target_count,
                             target_dt,
                             offset,
                             module->pt_idx,
                             module->match_bits,
                             request);
        if (PTL_OK != ret) {
            OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
            OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                         "%s,%d put_from_iovec() failed: ret = %d",
                         __FUNCTION__, __LINE__, ret));
            return ret;
        }
    } else {
        ret = ompi_datatype_get_true_extent(origin_dt, &origin_lb, &extent);
        if (OMPI_SUCCESS != ret) {
            OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
            return ret;
        }
        ret = ompi_datatype_get_true_extent(target_dt, &target_lb, &extent);
        if (OMPI_SUCCESS != ret) {
            OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
            return ret;
        }
        ompi_datatype_type_size(origin_dt, &size);
        length = size * origin_count;

        request->ops_expected += number_of_fragments(length, mca_osc_portals4_component.ptl_max_msg_size);

        OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                     "%s,%d RPut(origin_count=%d, origin_lb=%lu, target_count=%d, target_lb=%lu, length=%lu, op_count=%ld)",
                     __FUNCTION__, __LINE__, origin_count, origin_lb, target_count, target_lb, length, module->opcount));
        ret = segmentedPut(&module->opcount,
                           module->req_md_h,
                           (ptl_size_t) origin_addr + origin_lb,
                           length,
                           mca_osc_portals4_component.ptl_max_msg_size,
                           PTL_ACK_REQ,
                           peer,
                           module->pt_idx,
                           module->match_bits,
                           offset + target_lb,
                           request,
                           0);
        if (OMPI_SUCCESS != ret) {
            OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
            return ret;
        }
    }

    return OMPI_SUCCESS;
}


int
ompi_osc_portals4_rget(void *origin_addr,
                       int origin_count,
                       struct ompi_datatype_t *origin_dt,
                       int target,
                       ptrdiff_t target_disp,
                       int target_count,
                       struct ompi_datatype_t *target_dt,
                       struct ompi_win_t *win,
                       struct ompi_request_t **ompi_req)
{
    int ret;
    ompi_osc_portals4_request_t *request;
    ompi_osc_portals4_module_t *module =
        (ompi_osc_portals4_module_t*) win->w_osc_module;
    ptl_process_t peer = ompi_osc_portals4_get_peer(module, target);
    size_t offset, size;
    ptrdiff_t length, origin_lb, target_lb, extent;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "rget: 0x%lx, %d, %s, %d, %lu, %d, %s, 0x%lx",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, target, (unsigned long) target_disp,
                         target_count, target_dt->name,
                         (unsigned long) win));

    OMPI_OSC_PORTALS4_REQUEST_ALLOC(win, request);
    if (NULL == request) return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    *ompi_req = &request->super;

    offset = get_displacement(module, target) * target_disp;

    if (!ompi_datatype_is_contiguous_memory_layout(target_dt, target_count)) {
        ret = get_from_noncontig(&module->opcount,
                                 module->req_md_h,
                                 origin_addr,
                                 origin_count,
                                 origin_dt,
                                 peer,
                                 target_count,
                                 target_dt,
                                 offset,
                                 module->pt_idx,
                                 module->match_bits,
                                 request);
        if (PTL_OK != ret) {
            OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                         "%s,%d get_from_noncontig() failed: ret = %d",
                         __FUNCTION__, __LINE__, ret));
            return ret;
        }
    } else if (!ompi_datatype_is_contiguous_memory_layout(origin_dt, origin_count)) {
        ret = get_to_iovec(module,
                           origin_addr,
                           origin_count,
                           origin_dt,
                           peer,
                           target_count,
                           target_dt,
                           offset,
                           module->pt_idx,
                           module->match_bits,
                           request);
        if (PTL_OK != ret) {
            OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                         "%s,%d get_to_iovec() failed: ret = %d",
                         __FUNCTION__, __LINE__, ret));
            return ret;
        }
    } else {
        ret = ompi_datatype_get_true_extent(origin_dt, &origin_lb, &extent);
        if (OMPI_SUCCESS != ret) {
            return ret;
        }
        ret = ompi_datatype_get_true_extent(target_dt, &target_lb, &extent);
        if (OMPI_SUCCESS != ret) {
            return ret;
        }
        ompi_datatype_type_size(origin_dt, &size);
        length = size * origin_count;

        request->ops_expected += number_of_fragments(length, mca_osc_portals4_component.ptl_max_msg_size);

        OPAL_OUTPUT_VERBOSE((90,ompi_osc_base_framework.framework_output,
                              "%s,%d RGet", __FUNCTION__, __LINE__));
        ret = segmentedGet(&module->opcount,
                           module->req_md_h,
                           (ptl_size_t) origin_addr + origin_lb,
                           length,
                           mca_osc_portals4_component.ptl_max_msg_size,
                           peer,
                           module->pt_idx,
                           module->match_bits,
                           offset + target_lb,
                           request);
        if (OMPI_SUCCESS != ret) {
            OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
            return ret;
        }
    }

    return OMPI_SUCCESS;
}


int
ompi_osc_portals4_raccumulate(const void *origin_addr,
                              int origin_count,
                              struct ompi_datatype_t *origin_dt,
                              int target,
                              ptrdiff_t target_disp,
                              int target_count,
                              struct ompi_datatype_t *target_dt,
                              struct ompi_op_t *op,
                              struct ompi_win_t *win,
                              struct ompi_request_t **ompi_req)
{
    int ret;
    ompi_osc_portals4_request_t *request;
    ompi_osc_portals4_module_t *module =
        (ompi_osc_portals4_module_t*) win->w_osc_module;
    ptl_process_t peer = ompi_osc_portals4_get_peer(module, target);
    size_t offset, size;
    ptl_op_t ptl_op;
    ptl_datatype_t ptl_dt;
    ptrdiff_t sent, length, origin_lb, target_lb, extent;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "raccumulate: 0x%lx, %d, %s, %d, %lu, %d, %s, %s 0x%lx",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, target, (unsigned long) target_disp,
                         target_count, target_dt->name,
                         op->o_name,
                         (unsigned long) win));

    OMPI_OSC_PORTALS4_REQUEST_ALLOC(win, request);
    if (NULL == request) return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    *ompi_req = &request->super;

    offset = get_displacement(module, target) * target_disp;

    if (!ompi_datatype_is_contiguous_memory_layout(target_dt, target_count)) {
        if (MPI_REPLACE == op) {
            ret = atomic_put_to_noncontig(module,
                                          module->req_md_h,
                                          origin_addr,
                                          origin_count,
                                          origin_dt,
                                          peer,
                                          target_count,
                                          target_dt,
                                          offset,
                                          module->pt_idx,
                                          module->match_bits,
                                          request);
            if (PTL_OK != ret) {
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                    "%s,%d atomic_put_to_noncontig() failed: ret = %d",
                    __FUNCTION__, __LINE__, ret));
                return ret;
            }
        } else {
            ret = atomic_to_noncontig(module,
                                      module->req_md_h,
                                      origin_addr,
                                      origin_count,
                                      origin_dt,
                                      peer,
                                      target_count,
                                      target_dt,
                                      offset,
                                      module->pt_idx,
                                      module->match_bits,
                                      op,
                                      request);
            if (PTL_OK != ret) {
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                    "%s,%d atomic_to_noncontig() failed: ret = %d",
                    __FUNCTION__, __LINE__, ret));
                return ret;
            }
        }
    } else if (!ompi_datatype_is_contiguous_memory_layout(origin_dt, origin_count)) {
        if (MPI_REPLACE == op) {
            ret = atomic_put_from_iovec(module,
                                        origin_addr,
                                        origin_count,
                                        origin_dt,
                                        peer,
                                        target_count,
                                        target_dt,
                                        offset,
                                        module->pt_idx,
                                        module->match_bits,
                                        request);
            if (PTL_OK != ret) {
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                    "%s,%d atomic_put_from_iovec() failed: ret = %d",
                    __FUNCTION__, __LINE__, ret));
                return ret;
            }
        } else {
            ret = atomic_from_iovec(module,
                                    origin_addr,
                                    origin_count,
                                    origin_dt,
                                    peer,
                                    target_count,
                                    target_dt,
                                    offset,
                                    module->pt_idx,
                                    module->match_bits,
                                    op,
                                    request);
            if (PTL_OK != ret) {
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                    "%s,%d atomic_from_iovec() failed: ret = %d",
                    __FUNCTION__, __LINE__, ret));
                return ret;
            }
        }
    } else {
        ptl_size_t md_offset;

        ret = ompi_datatype_get_true_extent(origin_dt, &origin_lb, &extent);
        if (OMPI_SUCCESS != ret) {
            OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
            return ret;
        }
        ret = ompi_datatype_get_true_extent(target_dt, &target_lb, &extent);
        if (OMPI_SUCCESS != ret) {
            OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
            return ret;
        }
        ompi_datatype_type_size(origin_dt, &size);
        length = size * origin_count;
        sent = 0;

        md_offset = (ptl_size_t) origin_addr;

        request->ops_expected += number_of_fragments(length, module->atomic_max);

        if (MPI_REPLACE == op) {
            OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                                 "%s,%d Put", __FUNCTION__, __LINE__));
            ret = segmentedPut(&module->opcount,
                               module->req_md_h,
                               md_offset + origin_lb,
                               length,
                               module->atomic_max,
                               PTL_ACK_REQ,
                               peer,
                               module->pt_idx,
                               module->match_bits,
                               offset + target_lb,
                               request,
                               0);
            if (OMPI_SUCCESS != ret) {
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                return ret;
            }
        } else {
            ret = ompi_osc_portals4_get_dt(origin_dt, &ptl_dt);
            if (OMPI_SUCCESS != ret) {
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                opal_output(ompi_osc_base_framework.framework_output,
                        "datatype is not currently supported");
                return OMPI_ERR_NOT_SUPPORTED;
            }
            ret = ompi_osc_portals4_get_op(op, &ptl_op);
            if (OMPI_SUCCESS != ret) {
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                opal_output(ompi_osc_base_framework.framework_output,
                        "operation is not currently supported");
                return OMPI_ERR_NOT_SUPPORTED;
            }
            do {
                size_t msg_length = MIN(module->atomic_max, length - sent);

                (void)opal_atomic_add_fetch_64(&module->opcount, 1);

                OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                             "%s,%d Atomic", __FUNCTION__, __LINE__));
                ret = PtlAtomic(module->req_md_h,
                                md_offset + sent + origin_lb,
                                msg_length,
                                PTL_ACK_REQ,
                                peer,
                                module->pt_idx,
                                module->match_bits,
                                offset + sent + target_lb,
                                request,
                                0,
                                ptl_op,
                                ptl_dt);
                if (OMPI_SUCCESS != ret) {
                    (void)opal_atomic_add_fetch_64(&module->opcount, -1);
                    OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                    return ret;
                }
                sent += msg_length;
            } while (sent < length);
        }
    }

    return OMPI_SUCCESS;
}


int
ompi_osc_portals4_rget_accumulate(const void *origin_addr,
                                  int origin_count,
                                  struct ompi_datatype_t *origin_dt,
                                  void *result_addr,
                                  int result_count,
                                  struct ompi_datatype_t *result_dt,
                                  int target,
                                  ptrdiff_t target_disp,
                                  int target_count,
                                  struct ompi_datatype_t *target_dt,
                                  struct ompi_op_t *op,
                                  struct ompi_win_t *win,
                                  struct ompi_request_t **ompi_req)
{
    int ret;
    ompi_osc_portals4_request_t *request;
    ompi_osc_portals4_module_t *module =
        (ompi_osc_portals4_module_t*) win->w_osc_module;
    ptl_process_t peer = ompi_osc_portals4_get_peer(module, target);
    size_t target_offset, size;
    ptl_op_t ptl_op;
    ptl_datatype_t ptl_dt;
    ptrdiff_t length, origin_lb, target_lb, result_lb, extent;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "rget_accumulate: 0x%lx, %d, %s, 0x%lx, %d, %s, %d, %lu, %d, %s, %s, 0x%lx",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, (unsigned long) result_addr,
                         result_count, result_dt->name,
                         target, (unsigned long) target_disp,
                         target_count, target_dt->name,
                         op->o_name,
                         (unsigned long) win));

    OMPI_OSC_PORTALS4_REQUEST_ALLOC(win, request);
    if (NULL == request) return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    *ompi_req = &request->super;

    target_offset = get_displacement(module, target) * target_disp;

    if (target_count > 0 && !ompi_datatype_is_contiguous_memory_layout(target_dt, target_count)) {
        if (MPI_REPLACE == op) {
            OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                                 "rget_accumulate: MPI_REPLACE  non-contiguous target"));
            ret = swap_from_noncontig(module,
                                      module->req_md_h,
                                      result_addr,
                                      result_count,
                                      result_dt,
                                      module->md_h,
                                      origin_addr,
                                      origin_count,
                                      origin_dt,
                                      peer,
                                      target_count,
                                      target_dt,
                                      target_offset,
                                      module->pt_idx,
                                      module->match_bits,
                                      request);
            if (PTL_OK != ret) {
                OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                             "%s,%d swap_from_noncontig() failed: ret = %d",
                             __FUNCTION__, __LINE__, ret));
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                return ret;
            }
        } else if (MPI_NO_OP == op) {
            OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                                 "rget_accumulate: MPI_NO_OP  non-contiguous target"));
            ret = atomic_get_from_noncontig(module,
                                            module->req_md_h,
                                            result_addr,
                                            result_count,
                                            result_dt,
                                            peer,
                                            target_count,
                                            target_dt,
                                            target_offset,
                                            module->pt_idx,
                                            module->match_bits,
                                            request);
            if (PTL_OK != ret) {
                OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                             "%s,%d atomic_get_from_noncontig() failed: ret = %d",
                             __FUNCTION__, __LINE__, ret));
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                return ret;
            }
        } else {
            OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                                 "rget_accumulate: other-op  non-contiguous target"));
            ret = fetch_atomic_from_noncontig(module,
                                              module->req_md_h,
                                              result_addr,
                                              result_count,
                                              result_dt,
                                              module->md_h,
                                              origin_addr,
                                              origin_count,
                                              origin_dt,
                                              peer,
                                              target_count,
                                              target_dt,
                                              target_offset,
                                              module->pt_idx,
                                              module->match_bits,
                                              op,
                                              request);
            if (PTL_OK != ret) {
                OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                    "%s,%d fetch_atomic_from_noncontig() failed: ret = %d",
                    __FUNCTION__, __LINE__, ret));
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                return ret;
            }
        }
    } else if ((origin_count > 0 && !ompi_datatype_is_contiguous_memory_layout(origin_dt, origin_count)) ||
               (result_count > 0 && !ompi_datatype_is_contiguous_memory_layout(result_dt, result_count))) {
        if (MPI_REPLACE == op) {
            OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                                 "rget_accumulate: MPI_REPLACE  non-contiguous origin/result"));
            ret = swap_to_iovec(module,
                                result_addr,
                                result_count,
                                result_dt,
                                origin_addr,
                                origin_count,
                                origin_dt,
                                peer,
                                target_count,
                                target_dt,
                                target_offset,
                                module->pt_idx,
                                module->match_bits,
                                request);
            if (PTL_OK != ret) {
                OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                             "%s,%d swap_to_iovec() failed: ret = %d",
                             __FUNCTION__, __LINE__, ret));
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                return ret;
            }
        } else if (MPI_NO_OP == op) {
            OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                                 "rget_accumulate: MPI_NO_OP  non-contiguous origin/result"));
            ret = atomic_get_to_iovec(module,
                                      result_addr,
                                      result_count,
                                      result_dt,
                                      peer,
                                      target_count,
                                      target_dt,
                                      target_offset,
                                      module->pt_idx,
                                      module->match_bits,
                                      request);
            if (PTL_OK != ret) {
                OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                             "%s,%d atomic_get_to_iovec() failed: ret = %d",
                             __FUNCTION__, __LINE__, ret));
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                return ret;
            }
        } else {
            OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                                 "rget_accumulate: other-op  non-contiguous origin/result"));
            ret = fetch_atomic_to_iovec(module,
                                        result_addr,
                                        result_count,
                                        result_dt,
                                        origin_addr,
                                        origin_count,
                                        origin_dt,
                                        peer,
                                        target_count,
                                        target_dt,
                                        target_offset,
                                        module->pt_idx,
                                        module->match_bits,
                                        op,
                                        request);
            if (PTL_OK != ret) {
                OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                             "%s,%d fetch_atomic_to_iovec() failed: ret = %d",
                             __FUNCTION__, __LINE__, ret));
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                return ret;
            }
        }
    } else {
        if (MPI_REPLACE == op) {
            OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                                 "rget_accumulate: MPI_REPLACE  contiguous"));
            ptl_size_t result_md_offset, origin_md_offset;

            ret = ompi_datatype_get_true_extent(origin_dt, &origin_lb, &extent);
            if (OMPI_SUCCESS != ret) {
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                return ret;
            }
            ret = ompi_datatype_get_true_extent(target_dt, &target_lb, &extent);
            if (OMPI_SUCCESS != ret) {
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                return ret;
            }
            ret = ompi_datatype_get_true_extent(result_dt, &result_lb, &extent);
            if (OMPI_SUCCESS != ret) {
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                return ret;
            }
            ompi_datatype_type_size(origin_dt, &size);
            length = size * origin_count;

            ret = ompi_osc_portals4_get_dt(origin_dt, &ptl_dt);
            if (OMPI_SUCCESS != ret) {
                opal_output(ompi_osc_base_framework.framework_output,
                        "datatype is not currently supported");
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                return OMPI_ERR_NOT_SUPPORTED;
            }

            result_md_offset = (ptl_size_t) result_addr;
            origin_md_offset = (ptl_size_t) origin_addr;

            request->ops_expected += number_of_fragments(length, module->fetch_atomic_max);

            ret = segmentedSwap(&module->opcount,
                                module->req_md_h,
                                result_md_offset + result_lb,
                                module->md_h,
                                origin_md_offset + origin_lb,
                                length,
                                module->fetch_atomic_max,
                                peer,
                                module->pt_idx,
                                module->match_bits,
                                target_offset + target_lb,
                                request,
                                ptl_dt);
            if (OMPI_SUCCESS != ret) {
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                return ret;
            }
        } else if (MPI_NO_OP == op) {
            OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                                 "rget_accumulate: MPI_NO_OP  contiguous"));
            ptl_size_t md_offset;

            ret = ompi_datatype_get_true_extent(target_dt, &target_lb, &extent);
            if (OMPI_SUCCESS != ret) {
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                return ret;
            }
            ret = ompi_datatype_get_true_extent(result_dt, &result_lb, &extent);
            if (OMPI_SUCCESS != ret) {
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                return ret;
            }
            ompi_datatype_type_size(target_dt, &size);
            length = size * target_count;

            md_offset = (ptl_size_t) result_addr;

            request->ops_expected += number_of_fragments(length, module->fetch_atomic_max);

            OPAL_OUTPUT_VERBOSE((90,ompi_osc_base_framework.framework_output,
                                  "%s,%d MPI_Get_accumulate", __FUNCTION__, __LINE__));
            ret = segmentedGet(&module->opcount,
                               module->req_md_h,
                               (ptl_size_t) md_offset + result_lb,
                               length,
                               module->fetch_atomic_max,
                               peer,
                               module->pt_idx,
                               module->match_bits,
                               target_offset + target_lb,
                               request);
            if (OMPI_SUCCESS != ret) {
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                return ret;
            }
        } else {
            OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                                 "rget_accumulate: other-op  contiguous"));
            ptl_size_t result_md_offset, origin_md_offset;

            ret = ompi_datatype_get_true_extent(origin_dt, &origin_lb, &extent);
            if (OMPI_SUCCESS != ret) {
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                return ret;
            }
            ret = ompi_datatype_get_true_extent(target_dt, &target_lb, &extent);
            if (OMPI_SUCCESS != ret) {
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                return ret;
            }
            ret = ompi_datatype_get_true_extent(result_dt, &result_lb, &extent);
            if (OMPI_SUCCESS != ret) {
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                return ret;
            }
            ompi_datatype_type_size(origin_dt, &size);
            length = size * origin_count;

            result_md_offset = (ptl_size_t) result_addr;
            origin_md_offset = (ptl_size_t) origin_addr;

            ret = ompi_osc_portals4_get_dt(origin_dt, &ptl_dt);
            if (OMPI_SUCCESS != ret) {
                opal_output(ompi_osc_base_framework.framework_output,
                        "datatype is not currently supported");
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                return OMPI_ERR_NOT_SUPPORTED;
            }

            ret = ompi_osc_portals4_get_op(op, &ptl_op);
            if (OMPI_SUCCESS != ret) {
                opal_output(ompi_osc_base_framework.framework_output,
                        "operation is not currently supported");
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                return OMPI_ERR_NOT_SUPPORTED;
            }

            request->ops_expected += number_of_fragments(length, module->fetch_atomic_max);

            ret = segmentedFetchAtomic(&module->opcount,
                                       module->req_md_h,
                                       result_md_offset + result_lb,
                                       module->md_h,
                                       origin_md_offset + origin_lb,
                                       length,
                                       module->fetch_atomic_max,
                                       peer,
                                       module->pt_idx,
                                       module->match_bits,
                                       target_offset + target_lb,
                                       request,
                                       ptl_op,
                                       ptl_dt);
            if (OMPI_SUCCESS != ret) {
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                return ret;
            }
        }
    }

    return OMPI_SUCCESS;
}


int
ompi_osc_portals4_put(const void *origin_addr,
                      int origin_count,
                      struct ompi_datatype_t *origin_dt,
                      int target,
                      ptrdiff_t target_disp,
                      int target_count,
                      struct ompi_datatype_t *target_dt,
                      struct ompi_win_t *win)
{
    int ret;
    ompi_osc_portals4_module_t *module =
        (ompi_osc_portals4_module_t*) win->w_osc_module;
    ptl_process_t peer = ompi_osc_portals4_get_peer(module, target);
    size_t offset, size;
    ptrdiff_t length, origin_lb, target_lb, extent;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "put: 0x%lx, %d, %s, %d, %lu, %d, %s, 0x%lx",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, target, (unsigned long) target_disp,
                         target_count, target_dt->name,
                         (unsigned long) win));

    offset = get_displacement(module, target) * target_disp;

    if (!ompi_datatype_is_contiguous_memory_layout(target_dt, target_count)) {
        ret = put_to_noncontig(&module->opcount,
                               module->md_h,
                               origin_addr,
                               origin_count,
                               origin_dt,
                               peer,
                               target_count,
                               target_dt,
                               offset,
                               module->pt_idx,
                               module->match_bits,
                               NULL);
        if (PTL_OK != ret) {
            OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                         "%s,%d put_to_noncontig() failed: ret = %d",
                         __FUNCTION__, __LINE__, ret));
            return ret;
        }
    } else if (!ompi_datatype_is_contiguous_memory_layout(origin_dt, origin_count)) {
        ret = put_from_iovec(module,
                             origin_addr,
                             origin_count,
                             origin_dt,
                             peer,
                             target_count,
                             target_dt,
                             offset,
                             module->pt_idx,
                             module->match_bits,
                             NULL);
        if (PTL_OK != ret) {
            OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                         "%s,%d put_from_iovec() failed: ret = %d",
                         __FUNCTION__, __LINE__, ret));
            return ret;
        }
    } else {
        ret = ompi_datatype_get_true_extent(origin_dt, &origin_lb, &extent);
        if (OMPI_SUCCESS != ret) {
            return ret;
        }
        ret = ompi_datatype_get_true_extent(target_dt, &target_lb, &extent);
        if (OMPI_SUCCESS != ret) {
            return ret;
        }
        ompi_datatype_type_size(origin_dt, &size);
        length = size * origin_count;

        OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                     "%s,%d Put(origin_count=%d, origin_lb=%lu, target_count=%d, target_lb=%lu, length=%lu, op_count=%ld)",
                     __FUNCTION__, __LINE__, origin_count, origin_lb, target_count, target_lb, length, module->opcount));
        ret = segmentedPut(&module->opcount,
                           module->md_h,
                           (ptl_size_t) origin_addr + origin_lb,
                           length,
                           mca_osc_portals4_component.ptl_max_msg_size,
                           PTL_ACK_REQ,
                           peer,
                           module->pt_idx,
                           module->match_bits,
                           offset + target_lb,
                           NULL,
                           0);
        if (OMPI_SUCCESS != ret) {
            return ret;
        }
    }

    return OMPI_SUCCESS;
}


int
ompi_osc_portals4_get(void *origin_addr,
                      int origin_count,
                      struct ompi_datatype_t *origin_dt,
                      int target,
                      ptrdiff_t target_disp,
                      int target_count,
                      struct ompi_datatype_t *target_dt,
                      struct ompi_win_t *win)
{
    int ret;
    ompi_osc_portals4_module_t *module =
        (ompi_osc_portals4_module_t*) win->w_osc_module;
    ptl_process_t peer = ompi_osc_portals4_get_peer(module, target);
    size_t offset, size;
    ptrdiff_t length, origin_lb, target_lb, extent;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "get: 0x%lx, %d, %s, %d, %lu, %d, %s, 0x%lx",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, target, (unsigned long) target_disp,
                         target_count, target_dt->name,
                         (unsigned long) win));

    offset = get_displacement(module, target) * target_disp;

    if (!ompi_datatype_is_contiguous_memory_layout(target_dt, target_count)) {
        ret = get_from_noncontig(&module->opcount,
                                 module->md_h,
                                 origin_addr,
                                 origin_count,
                                 origin_dt,
                                 peer,
                                 target_count,
                                 target_dt,
                                 offset,
                                 module->pt_idx,
                                 module->match_bits,
                                 NULL);
        if (PTL_OK != ret) {
            OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                         "%s,%d get_from_noncontig() failed: ret = %d",
                         __FUNCTION__, __LINE__, ret));
            return ret;
        }
    } else if (!ompi_datatype_is_contiguous_memory_layout(origin_dt, origin_count)) {
        ret = get_to_iovec(module,
                           origin_addr,
                           origin_count,
                           origin_dt,
                           peer,
                           target_count,
                           target_dt,
                           offset,
                           module->pt_idx,
                           module->match_bits,
                           NULL);
        if (PTL_OK != ret) {
            OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                         "%s,%d get_to_iovec() failed: ret = %d",
                         __FUNCTION__, __LINE__, ret));
            return ret;
        }
    } else {
        ret = ompi_datatype_get_true_extent(origin_dt, &origin_lb, &extent);
        if (OMPI_SUCCESS != ret) {
            return ret;
        }
        ret = ompi_datatype_get_true_extent(target_dt, &target_lb, &extent);
        if (OMPI_SUCCESS != ret) {
            return ret;
        }
        ompi_datatype_type_size(origin_dt, &size);
        length = size * origin_count;

        OPAL_OUTPUT_VERBOSE((90,ompi_osc_base_framework.framework_output,
                              "%s,%d Get", __FUNCTION__, __LINE__));
        ret = segmentedGet(&module->opcount,
                           module->md_h,
                           (ptl_size_t) origin_addr + origin_lb,
                           length,
                           mca_osc_portals4_component.ptl_max_msg_size,
                           peer,
                           module->pt_idx,
                           module->match_bits,
                           offset + target_lb,
                           NULL);
        if (OMPI_SUCCESS != ret) {
            return ret;
        }
    }

    return OMPI_SUCCESS;
}


int
ompi_osc_portals4_accumulate(const void *origin_addr,
                             int origin_count,
                             struct ompi_datatype_t *origin_dt,
                             int target,
                             ptrdiff_t target_disp,
                             int target_count,
                             struct ompi_datatype_t *target_dt,
                             struct ompi_op_t *op,
                             struct ompi_win_t *win)
{
    int ret;
    ompi_osc_portals4_module_t *module =
        (ompi_osc_portals4_module_t*) win->w_osc_module;
    ptl_process_t peer = ompi_osc_portals4_get_peer(module, target);
    size_t offset, size;
    ptl_op_t ptl_op;
    ptl_datatype_t ptl_dt;
    ptrdiff_t sent, length, origin_lb, target_lb, extent;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "accumulate: 0x%lx, %d, %s, %d, %lu, %d, %s, %s, 0x%lx",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, target, (unsigned long) target_disp,
                         target_count, target_dt->name,
                         op->o_name,
                         (unsigned long) win));

    offset = get_displacement(module, target) * target_disp;

    if (!ompi_datatype_is_contiguous_memory_layout(target_dt, target_count)) {
        if (MPI_REPLACE == op) {
            ret = atomic_put_to_noncontig(module,
                                          module->md_h,
                                          origin_addr,
                                          origin_count,
                                          origin_dt,
                                          peer,
                                          target_count,
                                          target_dt,
                                          offset,
                                          module->pt_idx,
                                          module->match_bits,
                                          NULL);
            if (PTL_OK != ret) {
                OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                    "%s,%d atomic_put_to_noncontig() failed: ret = %d",
                    __FUNCTION__, __LINE__, ret));
                return ret;
            }
        } else {
            ret = atomic_to_noncontig(module,
                                      module->md_h,
                                      origin_addr,
                                      origin_count,
                                      origin_dt,
                                      peer,
                                      target_count,
                                      target_dt,
                                      offset,
                                      module->pt_idx,
                                      module->match_bits,
                                      op,
                                      NULL);
            if (PTL_OK != ret) {
                OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                    "%s,%d atomic_to_noncontig() failed: ret = %d",
                    __FUNCTION__, __LINE__, ret));
                return ret;
            }
        }
    } else if (!ompi_datatype_is_contiguous_memory_layout(origin_dt, origin_count)) {
        if (MPI_REPLACE == op) {
            ret = atomic_put_from_iovec(module,
                                        origin_addr,
                                        origin_count,
                                        origin_dt,
                                        peer,
                                        target_count,
                                        target_dt,
                                        offset,
                                        module->pt_idx,
                                        module->match_bits,
                                        NULL);
            if (PTL_OK != ret) {
                OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                    "%s,%d atomic_put_from_iovec() failed: ret = %d",
                    __FUNCTION__, __LINE__, ret));
                return ret;
            }
        } else {
            ret = atomic_from_iovec(module,
                                    origin_addr,
                                    origin_count,
                                    origin_dt,
                                    peer,
                                    target_count,
                                    target_dt,
                                    offset,
                                    module->pt_idx,
                                    module->match_bits,
                                    op,
                                    NULL);
            if (PTL_OK != ret) {
                OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                    "%s,%d atomic_from_iovec() failed: ret = %d",
                    __FUNCTION__, __LINE__, ret));
                return ret;
            }
        }
    } else {
        ptl_size_t md_offset;

        ret = ompi_datatype_get_true_extent(origin_dt, &origin_lb, &extent);
        if (OMPI_SUCCESS != ret) {
            return ret;
        }
        ret = ompi_datatype_get_true_extent(target_dt, &target_lb, &extent);
        if (OMPI_SUCCESS != ret) {
            return ret;
        }
        ompi_datatype_type_size(origin_dt, &size);
        length = size * origin_count;
        sent = 0;

        md_offset = (ptl_size_t) origin_addr;

        if (MPI_REPLACE == op) {
            OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                                 "%s,%d Put", __FUNCTION__, __LINE__));
            ret = segmentedPut(&module->opcount,
                               module->md_h,
                               md_offset + origin_lb,
                               length,
                               module->atomic_max,
                               PTL_ACK_REQ,
                               peer,
                               module->pt_idx,
                               module->match_bits,
                               offset + target_lb,
                               NULL,
                               0);
            if (OMPI_SUCCESS != ret) {
                return ret;
            }
        } else {
            ret = ompi_osc_portals4_get_dt(origin_dt, &ptl_dt);
            if (OMPI_SUCCESS != ret) {
                opal_output(ompi_osc_base_framework.framework_output,
                        "datatype is not currently supported");
                return OMPI_ERR_NOT_SUPPORTED;
            }
            ret = ompi_osc_portals4_get_op(op, &ptl_op);
            if (OMPI_SUCCESS != ret) {
                opal_output(ompi_osc_base_framework.framework_output,
                        "operation is not currently supported");
                return OMPI_ERR_NOT_SUPPORTED;
            }
            do {
                size_t msg_length = MIN(module->atomic_max, length - sent);

                (void)opal_atomic_add_fetch_64(&module->opcount, 1);

                OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                             "%s,%d Atomic", __FUNCTION__, __LINE__));
                ret = PtlAtomic(module->md_h,
                                md_offset + sent + origin_lb,
                                msg_length,
                                PTL_ACK_REQ,
                                peer,
                                module->pt_idx,
                                module->match_bits,
                                offset + sent + target_lb,
                                NULL,
                                0,
                                ptl_op,
                                ptl_dt);
                if (OMPI_SUCCESS != ret) {
                    (void)opal_atomic_add_fetch_64(&module->opcount, -1);
                    return ret;
                }
                sent += msg_length;
            } while (sent < length);
        }
    }

    return OMPI_SUCCESS;
}


int
ompi_osc_portals4_get_accumulate(const void *origin_addr,
                                 int origin_count,
                                 struct ompi_datatype_t *origin_dt,
                                 void *result_addr,
                                 int result_count,
                                 struct ompi_datatype_t *result_dt,
                                 int target,
                                 ptrdiff_t target_disp,
                                 int target_count,
                                 struct ompi_datatype_t *target_dt,
                                 struct ompi_op_t *op,
                                 struct ompi_win_t *win)
{
    int ret;
    ompi_osc_portals4_module_t *module =
        (ompi_osc_portals4_module_t*) win->w_osc_module;
    ptl_process_t peer = ompi_osc_portals4_get_peer(module, target);
    size_t target_offset, size;
    ptl_op_t ptl_op;
    ptl_datatype_t ptl_dt;
    ptrdiff_t length, origin_lb, target_lb, result_lb, extent;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "get_accumulate: 0x%lx, %d, %s, 0x%lx, %d, %s, %d, %lu, %d, %s, %s, 0x%lx",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, (unsigned long) result_addr,
                         result_count, result_dt->name,
                         target, (unsigned long) target_disp,
                         target_count, target_dt->name,
                         op->o_name,
                         (unsigned long) win));

    target_offset = get_displacement(module, target) * target_disp;

    if (target_count > 0 && !ompi_datatype_is_contiguous_memory_layout(target_dt, target_count)) {
        if (MPI_REPLACE == op) {
            OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                                 "get_accumulate: MPI_REPLACE  non-contiguous target"));
            ret = swap_from_noncontig(module,
                                      module->md_h,
                                      result_addr,
                                      result_count,
                                      result_dt,
                                      module->md_h,
                                      origin_addr,
                                      origin_count,
                                      origin_dt,
                                      peer,
                                      target_count,
                                      target_dt,
                                      target_offset,
                                      module->pt_idx,
                                      module->match_bits,
                                      NULL);
            if (PTL_OK != ret) {
                OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                             "%s,%d swap_from_noncontig() failed: ret = %d",
                             __FUNCTION__, __LINE__, ret));
                return ret;
            }
        } else if (MPI_NO_OP == op) {
            OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                                 "get_accumulate: MPI_NO_OP  non-contiguous target"));
            ret = atomic_get_from_noncontig(module,
                                            module->md_h,
                                            result_addr,
                                            result_count,
                                            result_dt,
                                            peer,
                                            target_count,
                                            target_dt,
                                            target_offset,
                                            module->pt_idx,
                                            module->match_bits,
                                            NULL);
            if (PTL_OK != ret) {
                OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                             "%s,%d atomic_get_from_noncontig() failed: ret = %d",
                             __FUNCTION__, __LINE__, ret));
                return ret;
            }
        } else {
            OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                                 "get_accumulate: other-op  non-contiguous target"));
            ret = fetch_atomic_from_noncontig(module,
                                              module->md_h,
                                              result_addr,
                                              result_count,
                                              result_dt,
                                              module->md_h,
                                              origin_addr,
                                              origin_count,
                                              origin_dt,
                                              peer,
                                              target_count,
                                              target_dt,
                                              target_offset,
                                              module->pt_idx,
                                              module->match_bits,
                                              op,
                                              NULL);
            if (PTL_OK != ret) {
                OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                    "%s,%d fetch_atomic_from_noncontig() failed: ret = %d",
                    __FUNCTION__, __LINE__, ret));
                return ret;
            }
        }
    } else if ((origin_count > 0 && !ompi_datatype_is_contiguous_memory_layout(origin_dt, origin_count)) ||
               (result_count > 0 && !ompi_datatype_is_contiguous_memory_layout(result_dt, result_count))) {
        if (MPI_REPLACE == op) {
            OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                                 "get_accumulate: MPI_REPLACE  non-contiguous origin/result"));
            ret = swap_to_iovec(module,
                                result_addr,
                                result_count,
                                result_dt,
                                origin_addr,
                                origin_count,
                                origin_dt,
                                peer,
                                target_count,
                                target_dt,
                                target_offset,
                                module->pt_idx,
                                module->match_bits,
                                NULL);
            if (PTL_OK != ret) {
                OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                             "%s,%d swap_to_iovec() failed: ret = %d",
                             __FUNCTION__, __LINE__, ret));
                return ret;
            }
        } else if (MPI_NO_OP == op) {
            OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                                 "get_accumulate: MPI_NO_OP  non-contiguous origin/result"));
            ret = atomic_get_to_iovec(module,
                                      result_addr,
                                      result_count,
                                      result_dt,
                                      peer,
                                      target_count,
                                      target_dt,
                                      target_offset,
                                      module->pt_idx,
                                      module->match_bits,
                                      NULL);
            if (PTL_OK != ret) {
                OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                             "%s,%d atomic_get_to_iovec() failed: ret = %d",
                             __FUNCTION__, __LINE__, ret));
                return ret;
            }
        } else {
            OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                                 "get_accumulate: other-op  non-contiguous origin/result"));
            ret = fetch_atomic_to_iovec(module,
                                        result_addr,
                                        result_count,
                                        result_dt,
                                        origin_addr,
                                        origin_count,
                                        origin_dt,
                                        peer,
                                        target_count,
                                        target_dt,
                                        target_offset,
                                        module->pt_idx,
                                        module->match_bits,
                                        op,
                                        NULL);
            if (PTL_OK != ret) {
                OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                             "%s,%d fetch_atomic_to_iovec() failed: ret = %d",
                             __FUNCTION__, __LINE__, ret));
                return ret;
            }
        }
    } else {
        if (MPI_REPLACE == op) {
            OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                                 "get_accumulate: MPI_REPLACE  contiguous"));
            ptl_size_t result_md_offset, origin_md_offset;

            ret = ompi_datatype_get_true_extent(origin_dt, &origin_lb, &extent);
            if (OMPI_SUCCESS != ret) {
                return ret;
            }
            ret = ompi_datatype_get_true_extent(target_dt, &target_lb, &extent);
            if (OMPI_SUCCESS != ret) {
                return ret;
            }
            ret = ompi_datatype_get_true_extent(result_dt, &result_lb, &extent);
            if (OMPI_SUCCESS != ret) {
                return ret;
            }
            ompi_datatype_type_size(origin_dt, &size);
            length = size * origin_count;

            ret = ompi_osc_portals4_get_dt(origin_dt, &ptl_dt);
            if (OMPI_SUCCESS != ret) {
                opal_output(ompi_osc_base_framework.framework_output,
                        "MPI_Get_accumulate: datatype is not currently supported");
                return OMPI_ERR_NOT_SUPPORTED;
            }

            result_md_offset = (ptl_size_t) result_addr;
            origin_md_offset = (ptl_size_t) origin_addr;

            ret = segmentedSwap(&module->opcount,
                                module->md_h,
                                result_md_offset + result_lb,
                                module->md_h,
                                origin_md_offset + origin_lb,
                                length,
                                module->fetch_atomic_max,
                                peer,
                                module->pt_idx,
                                module->match_bits,
                                target_offset + target_lb,
                                NULL,
                                ptl_dt);
            if (OMPI_SUCCESS != ret) {
                return ret;
            }
        } else if (MPI_NO_OP == op) {
            OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                                 "get_accumulate: MPI_NO_OP  contiguous"));
            ptl_size_t md_offset;

            ret = ompi_datatype_get_true_extent(target_dt, &target_lb, &extent);
            if (OMPI_SUCCESS != ret) {
                return ret;
            }
            ret = ompi_datatype_get_true_extent(result_dt, &result_lb, &extent);
            if (OMPI_SUCCESS != ret) {
                return ret;
            }
            ompi_datatype_type_size(target_dt, &size);
            length = size * target_count;

            md_offset = (ptl_size_t) result_addr;

            OPAL_OUTPUT_VERBOSE((90,ompi_osc_base_framework.framework_output,
                                  "%s,%d MPI_Get_accumulate", __FUNCTION__, __LINE__));
            ret = segmentedGet(&module->opcount,
                               module->md_h,
                               (ptl_size_t) md_offset + result_lb,
                               length,
                               module->fetch_atomic_max,
                               peer,
                               module->pt_idx,
                               module->match_bits,
                               target_offset + target_lb,
                               NULL);
            if (OMPI_SUCCESS != ret) {
                return ret;
            }
        } else {
            OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                                 "get_accumulate: other-op  contiguous"));
            ptl_size_t result_md_offset, origin_md_offset;

            ret = ompi_datatype_get_true_extent(origin_dt, &origin_lb, &extent);
            if (OMPI_SUCCESS != ret) {
                return ret;
            }
            ret = ompi_datatype_get_true_extent(target_dt, &target_lb, &extent);
            if (OMPI_SUCCESS != ret) {
                return ret;
            }
            ret = ompi_datatype_get_true_extent(result_dt, &result_lb, &extent);
            if (OMPI_SUCCESS != ret) {
                return ret;
            }
            ompi_datatype_type_size(origin_dt, &size);
            length = size * origin_count;

            result_md_offset = (ptl_size_t) result_addr;
            origin_md_offset = (ptl_size_t) origin_addr;

            ret = ompi_osc_portals4_get_dt(origin_dt, &ptl_dt);
            if (OMPI_SUCCESS != ret) {
                opal_output(ompi_osc_base_framework.framework_output,
                        "MPI_Get_accumulate: datatype is not currently supported");
                return OMPI_ERR_NOT_SUPPORTED;
            }

            ret = ompi_osc_portals4_get_op(op, &ptl_op);
            if (OMPI_SUCCESS != ret) {
                opal_output(ompi_osc_base_framework.framework_output,
                        "MPI_Get_accumulate: operation is not currently supported");
                return OMPI_ERR_NOT_SUPPORTED;
            }

            ret = segmentedFetchAtomic(&module->opcount,
                                       module->md_h,
                                       result_md_offset + result_lb,
                                       module->md_h,
                                       origin_md_offset + origin_lb,
                                       length,
                                       module->fetch_atomic_max,
                                       peer,
                                       module->pt_idx,
                                       module->match_bits,
                                       target_offset + target_lb,
                                       NULL,
                                       ptl_op,
                                       ptl_dt);
            if (OMPI_SUCCESS != ret) {
                return ret;
            }
        }
    }

    return OMPI_SUCCESS;
}


int
ompi_osc_portals4_compare_and_swap(const void *origin_addr,
                                   const void *compare_addr,
                                   void *result_addr,
                                   struct ompi_datatype_t *dt,
                                   int target,
                                   ptrdiff_t target_disp,
                                   struct ompi_win_t *win)
{
    int ret;
    ompi_osc_portals4_module_t *module =
        (ompi_osc_portals4_module_t*) win->w_osc_module;
    ptl_process_t peer = ompi_osc_portals4_get_peer(module, target);
    size_t length;
    size_t offset;
    ptl_datatype_t ptl_dt;
    ptl_size_t result_md_offset, origin_md_offset;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "compare_and_swap: 0x%lx, 0x%lx, 0x%lx, %s, %d, %lu, 0x%lx",
                         (unsigned long) origin_addr,
                         (unsigned long) compare_addr,
                         (unsigned long) result_addr,
                         dt->name, target, (unsigned long) target_disp,
                         (unsigned long) win));

    ret = ompi_osc_portals4_get_dt(dt, &ptl_dt);
    if (OMPI_SUCCESS != ret) {
        opal_output(ompi_osc_base_framework.framework_output,
                "MPI_Compare_and_swap: datatype is not currently supported");
        return OMPI_ERR_NOT_SUPPORTED;
    }

    offset = get_displacement(module, target) * target_disp;

    ret = ompi_datatype_type_size(dt, &length);
    if (OMPI_SUCCESS != ret) return ret;

    assert(length <= module->fetch_atomic_max);

    result_md_offset = (ptl_size_t) result_addr;
    origin_md_offset = (ptl_size_t) origin_addr;

    (void)opal_atomic_add_fetch_64(&module->opcount, 1);

    OPAL_OUTPUT_VERBOSE((90,ompi_osc_base_framework.framework_output,
                         "%s,%d Swap", __FUNCTION__, __LINE__));
    ret = PtlSwap(module->md_h,
                  result_md_offset,
                  module->md_h,
                  origin_md_offset,
                  length,
                  peer,
                  module->pt_idx,
                  module->match_bits,
                  offset,
                  NULL,
                  0,
                  compare_addr,
                  PTL_CSWAP,
                  ptl_dt);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    return OMPI_SUCCESS;
}


int
ompi_osc_portals4_fetch_and_op(const void *origin_addr,
                               void *result_addr,
                               struct ompi_datatype_t *dt,
                               int target,
                               ptrdiff_t target_disp,
                               struct ompi_op_t *op,
                               struct ompi_win_t *win)
{
    int ret;
    ompi_osc_portals4_module_t *module =
        (ompi_osc_portals4_module_t*) win->w_osc_module;
    ptl_process_t peer = ompi_osc_portals4_get_peer(module, target);
    size_t length;
    size_t offset;
    ptl_op_t ptl_op;
    ptl_datatype_t ptl_dt;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "fetch_and_op: 0x%lx, 0x%lx, %s, %d, %lu, %s, 0x%lx",
                         (unsigned long) origin_addr,
                         (unsigned long) result_addr,
                         dt->name, target, (unsigned long) target_disp,
                         op->o_name,
                         (unsigned long) win));

    ret = ompi_osc_portals4_get_dt(dt, &ptl_dt);
    if (OMPI_SUCCESS != ret) {
        opal_output(ompi_osc_base_framework.framework_output,
                "MPI_Fetch_and_op: datatype is not currently supported");
        return OMPI_ERR_NOT_SUPPORTED;
    }

    offset = get_displacement(module, target) * target_disp;

    ret = ompi_datatype_type_size(dt, &length);
    if (OMPI_SUCCESS != ret) return ret;

    assert(length <= module->fetch_atomic_max);

    if (MPI_REPLACE == op) {
        ptl_size_t result_md_offset, origin_md_offset;

        result_md_offset = (ptl_size_t) result_addr;
        origin_md_offset = (ptl_size_t) origin_addr;

        (void)opal_atomic_add_fetch_64(&module->opcount, 1);
        OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                             "%s,%d Swap", __FUNCTION__, __LINE__));
        ret = PtlSwap(module->md_h,
                      result_md_offset,
                      module->md_h,
                      origin_md_offset,
                      length,
                      peer,
                      module->pt_idx,
                      module->match_bits,
                      offset,
                      NULL,
                      0,
                      NULL,
                      PTL_SWAP,
                      ptl_dt);
    } else if (MPI_NO_OP == op) {
        ptl_size_t md_offset;

        md_offset = (ptl_size_t) result_addr;

        (void)opal_atomic_add_fetch_64(&module->opcount, 1);
        OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                             "%s,%d Get", __FUNCTION__, __LINE__));
        ret = PtlGet(module->md_h,
                     md_offset,
                     length,
                     peer,
                     module->pt_idx,
                     module->match_bits,
                     offset,
                     NULL);
    } else {
        ptl_size_t result_md_offset, origin_md_offset;
        (void)opal_atomic_add_fetch_64(&module->opcount, 1);

        ret = ompi_osc_portals4_get_op(op, &ptl_op);
        if (OMPI_SUCCESS != ret) {
            opal_output(ompi_osc_base_framework.framework_output,
                    "MPI_Fetch_and_op: operation is not currently supported");
            return OMPI_ERR_NOT_SUPPORTED;
        }

        result_md_offset = (ptl_size_t) result_addr;
        origin_md_offset = (ptl_size_t) origin_addr;

        OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                             "%s,%d FetchAtomic", __FUNCTION__, __LINE__));
        ret = PtlFetchAtomic(module->md_h,
                             result_md_offset,
                             module->md_h,
                             origin_md_offset,
                             length,
                             peer,
                             module->pt_idx,
                             module->match_bits,
                             offset,
                             NULL,
                             0,
                             ptl_op,
                             ptl_dt);
    }
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    return OMPI_SUCCESS;
}
