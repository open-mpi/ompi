/*
 * Copyright (c) 2011-2013 Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2014      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
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
number_of_fragment(ptl_size_t length, ptl_size_t maxlength)
{
    ptl_size_t nb_frag = length == 0 ? 1 : (length - 1) / maxlength + 1;
    OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                         "%s,%d : %ld fragment(s)", __FUNCTION__, __LINE__, nb_frag));
    return nb_frag;
}

static int
splittedPtlPut(ptl_handle_md_t md_h,
            ptl_size_t loc_offset,
            ptl_size_t length,
            ptl_ack_req_t ack_req,
            ptl_process_t target_id,
            ptl_pt_index_t pt_index,
            ptl_match_bits_t match_b,
            ptl_size_t rem_offset,
            void *usr_ptr,
            ptl_hdr_data_t hdr_data)
{
    ptl_size_t length_sent = 0;
    do {
        ptl_size_t length_frag;
        int ret;

        length_frag = (length > mca_osc_portals4_component.ptl_max_msg_size) ?
            mca_osc_portals4_component.ptl_max_msg_size :
            length;
        OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                             "Put size : %lu/%lu, offset:%lu", length_frag, length, length_sent));
        ret = PtlPut(md_h,
                     loc_offset + length_sent,
                     length_frag,
                     ack_req,
                     target_id,
                     pt_index,
                     match_b,
                     rem_offset + length_sent,
                     usr_ptr,
                     hdr_data);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                                 "%s:%d PtlPut failed with return value %d",
                                 __FUNCTION__, __LINE__, ret);
            return ret;
        }
        length -= length_frag;
        length_sent += length_frag;
    } while (length);
    return PTL_OK;
}

static int
splittedPtlGet(ptl_handle_md_t md_h,
               ptl_size_t loc_offset,
               ptl_size_t length,
               ptl_process_t target_id,
               ptl_pt_index_t pt_index,
               ptl_match_bits_t match_b,
               ptl_size_t rem_offset,
               void *usr_ptr)
{
    ptl_size_t length_submitted = 0;
    OPAL_OUTPUT_VERBOSE((90,ompi_osc_base_framework.framework_output, "Get"));

    do {
        ptl_size_t length_frag;
        int ret;
        length_frag = (length > mca_osc_portals4_component.ptl_max_msg_size) ?
            mca_osc_portals4_component.ptl_max_msg_size :
            length;
        ret = PtlGet(md_h,
                     (ptl_size_t) loc_offset + length_submitted,
                     length_frag,
                     target_id,
                     pt_index,
                     match_b,
                     rem_offset + length_submitted,
                     usr_ptr);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                                 "%s:%d PtlGet failed with return value %d",
                                 __FUNCTION__, __LINE__, ret);
            return ret;
        }
        length -= length_frag;
        length_submitted += length_frag;
    } while (length);
    return PTL_OK;
}

int
ompi_osc_portals4_rput(const void *origin_addr,
                       int origin_count,
                       struct ompi_datatype_t *origin_dt,
                       int target,
                       OPAL_PTRDIFF_TYPE target_disp,
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
    size_t offset;
    OPAL_PTRDIFF_TYPE length, origin_lb, target_lb;

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

    if (!ompi_datatype_is_contiguous_memory_layout(origin_dt, origin_count) ||
        !ompi_datatype_is_contiguous_memory_layout(target_dt, target_count)) {
        OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
        opal_output(ompi_osc_base_framework.framework_output,
                    "MPI_Rput: transfer of non-contiguous memory is not currently supported.\n");
        return OMPI_ERR_NOT_SUPPORTED;
    } else {
        ret = ompi_datatype_get_extent(origin_dt, &origin_lb, &length);
        if (OMPI_SUCCESS != ret) {
            OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
            return ret;
        }
        ret = ompi_datatype_type_lb(target_dt, &target_lb);
        if (OMPI_SUCCESS != ret) {
            OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
            return ret;
        }
        length *= origin_count;
        request->ops_expected = number_of_fragment(length, mca_osc_portals4_component.ptl_max_msg_size);
        opal_atomic_add_64(&module->opcount, request->ops_expected);
        OPAL_OUTPUT_VERBOSE((90,ompi_osc_base_framework.framework_output,
                             "%s,%d Put", __FUNCTION__, __LINE__));

        ret = splittedPtlPut(module->req_md_h,
                     (ptl_size_t) origin_addr + origin_lb,
                     length,
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
                       OPAL_PTRDIFF_TYPE target_disp,
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
    size_t offset;
    OPAL_PTRDIFF_TYPE length, origin_lb, target_lb;

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

    if (!ompi_datatype_is_contiguous_memory_layout(origin_dt, origin_count) ||
        !ompi_datatype_is_contiguous_memory_layout(target_dt, target_count)) {
        OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
        opal_output(ompi_osc_base_framework.framework_output,
                    "MPI_Rget: transfer of non-contiguous memory is not currently supported.\n");
        return OMPI_ERR_NOT_SUPPORTED;
    } else {
        ret = ompi_datatype_get_extent(origin_dt, &origin_lb, &length);
        if (OMPI_SUCCESS != ret) {
            OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
            return ret;
        }
        ret = ompi_datatype_type_lb(target_dt, &target_lb);
        if (OMPI_SUCCESS != ret) {
            OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
            return ret;
        }
        length *= origin_count;
        request->ops_expected = number_of_fragment(length, mca_osc_portals4_component.ptl_max_msg_size);
        opal_atomic_add_64(&module->opcount, request->ops_expected);
        OPAL_OUTPUT_VERBOSE((90,ompi_osc_base_framework.framework_output,
                             "%s,%d Get", __FUNCTION__, __LINE__));
        ret = splittedPtlGet(module->req_md_h,
                     (ptl_size_t) origin_addr + origin_lb,
                     length,
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
                              OPAL_PTRDIFF_TYPE target_disp,
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
    size_t offset;
    ptl_op_t ptl_op;
    ptl_datatype_t ptl_dt;
    OPAL_PTRDIFF_TYPE sent, length, origin_lb, target_lb;

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

    if (!ompi_datatype_is_contiguous_memory_layout(origin_dt, origin_count) ||
        !ompi_datatype_is_contiguous_memory_layout(target_dt, target_count)) {
        OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
        opal_output(ompi_osc_base_framework.framework_output,
                    "MPI_Raccumulate: transfer of non-contiguous memory is not currently supported.\n");
        return OMPI_ERR_NOT_SUPPORTED;
    } else {
        ptl_size_t md_offset;

        ret = ompi_datatype_get_extent(origin_dt, &origin_lb, &length);
        if (OMPI_SUCCESS != ret) {
            OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
            return ret;
        }
        ret = ompi_datatype_type_lb(target_dt, &target_lb);
        if (OMPI_SUCCESS != ret) {
            OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
            return ret;
        }
        length *= origin_count;
        sent = 0;

        md_offset = (ptl_size_t) origin_addr;

        do {
            size_t msg_length = MIN(module->atomic_max, length - sent);

            if (MPI_REPLACE == op) {
                request->ops_expected += number_of_fragment(msg_length, mca_osc_portals4_component.ptl_max_msg_size);
                opal_atomic_add_64(&module->opcount, number_of_fragment(msg_length, mca_osc_portals4_component.ptl_max_msg_size));
                OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                             "%s,%d Put", __FUNCTION__, __LINE__));
                ret = splittedPtlPut(module->req_md_h,
                             md_offset + sent + origin_lb,
                             msg_length,
                             PTL_ACK_REQ,
                             peer,
                             module->pt_idx,
                             module->match_bits,
                             offset + sent + target_lb,
                             request,
                             0);
            } else {
                request->ops_expected++;
                opal_atomic_add_64(&module->opcount, 1);
                ret = ompi_osc_portals4_get_dt(origin_dt, &ptl_dt);
                if (OMPI_SUCCESS != ret) {
                    opal_output(ompi_osc_base_framework.framework_output,
                            "MPI_Raccumulate: datatype is not currently supported");
                    return OMPI_ERR_NOT_SUPPORTED;
                }

                ret = ompi_osc_portals4_get_op(op, &ptl_op);
                if (OMPI_SUCCESS != ret) {
                    opal_output(ompi_osc_base_framework.framework_output,
                            "MPI_Raccumulate: operation is not currently supported");
                    return OMPI_ERR_NOT_SUPPORTED;
                }
                OPAL_OUTPUT_VERBOSE((90,ompi_osc_base_framework.framework_output,
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
            }
            if (OMPI_SUCCESS != ret) {
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                return ret;
            }
            sent += msg_length;
        } while (sent < length);
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
                                  OPAL_PTRDIFF_TYPE target_disp,
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
    size_t offset;
    ptl_op_t ptl_op;
    ptl_datatype_t ptl_dt;
    OPAL_PTRDIFF_TYPE sent, length, origin_lb, target_lb, result_lb;

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

    offset = get_displacement(module, target) * target_disp;

    if (!ompi_datatype_is_contiguous_memory_layout(origin_dt, origin_count) ||
        !ompi_datatype_is_contiguous_memory_layout(result_dt, result_count) ||
        !ompi_datatype_is_contiguous_memory_layout(target_dt, target_count)) {
        OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
        opal_output(ompi_osc_base_framework.framework_output,
                    "MPI_Rget_accumulate: transfer of non-contiguous memory is not currently supported.\n");
        return OMPI_ERR_NOT_SUPPORTED;
    } else {
        sent = 0;

        if (MPI_REPLACE == op) {
            ptl_size_t result_md_offset, origin_md_offset;

            ret = ompi_datatype_get_extent(origin_dt, &origin_lb, &length);
            if (OMPI_SUCCESS != ret) {
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                return ret;
            }
            ret = ompi_datatype_type_lb(target_dt, &target_lb);
            if (OMPI_SUCCESS != ret) {
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                return ret;
            }
            ret = ompi_datatype_type_lb(result_dt, &result_lb);
            if (OMPI_SUCCESS != ret) {
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                return ret;
            }

            ret = ompi_osc_portals4_get_dt(origin_dt, &ptl_dt);
            if (OMPI_SUCCESS != ret) {
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                opal_output(ompi_osc_base_framework.framework_output,
                        "MPI_Rget_accumulate: datatype is not currently supported");
                return OMPI_ERR_NOT_SUPPORTED;
            }
            length *= origin_count;

            result_md_offset = (ptl_size_t) result_addr;
            origin_md_offset = (ptl_size_t) origin_addr;

            do {
                size_t msg_length = MIN(module->fetch_atomic_max, length - sent);

                (void)opal_atomic_add_64(&module->opcount, 1);
                request->ops_expected++;

                OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                                      "%s,%d Swap", __FUNCTION__, __LINE__));
                ret = PtlSwap(module->req_md_h,
                              result_md_offset + sent + result_lb,
                              module->md_h,
                              origin_md_offset + sent + origin_lb,
                              msg_length,
                              peer,
                              module->pt_idx,
                              module->match_bits,
                              offset + sent + target_lb,
                              request,
                              0,
                              NULL,
                              PTL_SWAP,
                              ptl_dt);
                sent += msg_length;
            } while (sent < length);
        } else if (MPI_NO_OP == op) {
            ptl_size_t md_offset;

            ret = ompi_datatype_get_extent(target_dt, &target_lb, &length);
            if (OMPI_SUCCESS != ret) {
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                return ret;
            }
            ret = ompi_datatype_type_lb(result_dt, &result_lb);
            if (OMPI_SUCCESS != ret) {
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                return ret;
            }
            length *= target_count;

            md_offset = (ptl_size_t) result_addr;

            do {
                size_t msg_length = MIN(module->fetch_atomic_max, length - sent);

                opal_atomic_add_64(&module->opcount, number_of_fragment(msg_length, mca_osc_portals4_component.ptl_max_msg_size));
                request->ops_expected += number_of_fragment(msg_length, mca_osc_portals4_component.ptl_max_msg_size);
                OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                                     "%s,%d Get", __FUNCTION__, __LINE__));
                ret = splittedPtlGet(module->req_md_h,
                             md_offset + sent + result_lb,
                             msg_length,
                             peer,
                             module->pt_idx,
                             module->match_bits,
                             offset + sent + target_lb,
                             request);
                sent += msg_length;
            } while (sent < length);
        } else {
            ptl_size_t result_md_offset, origin_md_offset;

            ret = ompi_datatype_get_extent(origin_dt, &origin_lb, &length);
            if (OMPI_SUCCESS != ret) {
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                return ret;
            }
            ret = ompi_datatype_type_lb(target_dt, &target_lb);
            if (OMPI_SUCCESS != ret) {
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                return ret;
            }
            ret = ompi_datatype_type_lb(result_dt, &result_lb);
            if (OMPI_SUCCESS != ret) {
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                return ret;
            }
            length *= origin_count;

            result_md_offset = (ptl_size_t) result_addr;
            origin_md_offset = (ptl_size_t) origin_addr;

            ret = ompi_osc_portals4_get_dt(origin_dt, &ptl_dt);
            if (OMPI_SUCCESS != ret) {
                opal_output(ompi_osc_base_framework.framework_output,
                        "MPI_Rget_accumulate: datatype is not currently supported");
                return OMPI_ERR_NOT_SUPPORTED;
            }

            ret = ompi_osc_portals4_get_op(op, &ptl_op);
            if (OMPI_SUCCESS != ret) {
                opal_output(ompi_osc_base_framework.framework_output,
                        "MPI_Rget_accumulate: operation is not currently supported");
                return OMPI_ERR_NOT_SUPPORTED;
            }

            do {
                size_t msg_length = MIN(module->fetch_atomic_max, length - sent);

                (void)opal_atomic_add_64(&module->opcount, 1);
                request->ops_expected++;

                OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                                      "%s,%d FetchAtomic", __FUNCTION__, __LINE__));
                ret = PtlFetchAtomic(module->req_md_h,
                                     result_md_offset + sent + result_lb,
                                     module->md_h,
                                     origin_md_offset + sent + origin_lb,
                                     msg_length,
                                     peer,
                                     module->pt_idx,
                                     module->match_bits,
                                     offset + sent + target_lb,
                                     request,
                                     0,
                                     ptl_op,
                                     ptl_dt);
                sent += msg_length;
            } while (sent < length);
        }
        if (OMPI_SUCCESS != ret) {
            OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
            return ret;
        }
    }

    return OMPI_SUCCESS;
}


int
ompi_osc_portals4_put(const void *origin_addr,
                      int origin_count,
                      struct ompi_datatype_t *origin_dt,
                      int target,
                      OPAL_PTRDIFF_TYPE target_disp,
                      int target_count,
                      struct ompi_datatype_t *target_dt,
                      struct ompi_win_t *win)
{
    int ret;
    ompi_osc_portals4_module_t *module =
        (ompi_osc_portals4_module_t*) win->w_osc_module;
    ptl_process_t peer = ompi_osc_portals4_get_peer(module, target);
    size_t offset, size;
    OPAL_PTRDIFF_TYPE length, origin_lb, target_lb, extent;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "put: 0x%lx, %d, %s, %d, %lu, %d, %s, 0x%lx",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, target, (unsigned long) target_disp,
                         target_count, target_dt->name,
                         (unsigned long) win));

    offset = get_displacement(module, target) * target_disp;

    if (!ompi_datatype_is_contiguous_memory_layout(origin_dt, origin_count) ||
        !ompi_datatype_is_contiguous_memory_layout(target_dt, target_count)) {
        opal_output(ompi_osc_base_framework.framework_output,
                    "MPI_Put: transfer of non-contiguous memory is not currently supported.\n");
        return OMPI_ERR_NOT_SUPPORTED;
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

        opal_atomic_add_64(&module->opcount, number_of_fragment(length, mca_osc_portals4_component.ptl_max_msg_size));

        OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                     "%s,%d Put(origin_count=%d, origin_lb=%lu, target_count=%d, target_lb=%lu, length=%lu, op_count=%ld)",
                     __FUNCTION__, __LINE__, origin_count, origin_lb, target_count, target_lb, length, module->opcount));
        ret = splittedPtlPut(module->md_h,
                     (ptl_size_t) origin_addr + origin_lb,
                     length,
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
                      OPAL_PTRDIFF_TYPE target_disp,
                      int target_count,
                      struct ompi_datatype_t *target_dt,
                      struct ompi_win_t *win)
{
    int ret;
    ompi_osc_portals4_module_t *module =
        (ompi_osc_portals4_module_t*) win->w_osc_module;
    ptl_process_t peer = ompi_osc_portals4_get_peer(module, target);
    size_t offset;
    OPAL_PTRDIFF_TYPE length, origin_lb, target_lb;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "get: 0x%lx, %d, %s, %d, %lu, %d, %s, 0x%lx",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, target, (unsigned long) target_disp,
                         target_count, target_dt->name,
                         (unsigned long) win));

    offset = get_displacement(module, target) * target_disp;

    if (!ompi_datatype_is_contiguous_memory_layout(origin_dt, origin_count) ||
        !ompi_datatype_is_contiguous_memory_layout(target_dt, target_count)) {
        opal_output(ompi_osc_base_framework.framework_output,
                    "MPI_Get: transfer of non-contiguous memory is not currently supported.\n");
        return OMPI_ERR_NOT_SUPPORTED;
    } else {
        ret = ompi_datatype_get_extent(origin_dt, &origin_lb, &length);
        if (OMPI_SUCCESS != ret) {
            return ret;
        }
        ret = ompi_datatype_type_lb(target_dt, &target_lb);
        if (OMPI_SUCCESS != ret) {
            return ret;
        }
        length *= origin_count;
        opal_atomic_add_64(&module->opcount, number_of_fragment(length, mca_osc_portals4_component.ptl_max_msg_size));
        OPAL_OUTPUT_VERBOSE((90,ompi_osc_base_framework.framework_output,
                              "%s,%d Get", __FUNCTION__, __LINE__));
        ret = splittedPtlGet(module->md_h,
                     (ptl_size_t) origin_addr + origin_lb,
                     length,
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
                             OPAL_PTRDIFF_TYPE target_disp,
                             int target_count,
                             struct ompi_datatype_t *target_dt,
                             struct ompi_op_t *op,
                             struct ompi_win_t *win)
{
    int ret;
    ompi_osc_portals4_module_t *module =
        (ompi_osc_portals4_module_t*) win->w_osc_module;
    ptl_process_t peer = ompi_osc_portals4_get_peer(module, target);
    size_t offset;
    ptl_op_t ptl_op;
    ptl_datatype_t ptl_dt;
    OPAL_PTRDIFF_TYPE sent, length, origin_lb, target_lb;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "accumulate: 0x%lx, %d, %s, %d, %lu, %d, %s, %s, 0x%lx",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, target, (unsigned long) target_disp,
                         target_count, target_dt->name,
                         op->o_name,
                         (unsigned long) win));

    offset = get_displacement(module, target) * target_disp;

    if (!ompi_datatype_is_contiguous_memory_layout(origin_dt, origin_count) ||
        !ompi_datatype_is_contiguous_memory_layout(target_dt, target_count)) {
        opal_output(ompi_osc_base_framework.framework_output,
                    "MPI_Accumulate: transfer of non-contiguous memory is not currently supported.\n");
        return OMPI_ERR_NOT_SUPPORTED;
    } else {
        ptl_size_t md_offset;

        ret = ompi_datatype_get_extent(origin_dt, &origin_lb, &length);
        if (OMPI_SUCCESS != ret) {
            return ret;
        }
        ret = ompi_datatype_type_lb(target_dt, &target_lb);
        if (OMPI_SUCCESS != ret) {
            return ret;
        }
        length *= origin_count;
        sent = 0;

        md_offset = (ptl_size_t) origin_addr;

        do {
            size_t msg_length = MIN(module->atomic_max, length - sent);

            if (MPI_REPLACE == op) {
                opal_atomic_add_64(&module->opcount, number_of_fragment(msg_length, mca_osc_portals4_component.ptl_max_msg_size));
                OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                                     "%s,%d Put", __FUNCTION__, __LINE__));
                ret = splittedPtlPut(module->md_h,
                             md_offset + sent + origin_lb,
                             msg_length,
                             PTL_ACK_REQ,
                             peer,
                             module->pt_idx,
                             module->match_bits,
                             offset + sent + target_lb,
                             NULL,
                             0);
            } else {
                (void)opal_atomic_add_64(&module->opcount, 1);
                ret = ompi_osc_portals4_get_dt(origin_dt, &ptl_dt);
                if (OMPI_SUCCESS != ret) {
                    opal_output(ompi_osc_base_framework.framework_output,
                            "MPI_Accumulate: datatype is not currently supported");
                    return OMPI_ERR_NOT_SUPPORTED;
                }

                ret = ompi_osc_portals4_get_op(op, &ptl_op);
                if (OMPI_SUCCESS != ret) {
                    opal_output(ompi_osc_base_framework.framework_output,
                            "MPI_Accumulate: operation is not currently supported");
                    return OMPI_ERR_NOT_SUPPORTED;
                }

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
            }
            if (OMPI_SUCCESS != ret) {
                return ret;
            }
            sent += msg_length;
        } while (sent < length);
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
                                 OPAL_PTRDIFF_TYPE target_disp,
                                 int target_count,
                                 struct ompi_datatype_t *target_dt,
                                 struct ompi_op_t *op,
                                 struct ompi_win_t *win)
{
    int ret;
    ompi_osc_portals4_module_t *module =
        (ompi_osc_portals4_module_t*) win->w_osc_module;
    ptl_process_t peer = ompi_osc_portals4_get_peer(module, target);
    size_t offset;
    ptl_op_t ptl_op;
    ptl_datatype_t ptl_dt;
    OPAL_PTRDIFF_TYPE sent, length, origin_lb, target_lb, result_lb;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "get_accumulate: 0x%lx, %d, %s, 0x%lx, %d, %s, %d, %lu, %d, %s, %s, 0x%lx",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, (unsigned long) result_addr,
                         result_count, result_dt->name,
                         target, (unsigned long) target_disp,
                         target_count, target_dt->name,
                         op->o_name,
                         (unsigned long) win));

    offset = get_displacement(module, target) * target_disp;

    /* we don't support non-contiguous buffers.  but if the count is 0, we don't care if buffer is non-contiguous. */
    if ((origin_count > 0 && !ompi_datatype_is_contiguous_memory_layout(origin_dt, origin_count)) ||
        (result_count > 0 && !ompi_datatype_is_contiguous_memory_layout(result_dt, result_count)) ||
        (target_count > 0 && !ompi_datatype_is_contiguous_memory_layout(target_dt, target_count))) {
        opal_output(ompi_osc_base_framework.framework_output,
                    "MPI_Get_accumulate: transfer of non-contiguous memory is not currently supported.\n");
        return OMPI_ERR_NOT_SUPPORTED;
    } else {
        sent = 0;
        if (MPI_REPLACE == op) {
            ptl_size_t result_md_offset, origin_md_offset;

            ret = ompi_datatype_get_extent(origin_dt, &origin_lb, &length);
            if (OMPI_SUCCESS != ret) {
                return ret;
            }
            ret = ompi_datatype_type_lb(target_dt, &target_lb);
            if (OMPI_SUCCESS != ret) {
                return ret;
            }
            ret = ompi_datatype_type_lb(result_dt, &result_lb);
            if (OMPI_SUCCESS != ret) {
                return ret;
            }
            ret = ompi_osc_portals4_get_dt(origin_dt, &ptl_dt);
            if (OMPI_SUCCESS != ret) {
                opal_output(ompi_osc_base_framework.framework_output,
                        "MPI_Get_accumulate: datatype is not currently supported");
                return OMPI_ERR_NOT_SUPPORTED;
            }
            length *= origin_count;

            result_md_offset = (ptl_size_t) result_addr;
            origin_md_offset = (ptl_size_t) origin_addr;

            do {
                size_t msg_length = MIN(module->fetch_atomic_max, length - sent);

                (void)opal_atomic_add_64(&module->opcount, 1);

                OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                                      "%s,%d Swap", __FUNCTION__, __LINE__));
                ret = PtlSwap(module->md_h,
                              result_md_offset + sent + result_lb,
                              module->md_h,
                              origin_md_offset + sent + origin_lb,
                              msg_length,
                              peer,
                              module->pt_idx,
                              module->match_bits,
                              offset + sent + target_lb,
                              NULL,
                              0,
                              NULL,
                              PTL_SWAP,
                              ptl_dt);
                sent += msg_length;
            } while (sent < length);
        } else if (MPI_NO_OP == op) {
            ptl_size_t md_offset;

            ret = ompi_datatype_get_extent(target_dt, &target_lb, &length);
            if (OMPI_SUCCESS != ret) {
                return ret;
            }
            ret = ompi_datatype_type_lb(result_dt, &result_lb);
            if (OMPI_SUCCESS != ret) {
                return ret;
            }
            length *= target_count;

            md_offset = (ptl_size_t) result_addr;

            do {
                size_t msg_length = MIN(module->fetch_atomic_max, length - sent);

                opal_atomic_add_64(&module->opcount, number_of_fragment(msg_length, mca_osc_portals4_component.ptl_max_msg_size));
                OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                                     "%s,%d Get", __FUNCTION__, __LINE__));
                ret = splittedPtlGet(module->md_h,
                             md_offset + sent + result_lb,
                             msg_length,
                             peer,
                             module->pt_idx,
                             module->match_bits,
                             offset + sent + target_lb,
                             NULL);
                sent += msg_length;
            } while (sent < length);
        } else {
            ptl_size_t result_md_offset, origin_md_offset;

            ret = ompi_datatype_get_extent(origin_dt, &origin_lb, &length);
            if (OMPI_SUCCESS != ret) {
                return ret;
            }
            ret = ompi_datatype_type_lb(target_dt, &target_lb);
            if (OMPI_SUCCESS != ret) {
                return ret;
            }
            ret = ompi_datatype_type_lb(result_dt, &result_lb);
            if (OMPI_SUCCESS != ret) {
                return ret;
            }
            length *= origin_count;

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

            do {
                size_t msg_length = MIN(module->fetch_atomic_max, length - sent);

                (void)opal_atomic_add_64(&module->opcount, 1);

                OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                                      "%s,%d FetchAtomic", __FUNCTION__, __LINE__));
                ret = PtlFetchAtomic(module->md_h,
                                     result_md_offset + sent + result_lb,
                                     module->md_h,
                                     origin_md_offset + sent + origin_lb,
                                     msg_length,
                                     peer,
                                     module->pt_idx,
                                     module->match_bits,
                                     offset + sent + target_lb,
                                     NULL,
                                     0,
                                     ptl_op,
                                     ptl_dt);
                sent += msg_length;
            } while (sent < length);
        }
        if (OMPI_SUCCESS != ret) {
            return ret;
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
                                   OPAL_PTRDIFF_TYPE target_disp,
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

    (void)opal_atomic_add_64(&module->opcount, 1);

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
                               OPAL_PTRDIFF_TYPE target_disp,
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

        (void)opal_atomic_add_64(&module->opcount, 1);
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

        opal_atomic_add_64(&module->opcount, number_of_fragment(length, mca_osc_portals4_component.ptl_max_msg_size));
        OPAL_OUTPUT_VERBOSE((90, ompi_osc_base_framework.framework_output,
                             "%s,%d Get", __FUNCTION__, __LINE__));
        ret = splittedPtlGet(module->md_h,
                     md_offset,
                     length,
                     peer,
                     module->pt_idx,
                     module->match_bits,
                     offset,
                     NULL);
    } else {
        ptl_size_t result_md_offset, origin_md_offset;
        (void)opal_atomic_add_64(&module->opcount, 1);

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
