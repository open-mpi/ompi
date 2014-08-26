/*
 * Copyright (c) 2011-2013 Sandia National Laboratories.  All rights reserved.
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

#include "ompi/mca/mtl/portals4/mtl_portals4_endpoint.h"


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


int
ompi_osc_portals4_rput(void *origin_addr,
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
    size_t length;
    size_t offset;
    ptl_handle_md_t md_h;
    void *md_base;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "rput: 0x%lx, %d, %s, %d, %d, %d, %s, 0x%lx",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, target, (int) target_disp,
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
        opal_atomic_add_64(&module->opcount, 1);
        request->ops_expected = 1;
        ret = ompi_datatype_type_size(origin_dt, &length);
        if (OMPI_SUCCESS != ret) {
            OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
            return ret;
        }
        length *= origin_count;
        ompi_osc_portals4_get_md(origin_addr, module->req_md_h, &md_h, &md_base);
        ret = PtlPut(md_h,
                     (ptl_size_t) ((char*) origin_addr - (char*) md_base),
                     length,
                     PTL_ACK_REQ,
                     peer,
                     module->pt_idx,
                     module->match_bits,
                     offset,
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
    size_t length;
    size_t offset;
    ptl_handle_md_t md_h;
    void *md_base;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "rget: 0x%lx, %d, %s, %d, %d, %d, %s, 0x%lx",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, target, (int) target_disp,
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
        opal_atomic_add_64(&module->opcount, 1);
        request->ops_expected = 1;
        ret = ompi_datatype_type_size(origin_dt, &length);
        if (OMPI_SUCCESS != ret) {
            OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
            return ret;
        }
        length *= origin_count;
        ompi_osc_portals4_get_md(origin_addr, module->req_md_h, &md_h, &md_base);
        ret = PtlGet(md_h,
                     (ptl_size_t) ((char*) origin_addr - (char*) md_base),
                     length,
                     peer,
                     module->pt_idx,
                     module->match_bits,
                     offset,
                     request);
        if (OMPI_SUCCESS != ret) {
            OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
            return ret;
        }
    }

    return OMPI_SUCCESS;
}


int
ompi_osc_portals4_raccumulate(void *origin_addr,
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
    size_t length, sent;
    size_t offset;
    ptl_op_t ptl_op;
    ptl_datatype_t ptl_dt;
    ptl_handle_md_t md_h;
    void *md_base;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "raccumulate: 0x%lx, %d, %s, %d, %d, %d, %s, %s 0x%lx",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, target, (int) target_disp,
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

        ret = ompi_datatype_type_size(origin_dt, &length);
        if (OMPI_SUCCESS != ret) {
            OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
            return ret;
        }
        length *= origin_count;
        sent = 0;

        ompi_osc_portals4_get_md(origin_addr, module->req_md_h, &md_h, &md_base);
        md_offset = ((char*) origin_addr - (char*) md_base);

        do {
            size_t msg_length = MIN(module->atomic_max, length - sent);
            opal_atomic_add_64(&module->opcount, 1);
            request->ops_expected++;

            if (MPI_REPLACE == op) {
                ret = PtlPut(md_h,
                             md_offset + sent,
                             msg_length,
                             PTL_ACK_REQ,
                             peer,
                             module->pt_idx,
                             module->match_bits,
                             offset + sent,
                             request,
                             0);
            } else {
                ret = ompi_osc_portals4_get_dt(origin_dt, &ptl_dt);
                if (OMPI_SUCCESS != ret) return ret;

                ret = ompi_osc_portals4_get_op(op, &ptl_op);
                if (OMPI_SUCCESS != ret) return ret;

                ret = PtlAtomic(md_h,
                                offset + sent,
                                msg_length,
                                PTL_ACK_REQ,
                                peer,
                                module->pt_idx,
                                module->match_bits,
                                offset + sent,
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
ompi_osc_portals4_rget_accumulate(void *origin_addr, 
                                  int origin_count, 
                                  struct ompi_datatype_t *origin_dt,
                                  void *result_addr, 
                                  int result_count, 
                                  struct ompi_datatype_t *result_dt,
                                  int target, 
                                  MPI_Aint target_disp, 
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
    size_t length, sent;
    size_t offset;
    ptl_op_t ptl_op;
    ptl_datatype_t ptl_dt;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "rget_accumulate: 0x%lx, %d, %s, 0x%lx, %d, %s, %d, %d, %d, %s, %s, 0x%lx",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, (unsigned long) result_addr,
                         result_count, result_dt->name,
                         target, (int) target_disp,
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
            ptl_handle_md_t result_md_h, origin_md_h;
            void *result_md_base, *origin_md_base;
            ptl_size_t result_md_offset, origin_md_offset;

            ret = ompi_datatype_type_size(origin_dt, &length);
            if (OMPI_SUCCESS != ret) {
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                return ret;
            }
            length *= origin_count;

            ompi_osc_portals4_get_md(result_addr, module->req_md_h, &result_md_h, &result_md_base);
            result_md_offset = ((char*) result_addr - (char*) result_md_base);
            ompi_osc_portals4_get_md(origin_addr, module->md_h, &origin_md_h, &origin_md_base);
            origin_md_offset = ((char*) origin_addr - (char*) origin_md_base);

            do {
                size_t msg_length = MIN(module->fetch_atomic_max, length - sent);

                opal_atomic_add_64(&module->opcount, 1);
                request->ops_expected++;

                ret = PtlSwap(result_md_h,
                              result_md_offset + sent,
                              origin_md_h,
                              origin_md_offset + sent,
                              msg_length,
                              peer,
                              module->pt_idx,
                              module->match_bits,
                              offset + sent,
                              request,
                              0,
                              NULL,
                              PTL_SWAP,
                              ptl_dt);
                sent += msg_length;
            } while (sent < length);
        } else if (MPI_NO_OP == op) {
            ptl_handle_md_t md_h;
            void *md_base;
            ptl_size_t md_offset;

            ret = ompi_datatype_type_size(target_dt, &length);
            if (OMPI_SUCCESS != ret) {
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                return ret;
            }
            length *= target_count;

            ompi_osc_portals4_get_md(result_addr, module->req_md_h, &md_h, &md_base);
            md_offset = ((char*) result_addr - (char*) md_base);

            do {
                size_t msg_length = MIN(module->fetch_atomic_max, length - sent);

                opal_atomic_add_64(&module->opcount, 1);
                request->ops_expected++;

                ret = PtlGet(md_h,
                             md_offset + sent,
                             msg_length,
                             peer,
                             module->pt_idx,
                             module->match_bits,
                             offset + sent,
                             request);
                sent += msg_length;
            } while (sent < length);
        } else {
            ptl_handle_md_t result_md_h, origin_md_h;
            void *result_md_base, *origin_md_base;
            ptl_size_t result_md_offset, origin_md_offset;

            ret = ompi_datatype_type_size(origin_dt, &length);
            if (OMPI_SUCCESS != ret) {
                OMPI_OSC_PORTALS4_REQUEST_RETURN(request);
                return ret;
            }
            length *= origin_count;

            ompi_osc_portals4_get_md(result_addr, module->req_md_h, &result_md_h, &result_md_base);
            result_md_offset = ((char*) result_addr - (char*) result_md_base);
            ompi_osc_portals4_get_md(origin_addr, module->md_h, &origin_md_h, &origin_md_base);
            origin_md_offset = ((char*) origin_addr - (char*) origin_md_base);

            ret = ompi_osc_portals4_get_dt(origin_dt, &ptl_dt);
            if (OMPI_SUCCESS != ret) return ret;

            ret = ompi_osc_portals4_get_op(op, &ptl_op);
            if (OMPI_SUCCESS != ret) return ret;

            do {
                size_t msg_length = MIN(module->fetch_atomic_max, length - sent);

                opal_atomic_add_64(&module->opcount, 1);
                request->ops_expected++;

                ret = PtlFetchAtomic(result_md_h,
                                     result_md_offset + sent,
                                     origin_md_h,
                                     origin_md_offset + sent,
                                     msg_length,
                                     peer,
                                     module->pt_idx,
                                     module->match_bits,
                                     offset + sent,
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
ompi_osc_portals4_put(void *origin_addr,
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
    size_t length;
    size_t offset;
    ptl_handle_md_t md_h;
    void *md_base;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "put: 0x%lx, %d, %s, %d, %d, %d, %s, 0x%lx",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, target, (int) target_disp,
                         target_count, target_dt->name,
                         (unsigned long) win));

    offset = get_displacement(module, target) * target_disp;

    if (!ompi_datatype_is_contiguous_memory_layout(origin_dt, origin_count) ||
        !ompi_datatype_is_contiguous_memory_layout(target_dt, target_count)) {
        opal_output(ompi_osc_base_framework.framework_output,
                    "MPI_Put: transfer of non-contiguous memory is not currently supported.\n");
        return OMPI_ERR_NOT_SUPPORTED;
    } else {
        opal_atomic_add_64(&module->opcount, 1);
        ret = ompi_datatype_type_size(origin_dt, &length);
        if (OMPI_SUCCESS != ret) {
            return ret;
        }
        length *= origin_count;
        ompi_osc_portals4_get_md(origin_addr, module->md_h, &md_h, &md_base);
        ret = PtlPut(md_h,
                     (ptl_size_t) ((char*) origin_addr - (char*) md_base),
                     length,
                     PTL_ACK_REQ,
                     peer,
                     module->pt_idx,
                     module->match_bits,
                     offset,
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
    size_t length;
    size_t offset;
    ptl_handle_md_t md_h;
    void *md_base;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "get: 0x%lx, %d, %s, %d, %d, %d, %s, 0x%lx",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, target, (int) target_disp,
                         target_count, target_dt->name,
                         (unsigned long) win));

    offset = get_displacement(module, target) * target_disp;

    if (!ompi_datatype_is_contiguous_memory_layout(origin_dt, origin_count) ||
        !ompi_datatype_is_contiguous_memory_layout(target_dt, target_count)) {
        opal_output(ompi_osc_base_framework.framework_output,
                    "MPI_Get: transfer of non-contiguous memory is not currently supported.\n");
        return OMPI_ERR_NOT_SUPPORTED;
    } else {
        opal_atomic_add_64(&module->opcount, 1);
        ret = ompi_datatype_type_size(origin_dt, &length);
        if (OMPI_SUCCESS != ret) {
            return ret;
        }
        length *= origin_count;
        ompi_osc_portals4_get_md(origin_addr, module->md_h, &md_h, &md_base);
        ret = PtlGet(md_h,
                     (ptl_size_t) ((char*) origin_addr - (char*) md_base),
                     length,
                     peer,
                     module->pt_idx,
                     module->match_bits,
                     offset,
                     NULL);
        if (OMPI_SUCCESS != ret) {
            return ret;
        }
    }

    return OMPI_SUCCESS;
}


int
ompi_osc_portals4_accumulate(void *origin_addr,
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
    size_t length, sent;
    size_t offset;
    ptl_op_t ptl_op;
    ptl_datatype_t ptl_dt;
    ptl_handle_md_t md_h;
    void *md_base;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "accumulate: 0x%lx, %d, %s, %d, %d, %d, %s, %s, 0x%lx",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, target, (int) target_disp,
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

        ret = ompi_datatype_type_size(origin_dt, &length);
        if (OMPI_SUCCESS != ret) {
            return ret;
        }
        length *= origin_count;
        sent = 0;

        ompi_osc_portals4_get_md(origin_addr, module->md_h, &md_h, &md_base);
        md_offset = ((char*) origin_addr - (char*) md_base);

        do {
            size_t msg_length = MIN(module->atomic_max, length - sent);
            opal_atomic_add_64(&module->opcount, 1);

            if (MPI_REPLACE == op) {
                ret = PtlPut(md_h,
                             md_offset + sent,
                             msg_length,
                             PTL_ACK_REQ,
                             peer,
                             module->pt_idx,
                             module->match_bits,
                             offset + sent,
                             NULL,
                             0);
            } else {
                ret = ompi_osc_portals4_get_dt(origin_dt, &ptl_dt);
                if (OMPI_SUCCESS != ret) return ret;

                ret = ompi_osc_portals4_get_op(op, &ptl_op);
                if (OMPI_SUCCESS != ret) return ret;

                ret = PtlAtomic(md_h,
                                md_offset + sent,
                                msg_length,
                                PTL_ACK_REQ,
                                peer,
                                module->pt_idx,
                                module->match_bits,
                                offset + sent,
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
ompi_osc_portals4_get_accumulate(void *origin_addr, 
                                 int origin_count, 
                                 struct ompi_datatype_t *origin_dt,
                                 void *result_addr, 
                                 int result_count, 
                                 struct ompi_datatype_t *result_dt,
                                 int target, 
                                 MPI_Aint target_disp, 
                                 int target_count,
                                 struct ompi_datatype_t *target_dt,
                                 struct ompi_op_t *op, 
                                 struct ompi_win_t *win)
{
    int ret;
    ompi_osc_portals4_module_t *module =
        (ompi_osc_portals4_module_t*) win->w_osc_module;
    ptl_process_t peer = ompi_osc_portals4_get_peer(module, target);
    size_t length, sent;
    size_t offset;
    ptl_op_t ptl_op;
    ptl_datatype_t ptl_dt;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "get_accumulate: 0x%lx, %d, %s, 0x%lx, %d, %s, %d, %d, %d, %s, %s, 0x%lx",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, (unsigned long) result_addr,
                         result_count, result_dt->name,
                         target, (int) target_disp,
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
            ptl_handle_md_t result_md_h, origin_md_h;
            void *result_md_base, *origin_md_base;
            ptl_size_t result_md_offset, origin_md_offset;

            ret = ompi_datatype_type_size(origin_dt, &length);
            if (OMPI_SUCCESS != ret) {
                return ret;
            }
            length *= origin_count;

            ompi_osc_portals4_get_md(result_addr, module->md_h, &result_md_h, &result_md_base);
            result_md_offset = ((char*) result_addr - (char*) result_md_base);
            ompi_osc_portals4_get_md(origin_addr, module->md_h, &origin_md_h, &origin_md_base);
            origin_md_offset = ((char*) origin_addr - (char*) origin_md_base);

            do {
                size_t msg_length = MIN(module->fetch_atomic_max, length - sent);

                opal_atomic_add_64(&module->opcount, 1);

                ret = PtlSwap(result_md_h,
                              result_md_offset + sent,
                              origin_md_h,
                              origin_md_offset + sent,
                              msg_length,
                              peer,
                              module->pt_idx,
                              module->match_bits,
                              offset + sent,
                              NULL,
                              0,
                              NULL,
                              PTL_SWAP,
                              ptl_dt);
                sent += msg_length;
            } while (sent < length);
        } else if (MPI_NO_OP == op) {
            ptl_handle_md_t md_h;
            void *md_base;
            ptl_size_t md_offset;

            ret = ompi_datatype_type_size(target_dt, &length);
            if (OMPI_SUCCESS != ret) {
                return ret;
            }
            length *= target_count;

            ompi_osc_portals4_get_md(result_addr, module->md_h, &md_h, &md_base);
            md_offset = ((char*) result_addr - (char*) md_base);

            do {
                size_t msg_length = MIN(module->fetch_atomic_max, length - sent);

                opal_atomic_add_64(&module->opcount, 1);

                ret = PtlGet(md_h,
                             md_offset + sent,
                             msg_length,
                             peer,
                             module->pt_idx,
                             module->match_bits,
                             offset + sent,
                             NULL);
                sent += msg_length;
            } while (sent < length);
        } else {
            ptl_handle_md_t result_md_h, origin_md_h;
            void *result_md_base, *origin_md_base;
            ptl_size_t result_md_offset, origin_md_offset; 

            ret = ompi_datatype_type_size(origin_dt, &length);
            if (OMPI_SUCCESS != ret) {
                return ret;
            }
            length *= origin_count;

            ompi_osc_portals4_get_md(result_addr, module->md_h, &result_md_h, &result_md_base);
            result_md_offset = ((char*) result_addr - (char*) result_md_base);
            ompi_osc_portals4_get_md(origin_addr, module->md_h, &origin_md_h, &origin_md_base);
            origin_md_offset = ((char*) origin_addr - (char*) origin_md_base);

            ret = ompi_osc_portals4_get_dt(origin_dt, &ptl_dt);
            if (OMPI_SUCCESS != ret) return ret;

            ret = ompi_osc_portals4_get_op(op, &ptl_op);
            if (OMPI_SUCCESS != ret) return ret;


            do {
                size_t msg_length = MIN(module->fetch_atomic_max, length - sent);

                opal_atomic_add_64(&module->opcount, 1);

                ret = PtlFetchAtomic(result_md_h,
                                     result_md_offset + sent,
                                     origin_md_h,
                                     origin_md_offset + sent,
                                     msg_length,
                                     peer,
                                     module->pt_idx,
                                     module->match_bits,
                                     offset + sent,
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
ompi_osc_portals4_compare_and_swap(void *origin_addr,
                                   void *compare_addr,
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
    ptl_handle_md_t result_md_h, origin_md_h;
    void *result_md_base, *origin_md_base;
    ptl_size_t result_md_offset, origin_md_offset;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "compare_and_swap: 0x%lx, 0x%lx, 0x%lx, %s, %d, %d, 0x%lx",
                         (unsigned long) origin_addr, 
                         (unsigned long) compare_addr,
                         (unsigned long) result_addr,
                         dt->name, target, (int) target_disp,
                         (unsigned long) win));

    ret = ompi_osc_portals4_get_dt(dt, &ptl_dt);
    if (OMPI_SUCCESS != ret) return ret;

    offset = get_displacement(module, target) * target_disp;

    ret = ompi_datatype_type_size(dt, &length);
    if (OMPI_SUCCESS != ret) return ret;

    assert(length < module->fetch_atomic_max);

    ompi_osc_portals4_get_md(result_addr, module->md_h, &result_md_h, &result_md_base);
    result_md_offset = ((char*) result_addr - (char*) result_md_base);
    ompi_osc_portals4_get_md(origin_addr, module->md_h, &origin_md_h, &origin_md_base);
    origin_md_offset = ((char*) origin_addr - (char*) origin_md_base);

    opal_atomic_add_64(&module->opcount, 1);

    ret = PtlSwap(result_md_h,
                  result_md_offset,
                  origin_md_h,
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
ompi_osc_portals4_fetch_and_op(void *origin_addr,
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
                         "fetch_and_op: 0x%lx, 0x%lx, %s, %d, %d, %s, 0x%lx",
                         (unsigned long) origin_addr, 
                         (unsigned long) result_addr,
                         dt->name, target, (int) target_disp,
                         op->o_name,
                         (unsigned long) win));

    ret = ompi_osc_portals4_get_dt(dt, &ptl_dt);
    if (OMPI_SUCCESS != ret) return ret;

    offset = get_displacement(module, target) * target_disp;

    ret = ompi_datatype_type_size(dt, &length);
    if (OMPI_SUCCESS != ret) return ret;

    assert(length < module->fetch_atomic_max);

    opal_atomic_add_64(&module->opcount, 1);

    if (MPI_REPLACE == op) {
        ptl_handle_md_t result_md_h, origin_md_h;
        void *result_md_base, *origin_md_base;
        ptl_size_t result_md_offset, origin_md_offset;

        ompi_osc_portals4_get_md(result_addr, module->md_h, &result_md_h, &result_md_base);
        result_md_offset = ((char*) result_addr - (char*) result_md_base);
        ompi_osc_portals4_get_md(origin_addr, module->md_h, &origin_md_h, &origin_md_base);
        origin_md_offset = ((char*) origin_addr - (char*) origin_md_base);

        ret = PtlSwap(result_md_h,
                      result_md_offset,
                      origin_md_h,
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
        ptl_handle_md_t md_h;
        void *md_base;
        ptl_size_t md_offset;

        ompi_osc_portals4_get_md(result_addr, module->md_h, &md_h, &md_base);
        md_offset = ((char*) result_addr - (char*) md_base);

        ret = PtlGet(md_h,
                     md_offset,
                     length,
                     peer,
                     module->pt_idx,
                     module->match_bits,
                     offset,
                     NULL);
    } else {
        ptl_handle_md_t result_md_h, origin_md_h;
        void *result_md_base, *origin_md_base;
        ptl_size_t result_md_offset, origin_md_offset; 

        ret = ompi_osc_portals4_get_op(op, &ptl_op);
        if (OMPI_SUCCESS != ret) return ret;

        ompi_osc_portals4_get_md(result_addr, module->md_h, &result_md_h, &result_md_base);
        result_md_offset = ((char*) result_addr - (char*) result_md_base);
        ompi_osc_portals4_get_md(origin_addr, module->md_h, &origin_md_h, &origin_md_base);
        origin_md_offset = ((char*) origin_addr - (char*) origin_md_base);

        ret = PtlFetchAtomic(result_md_h,
                             result_md_offset,
                             origin_md_h,
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
