/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"
#include "btl_udapl.h"
#include "btl_udapl_mca.h"
#include <string.h>

/*
 * Utility routine for string parameter registration.
 * 
 * @param param_name (IN)        MCA parameter name
 * @param param_desc (IN)        MCA parameter description
 * @param default_value (IN)     MCA parameter default value
 * @param out_value (OUT)        value of MCA parameter; either default,
 *                                  or value as determined from typical
 *                                  MCA parameter setting methods 
 * @param flags (IN)             MCA parameter boundary flag
 * @return                       OMPI_SUCCESS or OMPI_ERR_BAD_PARAM
 */
static inline int mca_btl_udapl_reg_string(const char* param_name,
                                           const char* param_desc,
                                           const char* default_value,
                                           char **out_value, int flags)
{
    char *value;
    
    mca_base_param_reg_string(&mca_btl_udapl_component.super.btl_version, 
        param_name, param_desc, false, false, default_value, &value);

    if (NULL == value) {
        BTL_ERROR(("ERROR: MCA Parameter %s : Value (NULL) out of range : "
            "Default value (%s)\n \t Parameter Description : %s",
            param_name, default_value, param_desc));
        return OMPI_ERR_BAD_PARAM;
    }

    if ((flags & REGSTR_EMPTY_NOT_OK) && 0 == strlen(value)) {
        BTL_ERROR(("ERROR: MCA Parameter %s : Value (%s) out of range : "
            "Default value (%s)\n \t Parameter Description : %s",
            param_name, value, default_value, param_desc));
        return OMPI_ERR_BAD_PARAM;
    }

    *out_value = value;
    return OMPI_SUCCESS;
}


/*
 * Utility routine for integer parameter registration.
 *
 * @param param_name (IN)        MCA parameter name
 * @param param_desc (IN)        MCA parameter description
 * @param default_value (IN)     MCA parameter default value
 * @param out_value (OUT)        value of MCA parameter; either default,
 *                                  or value as determined from typical
 *                                  MCA parameter setting methods 
 * @param flags (IN)             MCA parameter boundary flag
 * @return                       OMPI_SUCCESS or OMPI_ERR_BAD_PARAM
 */
static inline int mca_btl_udapl_reg_int(const char* param_name,
                                        const char* param_desc,
                                        int default_value, int *out_value,
                                        int flags)
{
    int value;

    mca_base_param_reg_int(&mca_btl_udapl_component.super.btl_version, 
        param_name, param_desc, false, false, default_value, &value);

    if ((flags & REGINT_NEG_ONE_OK) && -1 == value) {
        *out_value = value;
        return OMPI_SUCCESS;
    }
    if (((flags & REGINT_GE_ZERO) && value < 0) ||
        ((flags & REGINT_GE_ONE) && value < 1) ||
        ((flags & REGINT_NONZERO) && 0 == value)) {
        BTL_ERROR(("ERROR: MCA Parameter %s : Value (%d) out of range : "
            "Default value (%d)\n \t Parameter Description : %s\n",
            param_name, value, default_value, param_desc));
        return OMPI_ERR_BAD_PARAM;
    }
    *out_value = value;
    return OMPI_SUCCESS;
}


/*
 * Register and check all MCA parameters
 *
 * @return                 OMPI_SUCCESS or OMPI_ERR_BAD_PARAM
 */
int mca_btl_udapl_register_mca_params(void) 
{
    int ival, rc, tmp_rc;

    rc = OMPI_SUCCESS;

    /* register uDAPL component parameters */
    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("free_list_num", 
        "Initial size of free lists (must be >= 1).", 
        8,
        &mca_btl_udapl_component.udapl_free_list_num,
        REGINT_GE_ONE), tmp_rc, rc);
    
    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("free_list_max",
        "Maximum size of free lists "
        "(-1 = infinite, otherwise must be >= 1).",
        -1,
        &mca_btl_udapl_component.udapl_free_list_max,
        REGINT_NEG_ONE_OK | REGINT_GE_ONE), tmp_rc, rc);

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("free_list_inc", 
        "Increment size of free lists (must be >= 1).",
        8,
        &mca_btl_udapl_component.udapl_free_list_inc,
        REGINT_GE_ONE), tmp_rc, rc);

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_string("mpool",
        "Name of the memory pool to be used.",
        "rdma",
        &mca_btl_udapl_component.udapl_mpool_name,
        REGSTR_EMPTY_NOT_OK), tmp_rc, rc);

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("max_modules",
        "Maximum number of supported HCAs.",
        8,
        &ival,
        REGINT_GE_ONE), tmp_rc, rc);
    mca_btl_udapl_component.udapl_max_btls = (uint32_t) ival;

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("num_recvs",
        "Total number of receive buffers to keep posted "
        "per endpoint (must be >= 1).",
        8,
        &mca_btl_udapl_component.udapl_num_recvs,
        REGINT_GE_ONE), tmp_rc, rc);

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("num_sends",
        "Maximum number of sends to post on an endpoint "
        "(must be >= 1).",
        7,
        &mca_btl_udapl_component.udapl_num_sends,
        REGINT_GE_ONE), tmp_rc, rc);

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("sr_win",
        "Window size at which point an explicit "
        "credit message will be generated (must be >= 1).",
        4,
        &mca_btl_udapl_component.udapl_sr_win,
        REGINT_GE_ONE), tmp_rc, rc);

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("eager_rdma_num",
        "Number of RDMA buffers to allocate "
        "for small messages (must be >= 1).",
        32,
        &mca_btl_udapl_component.udapl_eager_rdma_num,
        REGINT_GE_ONE), tmp_rc, rc); 
        
    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("max_eager_rdma_peers",
        "Maximum number of peers allowed to use "
        "RDMA for short messages (independently RDMA will "
        "still be used for large messages, (must be >= 0; "
        "if zero then RDMA will not be used for short messages).",
        16,
        &mca_btl_udapl_component.udapl_max_eager_rdma_peers,
        REGINT_GE_ZERO), tmp_rc, rc);
        
    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("eager_rdma_win",
        "Window size at which point an explicit "
        "credit message will be generated (must be >= 1).",
        28,
        &mca_btl_udapl_component.udapl_eager_rdma_win,
        REGINT_GE_ONE), tmp_rc, rc);

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("timeout",
        "Connection timeout, in microseconds.",
        MCA_BTL_UDAPL_CONN_TIMEOUT_DEFAULT,
        &ival,
        REGINT_GE_ONE), tmp_rc, rc);
    mca_btl_udapl_component.udapl_timeout = (uint32_t) ival;        

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("conn_priv_data",
        "Use connect private data to establish connections "
        "(not supported by all uDAPL implementations).",
        0,
        &mca_btl_udapl_component.udapl_conn_priv_data,
        REGINT_GE_ZERO), tmp_rc, rc);

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("async_events",
        "The asynchronous event queue will only be "
        "checked after entering progress this number of times.",
        100000000,
        &mca_btl_udapl_component.udapl_async_events,
        REGINT_GE_ONE), tmp_rc, rc);

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("buffer_alignment",
        "Preferred communication buffer alignment, "
        "in bytes (must be >= 1).",
        DAT_OPTIMAL_ALIGNMENT,
        &mca_btl_udapl_component.udapl_buffer_alignment,
        REGINT_GE_ONE), tmp_rc, rc);

    /* register uDAPL module parameters */
    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("async_evd_qlen",
        "The asynchronous event dispatcher queue length.",
        MCA_BTL_UDAPL_ASYNC_EVD_QLEN_DEFAULT,
        (int*)&mca_btl_udapl_module.udapl_async_evd_qlen,
        REGINT_GE_ONE), tmp_rc, rc);

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("conn_evd_qlen",
        "The connection event dispatcher queue length is "
        "a function of the number of connections expected.",
        MCA_BTL_UDAPL_CONN_EVD_QLEN_DEFAULT,
        (int*)&mca_btl_udapl_module.udapl_conn_evd_qlen,
        REGINT_GE_ONE), tmp_rc, rc);

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("dto_evd_qlen",
        "The data transfer operation event dispatcher queue length is "
        "a function of the number of connections as well as the "
        "maximum number of outstanding data transfer operations.",
        MCA_BTL_UDAPL_DTO_EVD_QLEN_DEFAULT,
        (int*)&mca_btl_udapl_module.udapl_dto_evd_qlen,
        REGINT_GE_ONE), tmp_rc, rc);

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("max_request_dtos",
        "Maximum number of outstanding "
        "submitted sends and rdma operations per endpoint, (see Section "
        "6.6.6 of uDAPL Spec.).",
        MCA_BTL_UDAPL_MAX_REQUEST_DTOS_DEFAULT,
        (int*)&mca_btl_udapl_module.udapl_max_request_dtos,
        REGINT_GE_ONE), tmp_rc, rc);

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("max_recv_dtos",
        "Maximum number of outstanding "
        "submitted receive operations per endpoint, (see Section "
        "6.6.6 of uDAPL Spec.).",
        MCA_BTL_UDAPL_MAX_RECV_DTOS_DEFAULT,
        (int*)&mca_btl_udapl_module.udapl_max_recv_dtos,
        REGINT_GE_ONE), tmp_rc, rc);

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("exclusivity",
        "uDAPL BTL exclusivity (must be >= 0).", 
        (MCA_BTL_EXCLUSIVITY_DEFAULT - 10),
        &ival,
        REGINT_GE_ZERO), tmp_rc, rc);
    mca_btl_udapl_module.super.btl_exclusivity = (uint32_t) ival;

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("eager_limit",
        "Eager send limit, in bytes (must be >= 1).",
        (8 * 1024),
        &ival,
        REGINT_GE_ONE), tmp_rc, rc);
    mca_btl_udapl_module.super.btl_eager_limit = (uint32_t) ival;
    
    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("min_send_size",
        "Minimum send size, in bytes (must be >= 1).",
        (16 * 1024),
        &ival,
        REGINT_GE_ONE), tmp_rc, rc);
    mca_btl_udapl_module.super.btl_min_send_size = (uint32_t) ival;

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("max_send_size",
        "Maximum send size, in bytes (must be >= 1).",
        (64 * 1024),
        &ival,
        REGINT_GE_ONE), tmp_rc, rc); 
    mca_btl_udapl_module.super.btl_max_send_size = (uint32_t) ival; 
    
    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("min_rdma_size",
        "Minimum RDMA size, in bytes (must be >= 1).",
        (512 * 1024),
        &ival,
        REGINT_GE_ONE), tmp_rc, rc);
    mca_btl_udapl_module.super.btl_min_rdma_size = (uint32_t) ival;

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("max_rdma_size",
        "Maximum RDMA size, in bytes (must be >= 1).",
        (128 * 1024),
        &ival,
        REGINT_GE_ONE), tmp_rc, rc); 
    mca_btl_udapl_module.super.btl_max_rdma_size = (uint32_t) ival; 

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("flags",
        "BTL flags, added together: PUT=2 (cannot be 0).",
        MCA_BTL_FLAGS_PUT,
        &ival,
        REGINT_GE_ZERO), tmp_rc, rc);
    mca_btl_udapl_module.super.btl_flags = (uint32_t) ival;
    
    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("bandwidth",
        "Approximate maximum bandwidth of network (must be >= 1).",
        225,
        &ival,
        REGINT_GE_ONE), tmp_rc, rc);
    mca_btl_udapl_module.super.btl_bandwidth = (uint32_t) ival;

    return rc;
}
