/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Sandia National Laboratories. All rights
 *                         reserved.
 * Copyright (c) 2008-2016 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <string.h>
#include <errno.h>

#include "opal/mca/base/mca_base_var.h"
#include "opal/util/argv.h"

#include "opal/constants.h"

#if BTL_IN_OPAL
#include "opal/mca/btl/btl.h"
#include "opal/mca/btl/base/base.h"
#else
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/base.h"
#endif

#include "btl_usnic.h"
#include "btl_usnic_frag.h"
#include "btl_usnic_endpoint.h"
#include "btl_usnic_module.h"


/*
 * Local flags
 */
enum {
    REGINT_NEG_ONE_OK = 0x01,
    REGINT_GE_ZERO = 0x02,
    REGINT_GE_ONE = 0x04,
    REGINT_NONZERO = 0x08,

    REGINT_MAX = 0x88
};


enum {
    REGSTR_EMPTY_OK = 0x01,

    REGSTR_MAX = 0x88
};


/*
 * utility routine for string parameter registration
 */
static int reg_string(const char* param_name,
                      const char* help_string,
                      const char* default_value, char **storage,
                      int flags, int level)
{
    *storage = (char*) default_value;
    mca_base_component_var_register(&mca_btl_usnic_component.super.btl_version,
                                    param_name, help_string,
                                    MCA_BASE_VAR_TYPE_STRING,
                                    NULL,
                                    0,
                                    0,
                                    level,
                                    MCA_BASE_VAR_SCOPE_READONLY,
                                    storage);

    if (0 == (flags & REGSTR_EMPTY_OK) &&
        (NULL == *storage || 0 == strlen(*storage))) {
        opal_output(0, "Bad parameter value for parameter \"%s\"",
                    param_name);
        return OPAL_ERR_BAD_PARAM;
    }

    return OPAL_SUCCESS;
}


/*
 * utility routine for integer parameter registration
 */
static int reg_int(const char* param_name,
                   const char* help_string,
                   int default_value, int *storage, int flags, int level)
{
    *storage = default_value;
    mca_base_component_var_register(&mca_btl_usnic_component.super.btl_version,
                                    param_name, help_string,
                                    MCA_BASE_VAR_TYPE_INT,
                                    NULL,
                                    0,
                                    0,
                                    level,
                                    MCA_BASE_VAR_SCOPE_READONLY,
                                    storage);

    if (0 != (flags & REGINT_NEG_ONE_OK) && -1 == *storage) {
        return OPAL_SUCCESS;
    }
    if ((0 != (flags & REGINT_GE_ZERO) && *storage < 0) ||
        (0 != (flags & REGINT_GE_ONE) && *storage < 1) ||
        (0 != (flags & REGINT_NONZERO) && 0 == *storage)) {
        opal_output(0, "Bad parameter value for parameter \"%s\"",
                    param_name);
        return OPAL_ERR_BAD_PARAM;
    }

    return OPAL_SUCCESS;
}


/*
 * utility routine for integer parameter registration
 */
static int reg_bool(const char* param_name,
                    const char* help_string,
                    bool default_value, bool *storage, int level)
{
    *storage = default_value;
    mca_base_component_var_register(&mca_btl_usnic_component.super.btl_version,
                                    param_name, help_string,
                                    MCA_BASE_VAR_TYPE_BOOL,
                                    NULL,
                                    0,
                                    0,
                                    level,
                                    MCA_BASE_VAR_SCOPE_READONLY,
                                    storage);

    return OPAL_SUCCESS;
}


int opal_btl_usnic_component_register(void)
{
    int tmp, ret = 0;
    static int max_modules;
    static int stats_relative;
    static int want_numa_device_assignment;
    static int sd_num;
    static int rd_num;
    static int prio_sd_num;
    static int prio_rd_num;
    static int cq_num;
    static int av_eq_num;
    static int udp_port_base;
    static int max_tiny_msg_size;
    static int eager_limit;
    static int rndv_eager_limit;
    static int pack_lazy_threshold;
    static int max_short_packets;

#define CHECK(expr) do {\
        tmp = (expr); \
        if (OPAL_SUCCESS != tmp) ret = tmp; \
     } while (0)

    CHECK(reg_int("max_btls",
                  "Maximum number of usNICs to use (default: 0 = as many as are available)",
                  0, &max_modules,
                  REGINT_GE_ZERO, OPAL_INFO_LVL_2));
    mca_btl_usnic_component.max_modules = (size_t) max_modules;

    CHECK(reg_string("if_include",
                     "Comma-delimited list of usNIC devices/networks to be used (e.g. \"eth3,usnic_0,10.10.0.0/16\"; empty value means to use all available usNICs).  Mutually exclusive with btl_usnic_if_exclude.",
                     NULL, &mca_btl_usnic_component.if_include,
                     REGSTR_EMPTY_OK, OPAL_INFO_LVL_1));

    CHECK(reg_string("if_exclude",
                     "Comma-delimited list of usNIC devices/networks to be excluded (empty value means to not exclude any usNICs).  Mutually exclusive with btl_usnic_if_include.",
                     NULL, &mca_btl_usnic_component.if_exclude,
                     REGSTR_EMPTY_OK, OPAL_INFO_LVL_1));

    CHECK(reg_int("stats",
                  "A non-negative integer specifying the frequency at which each usnic BTL will output statistics (default: 0 seconds, meaning that statistics are disabled)",
                  0, &mca_btl_usnic_component.stats_frequency, 0,
                  OPAL_INFO_LVL_4));
    mca_btl_usnic_component.stats_enabled =
        (bool) (mca_btl_usnic_component.stats_frequency > 0);

    CHECK(reg_int("stats_relative",
                  "If stats are enabled, output relative stats between the timestamps (vs. cumulative stats since the beginning of the job) (default: 0 -- i.e., absolute)",
                  0, &stats_relative, 0, OPAL_INFO_LVL_4));
    mca_btl_usnic_component.stats_relative = (bool) stats_relative;

    CHECK(reg_string("mpool", "Name of the memory pool to be used",
                     "grdma", &mca_btl_usnic_component.usnic_mpool_name, 0,
                     OPAL_INFO_LVL_5));

    want_numa_device_assignment = OPAL_HAVE_HWLOC ? 1 : -1;
    CHECK(reg_int("want_numa_device_assignment",
                  "If 1, use only Cisco VIC ports thare are a minimum NUMA distance from the MPI process for short messages.  If 0, use all available Cisco VIC ports for short messages.  This parameter is meaningless (and ignored) unless MPI proceses are bound to processor cores.  Defaults to 1 if NUMA support is included in Open MPI; -1 otherwise.",
                  want_numa_device_assignment,
                  &want_numa_device_assignment,
                  0, OPAL_INFO_LVL_5));
    mca_btl_usnic_component.want_numa_device_assignment =
        (1 == want_numa_device_assignment) ? true : false;

    CHECK(reg_int("sd_num", "Maximum send descriptors to post (-1 = pre-set defaults; depends on number and type of devices available)",
                  -1, &sd_num, REGINT_NEG_ONE_OK, OPAL_INFO_LVL_5));
    mca_btl_usnic_component.sd_num = (int32_t) sd_num;

    CHECK(reg_int("rd_num", "Number of pre-posted receive buffers (-1 = pre-set defaults; depends on number and type of devices available)",
                  -1, &rd_num, REGINT_NEG_ONE_OK, OPAL_INFO_LVL_5));
    mca_btl_usnic_component.rd_num = (int32_t) rd_num;

    CHECK(reg_int("prio_sd_num", "Maximum priority send descriptors to post (-1 = pre-set defaults; depends on number and type of devices available)",
                  -1, &prio_sd_num, REGINT_NEG_ONE_OK, OPAL_INFO_LVL_5));
    mca_btl_usnic_component.prio_sd_num = (int32_t) prio_sd_num;

    CHECK(reg_int("prio_rd_num", "Number of pre-posted priority receive buffers (-1 = pre-set defaults; depends on number and type of devices available)",
                  -1, &prio_rd_num, REGINT_NEG_ONE_OK, OPAL_INFO_LVL_5));
    mca_btl_usnic_component.prio_rd_num = (int32_t) prio_rd_num;

    CHECK(reg_int("cq_num", "Number of completion queue entries (-1 = pre-set defaults; depends on number and type of devices available; will error if (sd_num+rd_num)>cq_num)",
                  -1, &cq_num, REGINT_NEG_ONE_OK, OPAL_INFO_LVL_5));
    mca_btl_usnic_component.cq_num = (int32_t) cq_num;

    CHECK(reg_int("av_eq_num", "Number of event queue entries for peer address resolution",
                  1024, &av_eq_num, REGINT_GE_ONE, OPAL_INFO_LVL_5));
    mca_btl_usnic_component.av_eq_num = (int32_t) av_eq_num;

    CHECK(reg_int("base_udp_port", "Base UDP port to use for usNIC communications.  If 0, system will pick the port number.  If non-zero, it will be added to each process' local rank to obtain the final port number (default: 0)",
                  0, &udp_port_base, REGINT_GE_ZERO, OPAL_INFO_LVL_5));
    mca_btl_usnic_component.udp_port_base = (int) udp_port_base;

    CHECK(reg_int("retrans_timeout", "Number of microseconds before retransmitting a frame",
                  5000, &mca_btl_usnic_component.retrans_timeout,
                  REGINT_GE_ONE, OPAL_INFO_LVL_5));

    CHECK(reg_int("priority_limit", "Max size of \"priority\" messages (0 = use pre-set defaults; depends on number and type of devices available)",
                  0, &max_tiny_msg_size,
                  REGINT_GE_ZERO, OPAL_INFO_LVL_5));
    opal_btl_usnic_module_template.max_tiny_msg_size =
        (size_t) max_tiny_msg_size;

    CHECK(reg_int("eager_limit", "Eager send limit (0 = use pre-set defaults; depends on number and type of devices available)",
                  0, &eager_limit, REGINT_GE_ZERO, OPAL_INFO_LVL_5));
    opal_btl_usnic_module_template.super.btl_eager_limit = eager_limit;

    CHECK(reg_int("rndv_eager_limit", "Eager rendezvous limit (0 = use pre-set defaults; depends on number and type of devices available)",
                  0, &rndv_eager_limit, REGINT_GE_ZERO, OPAL_INFO_LVL_5));
    opal_btl_usnic_module_template.super.btl_rndv_eager_limit =
        rndv_eager_limit;

    CHECK(reg_int("pack_lazy_threshold", "Convertor packing on-the-fly threshold (-1 = always pack eagerly, 0 = always pack lazily, otherwise will pack on the fly if fragment size is > limit)",
                  USNIC_DFLT_PACK_LAZY_THRESHOLD, &pack_lazy_threshold, REGINT_NEG_ONE_OK, OPAL_INFO_LVL_5));
    mca_btl_usnic_component.pack_lazy_threshold = pack_lazy_threshold;

    CHECK(reg_int("max_short_packets", "Number of abnormally-short packets received before outputting a warning (0 = never show the warning)",
                  25, &max_short_packets,
                  REGINT_GE_ZERO, OPAL_INFO_LVL_5));
    mca_btl_usnic_component.max_short_packets = max_short_packets;

    /* Default to bandwidth auto-detection */
    opal_btl_usnic_module_template.super.btl_bandwidth = 0;
    opal_btl_usnic_module_template.super.btl_latency = 2;

    /* Show "cannot find route" warnings? */
    mca_btl_usnic_component.show_route_failures = true;
    CHECK(reg_bool("show_route_failures",
                   "Whether to show a warning when route failures between MPI process peers are detected (default = 1, enabled; 0 = disabled)",
                   mca_btl_usnic_component.show_route_failures,
                   &mca_btl_usnic_component.show_route_failures,
                   OPAL_INFO_LVL_3));

    /* Connectivity verification */
    mca_btl_usnic_component.connectivity_enabled = true;
    CHECK(reg_bool("connectivity_check",
                   "Whether to enable the usNIC connectivity check upon first send (default = 1, enabled; 0 = disabled)",
                   mca_btl_usnic_component.connectivity_enabled,
                   &mca_btl_usnic_component.connectivity_enabled,
                   OPAL_INFO_LVL_3));

    mca_btl_usnic_component.connectivity_ack_timeout = 250;
    CHECK(reg_int("connectivity_ack_timeout",
                  "Timeout, in milliseconds, while waiting for an ACK while verification connectivity between usNIC interfaces.  If 0, the connectivity check is disabled (must be >=0).",
                  mca_btl_usnic_component.connectivity_ack_timeout,
                  &mca_btl_usnic_component.connectivity_ack_timeout,
                  REGINT_GE_ZERO, OPAL_INFO_LVL_3));

    mca_btl_usnic_component.connectivity_num_retries = 40;
    CHECK(reg_int("connectivity_error_num_retries",
                  "Number of times to retry usNIC connectivity verification before aborting the MPI job (must be >0).",
                  mca_btl_usnic_component.connectivity_num_retries,
                  &mca_btl_usnic_component.connectivity_num_retries,
                  REGINT_GE_ONE, OPAL_INFO_LVL_3));

    mca_btl_usnic_component.connectivity_map_prefix = NULL;
    CHECK(reg_string("connectivity_map",
                     "Write a per-process file containing the usNIC connectivity map.  If this parameter is specified, it is the filename prefix emitted by each MPI process.  The full filename emitted by each process is of the form: <prefix>-<hostname>.<pid>.<jobid>.<MCW rank>.txt.",
                     mca_btl_usnic_component.connectivity_map_prefix,
                     &mca_btl_usnic_component.connectivity_map_prefix,
                     REGSTR_EMPTY_OK, OPAL_INFO_LVL_3));

    return ret;
}
