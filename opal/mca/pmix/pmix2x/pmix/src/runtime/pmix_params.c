/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2014 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2008-2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2010-2014 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014      Hochschule Esslingen.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016-2017 Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "pmix_config.h"

#include "src/include/types.h"
#include "src/mca/base/pmix_mca_base_var.h"
#include "src/runtime/pmix_rte.h"
#include "src/util/timings.h"

#if PMIX_ENABLE_TIMING
char *pmix_timing_output = NULL;
bool pmix_timing_overhead = true;
#endif

static bool pmix_register_done = false;
char *pmix_net_private_ipv4 = NULL;
int pmix_event_caching_window = 1;
bool pmix_suppress_missing_data_warning = false;

pmix_status_t pmix_register_params(void)
{
    int ret;

    if (pmix_register_done) {
        return PMIX_SUCCESS;
    }

    pmix_register_done = true;

#if PMIX_ENABLE_TIMING
    pmix_timing_output = NULL;
    (void) pmix_mca_base_var_register ("pmix", "pmix", NULL, "timing_output",
                                  "The name of output file for timing information. If this parameter is not set then output will be directed into PMIX debug channel.",
                                  PMIX_MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                  PMIX_INFO_LVL_9, PMIX_MCA_BASE_VAR_SCOPE_ALL,
                                  &pmix_timing_output);

    pmix_timing_overhead = true;
    (void) pmix_mca_base_var_register ("pmix", "pmix", NULL, "timing_overhead",
                                  "Timing framework introduce additional overhead (malloc's mostly)."
                                  " The time spend in such costly routines is measured and may be accounted"
                                  " (subtracted from timestamps). 'true' means consider overhead, 'false' - ignore (default: true).",
                                  PMIX_MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                  PMIX_INFO_LVL_9, PMIX_MCA_BASE_VAR_SCOPE_ALL,
                                  &pmix_timing_overhead);
#endif

    /* RFC1918 defines
       - 10.0.0./8
       - 172.16.0.0/12
       - 192.168.0.0/16

       RFC3330 also mentions
       - 169.254.0.0/16 for DHCP onlink iff there's no DHCP server
    */
    pmix_net_private_ipv4 = "10.0.0.0/8;172.16.0.0/12;192.168.0.0/16;169.254.0.0/16";
    ret = pmix_mca_base_var_register ("pmix", "pmix", "net", "private_ipv4",
                                      "Semicolon-delimited list of CIDR notation entries specifying what networks are considered \"private\" (default value based on RFC1918 and RFC3330)",
                                      PMIX_MCA_BASE_VAR_TYPE_STRING, NULL, 0, PMIX_MCA_BASE_VAR_FLAG_SETTABLE,
                                      PMIX_INFO_LVL_3, PMIX_MCA_BASE_VAR_SCOPE_ALL_EQ,
                                      &pmix_net_private_ipv4);
    if (0 > ret) {
        return ret;
    }

    (void) pmix_mca_base_var_register ("pmix", "pmix", NULL, "event_caching_window",
                                  "Time (in seconds) to aggregate events before reporting them - this "
                                  "suppresses event cascades when processes abnormally terminate",
                                  PMIX_MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                  PMIX_INFO_LVL_1, PMIX_MCA_BASE_VAR_SCOPE_ALL,
                                  &pmix_event_caching_window);

    (void) pmix_mca_base_var_register ("pmix", "pmix", NULL, "suppress_missing_data_warning",
                                  "Suppress warning that PMIx is missing job-level data that "
                                  "is supposed to be provided by the host RM.",
                                  PMIX_MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                  PMIX_INFO_LVL_1, PMIX_MCA_BASE_VAR_SCOPE_ALL,
                                  &pmix_suppress_missing_data_warning);

    return PMIX_SUCCESS;
}

pmix_status_t pmix_deregister_params(void)
{
    pmix_register_done = false;

    return PMIX_SUCCESS;
}
