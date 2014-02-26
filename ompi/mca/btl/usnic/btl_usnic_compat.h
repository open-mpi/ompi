/*
 * Copyright (c) 2013-2014 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/* This header contains macros to help minimize usnic BTL differences between
 * v1.6, v1.7, and trunk. Note that we are stomping on other namespaces,
 * especially to make stock trunk code play nicely with v1.6 OMPI. */

#ifndef BTL_USNIC_COMPAT_H
#define BTL_USNIC_COMPAT_H

#if OMPI_MAJOR_VERSION == 1 && OMPI_MINOR_VERSION == 6
/* The usnic BTL will never be in the upstream v1.6 branch.  The only v1.6
 * variant will be a Cisco-supplied version. */

/* for per-file #ifdefs, esp. #includes */
#  define OMPI_BTL_USNIC_CISCO_V1_6 1

/* for some of the ORTE_* constants that we name-shift to OMPI_* here */
#  include "orte/mca/errmgr/errmgr.h"
#  include "orte/runtime/orte_globals.h"
#  include "orte/util/show_help.h"

#define opal_event_set(event_base_,A,B,C,D,E) \
    opal_event_set(A,B,C,D,E)
#  define opal_show_help orte_show_help
#  define mca_base_var_check_exclusive(S,A,B,C,D,E,F) \
    mca_base_param_check_exclusive_string(A,B,C,D,E,F)
#  define ompi_rte_compare_name_fields(a, b, c) \
    orte_util_compare_name_fields(a, b, c)
#  define OMPI_RTE_CMP_ALL ORTE_NS_CMP_ALL
#  define ompi_process_info orte_process_info
#  define ompi_rte_hash_name orte_util_hash_name
#  define OMPI_PROC_MY_NAME ORTE_PROC_MY_NAME
#  define OMPI_ERROR_LOG ORTE_ERROR_LOG
#  define OMPI_NAME_PRINT ORTE_NAME_PRINT

#  define USNIC_OUT mca_btl_base_output
#  define proc_bound() ompi_btl_usnic_proc_bound_v1_6_helper()

/* the opal_if code lives inside the BTL dir in v1.6-cisco */
#  define opal_ifbegin       btl_usnic_opal_ifbegin
#  define opal_ifnext        btl_usnic_opal_ifnext
#  define opal_ifindextoaddr btl_usnic_opal_ifindextoaddr
#  define opal_ifindextomac  btl_usnic_opal_ifindextomac
#  define opal_ifindextomask btl_usnic_opal_ifindextomask
#  define opal_ifindextomtu  btl_usnic_opal_ifindextomtu
#  define opal_ifindextoname btl_usnic_opal_ifindextoname

/* this _FOREACH macro is not present in v1.6 */
#define OPAL_LIST_FOREACH(item, list, type)                             \
  for (item = (type *) (list)->opal_list_sentinel.opal_list_next ;      \
       item != (type *) &(list)->opal_list_sentinel ;                   \
       item = (type *) ((opal_list_item_t *) (item))->opal_list_next)

#elif (OMPI_MAJOR_VERSION == 1 && OMPI_MINOR_VERSION >= 7) || \
      (OMPI_MAJOR_VERSION >= 2)

/* the v1.7+ equivalent way to get OMPI_ERROR_LOG and friends */
#  include "ompi/mca/rte/rte.h"

/* v1.7, v1.8 (to be released), trunk (v1.9), or later */
/* TODO validate that all of these things actually work with v1.7 */
#  define USNIC_OUT ompi_btl_base_framework.framework_output
#  define proc_bound() (ompi_rte_proc_is_bound)

#else
#  error OMPI version too old (< 1.6)
#endif

/* The FREE_LIST_*_MT stuff was introduced on the SVN trunk in r28722
   (2013-07-04), but so far, has not been merged into the v1.7 branch
   yet (2013-09-06). */
#ifndef OMPI_FREE_LIST_GET_MT
#  define OMPI_FREE_LIST_GET_MT(list_, item_) \
    do { \
        int rc_ __opal_attribute_unused__; \
        OMPI_FREE_LIST_GET(list_, item_, rc_); \
    } while (0)
#  define OMPI_FREE_LIST_RETURN_MT(list_, item_) \
        OMPI_FREE_LIST_RETURN(list_, item_)
#endif

#endif /* BTL_USNIC_COMPAT_H */
