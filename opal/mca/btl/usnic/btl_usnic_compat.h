/*
 * Copyright (c) 2013-2014 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/* This header contains macros to help minimize usnic BTL differences
 * between v1.7/v1.8 and v1.9/v2.0. */

#ifndef BTL_USNIC_COMPAT_H
#define BTL_USNIC_COMPAT_H

/************************************************************************/

/* v1.9 and beyond */

#if (OPAL_MAJOR_VERSION == 1 && OPAL_MINOR_VERSION >= 9) || \
    (OPAL_MAJOR_VERSION >= 2)

/* OMPI_ERROR_LOG and friends */
#  include "opal/util/error.h"

#  define USNIC_OUT opal_btl_base_framework.framework_output
/* JMS Really want to be able to get the job size somehow...  But for
   now, so that we can compile, just set it to a constant :-( */
#  define USNIC_MCW_SIZE 16
#if OPAL_HAVE_HWLOC
#    define proc_bound() (NULL != opal_process_info.cpuset ? 1 : 0)
#else
#    define proc_bound() 0
#endif

/************************************************************************/

/* v1.7 and v1.8 */

#elif (OPAL_MAJOR_VERSION == 1 && OPAL_MINOR_VERSION >= 7)

/* OMPI_ERROR_LOG and friends */
#  include "ompi/mca/rte/rte.h"

#  define USNIC_OUT ompi_btl_base_framework.framework_output
#  define USNIC_MCW_SIZE ompi_process_info.num_procs
#  define proc_bound() (ompi_rte_proc_is_bound)

/************************************************************************/

#else
#  error OMPI version too old (< 1.7)
#endif

/************************************************************************/

/* The FREE_LIST_*_MT stuff was introduced on the SVN trunk in r28722
   (2013-07-04), but so far, has not been merged into the v1.7 branch
   yet (2013-09-06). */
#ifndef OPAL_FREE_LIST_GET_MT
#  define OPAL_FREE_LIST_GET_MT(list_, item_) \
    do { \
        int rc_ __opal_attribute_unused__; \
        OPAL_FREE_LIST_GET(list_, item_, rc_); \
    } while (0)
#  define OPAL_FREE_LIST_RETURN_MT(list_, item_) \
        OPAL_FREE_LIST_RETURN(list_, item_)
#endif

#endif /* BTL_USNIC_COMPAT_H */
