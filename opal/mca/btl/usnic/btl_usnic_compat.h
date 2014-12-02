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

/* PMIX / modex stuff */
#  include "opal/mca/pmix/pmix.h"

/* Proc stuff */
#  include "opal/util/proc.h"

#  define USNIC_OUT opal_btl_base_framework.framework_output
/* JMS Really want to be able to get the job size somehow...  But for
   now, so that we can compile, just set it to a constant :-( */
#  define USNIC_MCW_SIZE 2
#if OPAL_HAVE_HWLOC
#    define proc_bound() (NULL != opal_process_info.cpuset ? 1 : 0)
#else
#    define proc_bound() 0
#endif
#  define USNIC_BTL_DEFAULT_VERSION(name) MCA_BTL_DEFAULT_VERSION(name)

#  define USNIC_SEND_LOCAL        des_local
#  define USNIC_SEND_LOCAL_COUNT  des_local_count
#  define USNIC_SEND_REMOTE       des_remote
#  define USNIC_SEND_REMOTE_COUNT des_remote_count

#  define USNIC_RECV_LOCAL        des_local
#  define USNIC_RECV_LOCAL_COUNT  des_local_count
#  define USNIC_RECV_REMOTE       des_remote
#  define USNIC_RECV_REMOTE_COUNT des_remote_count

#  define USNIC_PUT_LOCAL         des_local
#  define USNIC_PUT_LOCAL_COUNT   des_local_count
#  define USNIC_PUT_REMOTE        des_remote
#  define USNIC_PUT_REMOTE_COUNT  des_remote_count

/*
 * Performance critical; needs to be inline
 */
static inline int
usnic_compat_proc_name_compare(opal_process_name_t a,
                               opal_process_name_t b)
{
    return (bool) (a.jobid == b.jobid && a.vpid == b.vpid);
}

/************************************************************************/

/* v1.7 and v1.8 */

#elif (OPAL_MAJOR_VERSION == 1 && OPAL_MINOR_VERSION >= 7)

/* OMPI_ERROR_LOG and friends */
#  include "ompi/mca/rte/rte.h"

/* Proc stuff */
#  include "ompi/proc/proc.h"

#  define USNIC_OUT ompi_btl_base_framework.framework_output
#  define USNIC_MCW_SIZE ompi_process_info.num_procs
#  define proc_bound() (ompi_rte_proc_is_bound)
#  define opal_proc_local_get() ompi_proc_local()

#  define opal_process_info orte_process_info

#  define opal_proc_t ompi_proc_t
#  define opal_process_name_t ompi_process_name_t

#  define opal_btl_usnic_modex_t ompi_btl_usnic_modex_t
#  define opal_btl_usnic_component_t ompi_btl_usnic_component_t
#  define opal_btl_usnic_module_t ompi_btl_usnic_module_t
#  define opal_btl_usnic_endpoint_t ompi_btl_usnic_endpoint_t
#  define opal_btl_usnic_endpoint_t_class ompi_btl_usnic_endpoint_t_class
#  define opal_btl_usnic_frag_t ompi_btl_usnic_frag_t
#  define opal_btl_usnic_frag_t_class ompi_btl_usnic_frag_t_class
#  define opal_btl_usnic_send_frag_t ompi_btl_usnic_send_frag_t
#  define opal_btl_usnic_send_frag_t_class ompi_btl_usnic_send_frag_t_class
#  define opal_btl_usnic_large_send_frag_t ompi_btl_usnic_large_send_frag_t
#  define opal_btl_usnic_large_send_frag_t_class ompi_btl_usnic_large_send_frag_t_class
#  define opal_btl_usnic_small_send_frag_t ompi_btl_usnic_small_send_frag_t
#  define opal_btl_usnic_small_send_frag_t_class ompi_btl_usnic_small_send_frag_t_class
#  define opal_btl_usnic_put_dest_frag_t ompi_btl_usnic_put_dest_frag_t
#  define opal_btl_usnic_put_dest_frag_t_class ompi_btl_usnic_put_dest_frag_t_class
#  define opal_btl_usnic_rx_buf_t ompi_btl_usnic_rx_buf_t
#  define opal_btl_usnic_rx_buf_t_class ompi_btl_usnic_rx_buf_t_class
#  define opal_btl_usnic_segment_t ompi_btl_usnic_segment_t
#  define opal_btl_usnic_segment_t_class ompi_btl_usnic_segment_t_class
#  define opal_btl_usnic_frag_segment_t ompi_btl_usnic_frag_segment_t
#  define opal_btl_usnic_frag_segment_t_class ompi_btl_usnic_frag_segment_t_class
#  define opal_btl_usnic_chunk_segment_t ompi_btl_usnic_chunk_segment_t
#  define opal_btl_usnic_chunk_segment_t_class ompi_btl_usnic_chunk_segment_t_class
#  define opal_btl_usnic_recv_segment_t ompi_btl_usnic_recv_segment_t
#  define opal_btl_usnic_recv_segment_t_class ompi_btl_usnic_recv_segment_t_class
#  define opal_btl_usnic_ack_segment_t ompi_btl_usnic_ack_segment_t
#  define opal_btl_usnic_ack_segment_t_class ompi_btl_usnic_ack_segment_t_class

#  define opal_btl_usnic_graph_t ompi_btl_usnic_graph_t

#  define opal_btl_usnic_run_tests ompi_btl_usnic_run_tests

#  define USNIC_SEND_LOCAL        des_src
#  define USNIC_SEND_LOCAL_COUNT  des_src_cnt
#  define USNIC_SEND_REMOTE       des_dst
#  define USNIC_SEND_REMOTE_COUNT des_dst_cnt

#  define USNIC_RECV_LOCAL        des_dst
#  define USNIC_RECV_LOCAL_COUNT  des_dst_cnt
#  define USNIC_RECV_REMOTE       des_src
#  define USNIC_RECV_REMOTE_COUNT des_src_cnt

#  define USNIC_PUT_LOCAL         des_dst
#  define USNIC_PUT_LOCAL_COUNT   des_dst_cnt
#  define USNIC_PUT_REMOTE        des_dst
#  define USNIC_PUT_REMOTE_COUNT  des_dst_cnt

#  define USNIC_COMPAT_BASE_VERSION                                 \
    MCA_BTL_BASE_VERSION_2_0_0,                                     \
        .mca_type_name = "btl",                                     \
        .mca_type_major_version = OMPI_MAJOR_VERSION,               \
        .mca_type_minor_version = OMPI_MINOR_VERSION,               \
        .mca_type_release_version = OMPI_RELEASE_VERSION

#  define USNIC_BTL_DEFAULT_VERSION(name)                       \
    USNIC_COMPAT_BASE_VERSION,                                  \
        .mca_component_name = name,                             \
        .mca_component_major_version = OPAL_MAJOR_VERSION,      \
        .mca_component_minor_version = OPAL_MINOR_VERSION,      \
        .mca_component_release_version = OPAL_RELEASE_VERSION

#define OPAL_BTL_USNIC_UNIT_TESTS OMPI_BTL_USNIC_UNIT_TESTS

/*
 * Performance critical; needs to be inline
 */
static inline int
usnic_compat_proc_name_compare(opal_process_name_t a,
                               opal_process_name_t b)
{
    return ompi_rte_compare_name_fields(OMPI_RTE_CMP_ALL, &a, &b);
}

/*
 * Replicate functions that exist on master
 */
char* opal_get_proc_hostname(opal_proc_t *proc);

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

/************************************************************************
 * Common to all versions
 ************************************************************************/

/* Forward declare to avoid #include ordering complications */
struct opal_btl_usnic_modex_t;

void usnic_compat_modex_send(int *rc,
                             mca_base_component_t *component,
                             struct opal_btl_usnic_modex_t *modexes,
                             size_t size);

void usnic_compat_modex_recv(int *rc,
                             mca_base_component_t *component,
                             opal_proc_t *proc,
                             struct opal_btl_usnic_modex_t **modexes,
                             size_t *size);

uint64_t usnic_compat_rte_hash_name(opal_process_name_t *pname);
const char *usnic_compat_proc_name_print(opal_process_name_t *pname);

#endif /* BTL_USNIC_COMPAT_H */
