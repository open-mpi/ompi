/*
 * $HEADER$
 */
/**
 * @file
 */
#ifndef _MCA_SVC_SCHED_NODE_
#define _MCA_SVC_SCHED_NODE_

#include "mca/svc/svc.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
struct mca_svc_sched_node_t {
    ompi_list_item_t node_item;
    ompi_process_name_t node_name;
    char* node_hostname;
    char* node_contactinfo;
    int32_t node_proc_slots;
    int32_t node_proc_avail;
};
typedef struct mca_svc_sched_node_t mca_svc_sched_node_t;

OBJ_CLASS_DECLARATION(mca_svc_sched_node_t);

/**
 * Setup node attributes
 */

void mca_svc_sched_node_set(
    mca_svc_sched_node_t* node, 
    const char* hostname, 
    const char* contactinfo,
    int32_t proc_slots);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

