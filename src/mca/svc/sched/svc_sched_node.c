/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <string.h>

#include "svc_sched_node.h"


static void mca_svc_sched_node_construct(mca_svc_sched_node_t* node)
{
    node->node_hostname = NULL;
    node->node_contactinfo = NULL;
    node->node_proc_slots = 0;
    node->node_proc_avail = 0;
}

static void mca_svc_sched_node_destruct(mca_svc_sched_node_t* node)
{
    if(node->node_hostname != NULL)
        free(node->node_hostname);
    if(node->node_contactinfo != NULL)
        free(node->node_contactinfo);
}

OBJ_CLASS_INSTANCE(
    mca_svc_sched_node_t,
    ompi_list_item_t,
    mca_svc_sched_node_construct,
    mca_svc_sched_node_destruct);

/**
 * Setup node attributes
 */
                                                                                                                    
void mca_svc_sched_node_set(
    mca_svc_sched_node_t* node,
    const char* hostname,
    const char* contactinfo,
    int32_t proc_slots)
{
    if(node->node_hostname != NULL)
        free(node->node_hostname);
    node->node_hostname = strdup(hostname);
    if(node->node_contactinfo != NULL)
        free(node->node_contactinfo);
    node->node_contactinfo = strdup(contactinfo);
    node->node_proc_slots = proc_slots;
}

