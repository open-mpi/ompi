/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "orte_config.h"

#include <errno.h>
#include <unistd.h>
#include <string.h>

#include "tm.h"

#include "include/orte_constants.h"
#include "include/orte_types.h"
#include "util/argv.h"
#include "util/output.h"
#include "mca/ras/base/base.h"
#include "mca/ras/base/ras_base_node.h"
#include "ras_tm.h"


/*
 * Local functions
 */
static int allocate(orte_jobid_t jobid);
static int deallocate(orte_jobid_t jobid);
static int finalize(void);

static int discover(ompi_list_t* nodelist);
static int get_tm_hostname(tm_node_id node, char **hostname, char **arch);


/*
 * Global variable
 */
orte_ras_base_module_t orte_ras_tm_module = {
    allocate,
    deallocate,
    finalize
};


/**
 * Discover available (pre-allocated) nodes.  Allocate the
 * requested number of nodes/process slots to the job.
 *  
 */
#include "mca/gpr/gpr.h"
static int allocate(orte_jobid_t jobid)
{
    int ret;
    ompi_list_t nodes;
    ompi_list_item_t* item;
    struct tm_roots root;

    /* Open up our connection to tm */

    ret = tm_init(NULL, &root);
    if (TM_SUCCESS != ret) {
        /* JMS May change...? */
        ompi_output(orte_ras_base.ras_output, 
                    "ras:tm:allocate: tm_init failed!");
        return ORTE_ERR_RESOURCE_BUSY;
    }

    OBJ_CONSTRUCT(&nodes, ompi_list_t);
    if (ORTE_SUCCESS != (ret = discover(&nodes))) {
        /* JMS May change...? */
        ompi_output(orte_ras_base.ras_output,
                    "ras:tm:allocate: discover failed!");
        tm_finalize();
        return ret;
    }
    ret = orte_ras_base_allocate_nodes(jobid, &nodes);

    while (NULL != (item = ompi_list_remove_first(&nodes))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&nodes);

    /* All done */

    if (ORTE_SUCCESS == ret) {
        ompi_output(orte_ras_base.ras_output, 
                    "ras:tm:allocate: success");
    } else {
        ompi_output(orte_ras_base.ras_output, 
                    "ras:tm:allocate: failure (base_allocate_nodes=%d)", ret);
    }
    tm_finalize();
    return ret;
}


/*
 * There's really nothing to do here
 */
static int deallocate(orte_jobid_t jobid)
{
    ompi_output(orte_ras_base.ras_output, 
                "ras:tm:deallocate: success (nothing to do)");
    return ORTE_SUCCESS;
}


/*
 * There's really nothing to do here
 */
static int finalize(void)
{
    ompi_output(orte_ras_base.ras_output, 
                "ras:tm:finalize: success (nothing to do)");
    return ORTE_SUCCESS;
}


/**
 * Discover the available resources.  Obtain directly from TM (and
 * therefore have no need to validate) -- ignore hostfile or any other
 * user-specified parameters.
 *
 *  - validate any nodes specified via hostfile/commandline
 *  - check for additional nodes that have already been allocated
 */

static int discover(ompi_list_t* nodelist)
{
    int i, ret, num_node_ids;
    orte_ras_base_node_t *node;
    ompi_list_item_t* item;
    ompi_list_t new_nodes;
    tm_node_id *tm_node_ids;
    char *hostname, *arch;

    /* Ignore anything that the user already specified -- we're
       getting nodes only from TM. */

    /* TM "nodes" may actually correspond to PBS "VCPUs", which means
       there may be multiple "TM nodes" that correspond to the same
       physical node.  This doesn't really affect what we're doing
       here (we actually ignore the fact that they're duplicates --
       slightly inefficient, but no big deal); just mentioned for
       completeness... */

    ret = tm_nodeinfo(&tm_node_ids, &num_node_ids);
    if (ret != TM_SUCCESS) {
        ompi_output(orte_ras_base.ras_output, 
                    "ras:tm:allocate:discover: tm_nodeinfo failed");
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /* Iterate through all the nodes and make an entry for each */

    OBJ_CONSTRUCT(&new_nodes, ompi_list_t);
    for (i = 0; i < num_node_ids; ++i) {
        get_tm_hostname(tm_node_ids[i], &hostname, &arch);
        ompi_output(orte_ras_base.ras_output, 
                    "ras:tm:allocate:discover: got hostname %s", hostname);

        /* Remember that TM may list the same node more than once.  So
           we have to check for duplicates. */

        for (item = ompi_list_get_first(&new_nodes);
             ompi_list_get_end(&new_nodes) != item;
             item = ompi_list_get_next(item)) {
            node = (orte_ras_base_node_t*) item;
            if (0 == strcmp(node->node_name, hostname)) {
                ++node->node_slots_max;
                ++node->node_slots;
                ompi_output(orte_ras_base.ras_output, 
                            "ras:tm:allocate:discover: found -- bumped slots to %d",
                            node->node_slots);
                break;
            }
        }

        /* Did we find it? */

        if (ompi_list_get_end(&new_nodes) == item) {

            /* Nope -- didn't find it, so add a new item to the list */

            ompi_output(orte_ras_base.ras_output, 
                        "ras:tm:allocate:discover: not found -- added to list");
            node = OBJ_NEW(orte_ras_base_node_t);
            node->node_name = hostname;
            node->node_arch = arch;
            node->node_state = ORTE_NODE_STATE_UP;
            node->node_cellid = 0;
            node->node_slots_inuse = 0;
            node->node_slots_max = 1;
            node->node_slots = 1;
            ompi_list_append(&new_nodes, &node->super);
        } else {

            /* Yes, so we need to free the hostname that came back
               from get_tm_hostname() */

            free(hostname);
        }
    }

    /* Add these nodes to the registry, and return all the values */

    ompi_output(orte_ras_base.ras_output, 
                "ras:tm:allocate:discover: done -- adding to registry");
    ret = orte_ras_base_node_insert(&new_nodes);
    for (item = ompi_list_remove_first(&new_nodes);
         NULL != item; item = ompi_list_remove_first(&new_nodes)) {
        if (ORTE_SUCCESS == ret) {
            ompi_list_append(nodelist, item);
        } else {
            OBJ_RELEASE(item);
        }
    }

    /* All done */

    if (ORTE_SUCCESS == ret) {
        ompi_output(orte_ras_base.ras_output, 
                    "ras:tm:allocate:discover: success");
    } else {
        ompi_output(orte_ras_base.ras_output, 
                    "ras:tm:allocate:discover: failed (rc=%d)", ret);
    }
    OBJ_DESTRUCT(&new_nodes);
    return ret;
}


/*
 * For a given TM node ID, get the string hostname corresponding to
 * it.
 */
static int get_tm_hostname(tm_node_id node, char **hostname, char **arch)
{
    int ret, local_errno;
    tm_event_t event;
    char buffer[256];
    char **argv;

    /* Get the info string corresponding to this TM node ID */

    ret = tm_rescinfo(node, buffer, sizeof(buffer) - 1, &event);
    if (TM_SUCCESS != ret) {
        ompi_output(orte_ras_base.ras_output, 
                    "ras:tm:hostname: tm_rescinfo failed");
        return ORTE_ERROR;
    }

    /* Now wait for that event to happen */

    ret = tm_poll(TM_NULL_EVENT, &event, 1, &local_errno);
    if (TM_SUCCESS != ret) {
        return ORTE_ERROR;
    }

    /* According to the TM man page, we get back a space-separated
       string array.  The hostname is the second item.  Use a cheap
       trick to get it. */

    ompi_output(orte_ras_base.ras_output, 
                "ras:tm:hostname: got back %s", buffer);
    buffer[sizeof(buffer) - 1] = '\0';
    argv = ompi_argv_split(buffer, ' ');
    if (NULL == argv) {
        return ORTE_ERROR;
    }
    *hostname = strdup(argv[1]);
    *arch = strdup(buffer);
    ompi_argv_free(argv);

    /* All done */

    ompi_output(orte_ras_base.ras_output, 
                "ras:tm:hostname: got hostname %s", hostname);
    return ORTE_SUCCESS;
}
