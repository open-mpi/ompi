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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "orte_config.h"
#include "orte/orte_constants.h"
#include "orte/orte_types.h"

#include <errno.h>
#include <unistd.h>
#include <string.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */

#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/os_path.h"

#include "orte/dss/dss.h"
#include "orte/mca/rmgr/rmgr.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/ras/base/ras_private.h"
#include "ras_tm.h"


/*
 * Local functions
 */
static int allocate(orte_jobid_t jobid, opal_list_t *attributes);
static int deallocate(orte_jobid_t jobid);
static int finalize(void);

static int discover(opal_list_t* nodelist, char *pbs_jobid);
static char *tm_getline(FILE *fp);

#define TM_FILE_MAX_LINE_LENGTH 512

/*
 * Global variable
 */
orte_ras_base_module_t orte_ras_tm_module = {
    allocate,
    orte_ras_base_node_insert,
    orte_ras_base_node_query,
    orte_ras_base_node_query_alloc,
    orte_ras_base_node_lookup,
    deallocate,
    finalize
};


/**
 * Discover available (pre-allocated) nodes.  Allocate the
 * requested number of nodes/process slots to the job.
 *  
 */
static int allocate(orte_jobid_t jobid, opal_list_t *attributes)
{
    int ret;
    opal_list_t nodes;
    opal_list_item_t* item;
    char *pbs_jobid;

    /* get our PBS jobid from the environment */
    if (NULL == (pbs_jobid = getenv("PBS_JOBID"))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    
    OBJ_CONSTRUCT(&nodes, opal_list_t);
    if (ORTE_SUCCESS != (ret = discover(&nodes, pbs_jobid))) {
        opal_output(orte_ras_base.ras_output,
                    "ras:tm:allocate: discover failed!");
        return ret;
    }
    ret = orte_ras_base_allocate_nodes(jobid, &nodes);

    while (NULL != (item = opal_list_remove_first(&nodes))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&nodes);

    /* All done */

    if (ORTE_SUCCESS == ret) {
        opal_output(orte_ras_base.ras_output, 
                    "ras:tm:allocate: success");
    } else {
        opal_output(orte_ras_base.ras_output, 
                    "ras:tm:allocate: failure (base_allocate_nodes=%d)", ret);
    }
    return ret;
}

/*
 * There's really nothing to do here
 */
static int deallocate(orte_jobid_t jobid)
{
    opal_output(orte_ras_base.ras_output, 
                "ras:tm:deallocate: success (nothing to do)");
    return ORTE_SUCCESS;
}


/*
 * There's really nothing to do here
 */
static int finalize(void)
{
    opal_output(orte_ras_base.ras_output, 
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

static int discover(opal_list_t* nodelist, char *pbs_jobid)
{
    int ret;
    int32_t nodeid;
    orte_ras_node_t *node;
    opal_list_item_t* item;
    opal_list_t new_nodes;
    FILE *fp;
    char *hostname, *filename;
    struct timeval start, stop;

    /* check for timing request - get start time if so */
    if (orte_ras_base.timing) {
        gettimeofday(&start, NULL);
    }
    
    /* Ignore anything that the user already specified -- we're
       getting nodes only from TM. */

    /* TM "nodes" may actually correspond to PBS "VCPUs", which means
       there may be multiple "TM nodes" that correspond to the same
       physical node.  This doesn't really affect what we're doing
       here (we actually ignore the fact that they're duplicates --
       slightly inefficient, but no big deal); just mentioned for
       completeness... */

    /* setup the full path to the PBS file */
    filename = opal_os_path(false, mca_ras_tm_component.nodefile_dir,
                            pbs_jobid, NULL);
    fp = fopen(filename, "r");
    if (NULL == fp) {
        ORTE_ERROR_LOG(ORTE_ERR_FILE_OPEN_FAILURE);
        free(filename);
        return ORTE_ERR_FILE_OPEN_FAILURE;
    }

    /* Iterate through all the nodes and make an entry for each.  TM
       node ID's will never be duplicated, but they may end up
       resolving to the same hostname (i.e., vcpu's on a single
       host). */

    OBJ_CONSTRUCT(&new_nodes, opal_list_t);
    nodeid=0;
    while (NULL != (hostname = tm_getline(fp))) {
        opal_output(orte_ras_base.ras_output, 
                    "ras:tm:allocate:discover: got hostname %s", hostname);

        /* Remember that TM may list the same node more than once.  So
           we have to check for duplicates. */

        for (item = opal_list_get_first(&new_nodes);
             opal_list_get_end(&new_nodes) != item;
             item = opal_list_get_next(item)) {
            node = (orte_ras_node_t*) item;
            if (0 == strcmp(node->node_name, hostname)) {
                ++node->node_slots;
                opal_output(orte_ras_base.ras_output, 
                            "ras:tm:allocate:discover: found -- bumped slots to %d",
                            node->node_slots);
                break;
            }
        }

        /* Did we find it? */

        if (opal_list_get_end(&new_nodes) == item) {

            /* Nope -- didn't find it, so add a new item to the list */

            opal_output(orte_ras_base.ras_output, 
                        "ras:tm:allocate:discover: not found -- added to list");
            node = OBJ_NEW(orte_ras_node_t);
            node->node_name = hostname;
            node->launch_id = nodeid;
            node->node_cellid = 0;
            node->node_slots_inuse = 0;
            node->node_slots_max = 0;
            node->node_slots = 1;
            opal_list_append(&new_nodes, &node->super);
        } else {

            /* Yes, so we need to free the hostname that came back */
            free(hostname);
        }

        /* up the nodeid */
        nodeid++;
    }

    /* Add these nodes to the registry */

    opal_output(orte_ras_base.ras_output, 
                "ras:tm:allocate:discover: done -- adding to registry");
    ret = orte_ras_base_node_insert(&new_nodes);
    for (item = opal_list_remove_first(&new_nodes);
         NULL != item; item = opal_list_remove_first(&new_nodes)) {
        if (ORTE_SUCCESS == ret) {
            opal_list_append(nodelist, item);
        } else {
            OBJ_RELEASE(item);
        }
    }

    /* All done */
    if (ORTE_SUCCESS == ret) {
        opal_output(orte_ras_base.ras_output, 
                    "ras:tm:allocate:discover: success");
    } else {
        opal_output(orte_ras_base.ras_output, 
                    "ras:tm:allocate:discover: failed (rc=%d)", ret);
    }
    OBJ_DESTRUCT(&new_nodes);

    /* check for timing request - get stop time and report elapsed time if so */
    if (orte_ras_base.timing) {
        gettimeofday(&stop, NULL);
        opal_output(0, "ras_tm: time to allocate is %ld usec",
                    (long int)((stop.tv_sec - start.tv_sec)*1000000 +
                               (stop.tv_usec - start.tv_usec)));
        gettimeofday(&start, NULL);
    }
    
    return ret;
}

static char *tm_getline(FILE *fp)
{
    char *ret, *buff;
    char input[TM_FILE_MAX_LINE_LENGTH];
    
    ret = fgets(input, TM_FILE_MAX_LINE_LENGTH, fp);
    if (NULL != ret) {
        input[strlen(input)-1] = '\0';  /* remove newline */
        buff = strdup(input);
        return buff;
    }
    
    return NULL;
}

