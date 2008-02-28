/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      UT-Battelle, LLC. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include <unistd.h>
#include <string.h>
#include <ctype.h>

#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"

#include "ras_alps.h"
#include "orte/mca/ras/base/ras_private.h"


/*
 * Local functions
 */
static int orte_ras_alps_allocate(opal_list_t *nodes);
static int orte_ras_alps_finalize(void);
int orte_ras_alps_read_nodename_file(opal_list_t *nodes, char *filename);




/*
 * Global variable
 */
orte_ras_base_module_t orte_ras_alps_module = {
    orte_ras_alps_allocate,
    orte_ras_alps_finalize
};

/**
 * Discover available (pre-allocated) nodes.  Allocate the
 * requested number of nodes/process slots to the job.
 *  
 */
static int orte_ras_alps_allocate(opal_list_t *nodes)
{
    int ret;
    char *alps_batch_id;
    
    char *alps_node_cmd_str = "apstat -a `apstat -r | grep $BATCH_PARTITION_ID  | awk '{print $2}'` " 
        " -r   -v | egrep  \"(nid [0-9]+)\" -o | awk '{print $2}' > ./ompi_ras_alps_node_file";
            
    
    alps_batch_id = getenv("BATCH_PARTITION_ID");
    if (NULL == alps_batch_id) {
        opal_show_help("help-ras-alps.txt", "alps-env-var-not-found", 1,
                       "BATCH_PARTITION_ID");
        return ORTE_ERR_NOT_FOUND;
    }

    if(system(alps_node_cmd_str)) { 
        opal_output(0, "Error in orte_ras_alps_allocate: system call returned an error, for reference I tried to run: %s", 
                    alps_node_cmd_str);
        return ORTE_ERROR;
    }
    
    if (ORTE_SUCCESS != (ret = orte_ras_alps_read_nodename_file(nodes, "./ompi_ras_alps_node_file"))) {
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }

#if 0
    ret = orte_ras_alps_allocate_nodes(jobid, &nodes);

    ret = orte_ras_alps_node_insert(&nodes);
#endif

cleanup:
#if 0
    while (NULL != (item = opal_list_remove_first(&nodes))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&nodes);
#endif 

    /* All done */

    if (ORTE_SUCCESS == ret) {
        opal_output(orte_ras_base.ras_output, 
                    "ras:alps:allocate: success");
    } else {
        opal_output(orte_ras_base.ras_output, 
                    "ras:alps:allocate: failure (base_allocate_nodes=%d)", ret);
    }
    return ret;
}


#define RAS_BASE_FILE_MAX_LINE_LENGTH   512

static char *ras_alps_getline(FILE *fp)
{
    char *ret, *buff = NULL;
    char input[RAS_BASE_FILE_MAX_LINE_LENGTH];
    
    ret = fgets(input, RAS_BASE_FILE_MAX_LINE_LENGTH, fp);
    if (NULL != ret) {
        input[strlen(input)-1] = '\0';  /* remove newline */
        buff = strdup(input);
    }
    
    return buff;
}

int orte_ras_alps_read_nodename_file(opal_list_t *nodes, char *filename)
{
    FILE *fp;
    int32_t nodeid=0;
    char *hostname;
    orte_node_t* node = NULL;
    fp = fopen(filename, "r");
    if (NULL == fp) {
        ORTE_ERROR_LOG(ORTE_ERR_FILE_OPEN_FAILURE);
        return ORTE_ERR_FILE_OPEN_FAILURE;
    }
    
    while (NULL != (hostname = ras_alps_getline(fp))) {
        opal_output(orte_ras_base.ras_output, 
                    "ras:base:read_nodename: got hostname %s", hostname);
        
        /* if this matches the prior nodename, then just add
         * to the slot count
         */
        if (NULL != node &&
            0 == strcmp(node->name, hostname)) {
            ++node->slots;
            /* free the hostname that came back since we don't need it */
            free(hostname);
            continue;
        }
        
        /* must be a new name, so add a new item to the list */
        opal_output(orte_ras_base.ras_output, 
                    "ras:base:read_nodename: not found -- added to list");
        node = OBJ_NEW(orte_node_t);
        node->name = hostname;
        node->launch_id = nodeid;
        node->slots_inuse = 0;
        node->slots_max = 0;
        node->slots = 1;
        opal_list_append(nodes, &node->super);
        /* up the nodeid */
        nodeid++;
    }
    fclose(fp);
    
    return ORTE_SUCCESS;
}

/*
 * There's really nothing to do here
 */
static int orte_ras_alps_finalize(void)
{
    opal_output(orte_ras_base.ras_output, 
                "ras:alps:finalize: success (nothing to do)");
    return ORTE_SUCCESS;
}

