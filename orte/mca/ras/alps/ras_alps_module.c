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
 * Copyright (c) 2008      UT-Battelle, LLC
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "orte_config.h"
#include "orte/orte_constants.h"
#include "orte/orte_types.h"

#include <unistd.h>
#include <string.h>
#include <ctype.h>

#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "orte/dss/dss.h"
#include "orte/mca/rmgr/rmgr.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/ras/base/ras_private.h"
#include "ras_alps.h"


/*
 * Local functions
 */
static int orte_ras_alps_allocate(orte_jobid_t jobid, opal_list_t *attributes);
static int orte_ras_alps_deallocate(orte_jobid_t jobid);
static int orte_ras_alps_finalize(void);




/*
 * Global variable
 */
orte_ras_base_module_t orte_ras_alps_module = {
    orte_ras_alps_allocate,
    orte_ras_base_node_insert,
    orte_ras_base_node_query,
    orte_ras_base_node_query_alloc,
    orte_ras_base_node_lookup,
    orte_ras_base_proc_query_alloc,
    orte_ras_alps_deallocate,
    orte_ras_alps_finalize
};

/**
 * Discover available (pre-allocated) nodes.  Allocate the
 * requested number of nodes/process slots to the job.
 *  
 */
static int orte_ras_alps_allocate(orte_jobid_t jobid, opal_list_t *attributes)
{
    int ret;
    char *alps_batch_id;
    opal_list_t nodes;
    opal_list_item_t* item;
    
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

    OBJ_CONSTRUCT(&nodes, opal_list_t);

    if (ORTE_SUCCESS != (ret = orte_ras_base_read_nodename_file(&nodes, "./ompi_ras_alps_node_file"))) {
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }

    ret = orte_ras_base_allocate_nodes(jobid, &nodes);

    ret = orte_ras_base_node_insert(&nodes);

cleanup:
    while (NULL != (item = opal_list_remove_first(&nodes))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&nodes);

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

/*
 * There's really nothing to do here
 */
static int orte_ras_alps_deallocate(orte_jobid_t jobid)
{
    opal_output(orte_ras_base.ras_output, 
                "ras:alps:deallocate: success (nothing to do)");
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

