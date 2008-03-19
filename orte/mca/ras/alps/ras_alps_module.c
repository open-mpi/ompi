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

#include "opal/mca/installdirs/installdirs.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/os_path.h"
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
static char *ras_alps_getline(FILE *fp);



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
    FILE *fp;
    char *alps_node_cmd_str;
    char *str;
    char *node_file; 
    
    alps_batch_id = getenv("BATCH_PARTITION_ID");
    if (NULL == alps_batch_id) {
        opal_show_help("help-ras-alps.txt", "alps-env-var-not-found", 1,
                       "BATCH_PARTITION_ID");
        return ORTE_ERR_NOT_FOUND;
    }

    node_file = opal_os_path(false, orte_process_info.job_session_dir,
                             "orte_ras_alps_node_file.txt", NULL); 
    
    OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                         "ras:alps:allocate: node_file in %s", node_file));
    
    asprintf(&str, "%s/ras-alps-command.sh",
             opal_install_dirs.pkgdatadir
             );
    if (NULL == str) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    fp = fopen(str, "r");
    if (NULL == fp) {
        ORTE_ERROR_LOG(ORTE_ERR_FILE_OPEN_FAILURE);
        return ORTE_ERR_FILE_OPEN_FAILURE;
    }
    
    asprintf(&alps_node_cmd_str, "%s > %s", 
             ras_alps_getline(fp), 
             node_file
             );

    OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                         "ras:alps:allocate: got command string %s", alps_node_cmd_str));
    

    if(system(alps_node_cmd_str)) { 
        opal_output(0, "Error in orte_ras_alps_allocate: system call returned an error, for reference I tried to run: %s", 
                    alps_node_cmd_str);
        return ORTE_ERROR;
    }
    
    if (ORTE_SUCCESS != (ret = orte_ras_alps_read_nodename_file(nodes, node_file))) {
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
        OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                             "ras:alps:allocate: success"));
    } else {
        OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                             "ras:alps:allocate: failure (base_allocate_nodes=%d)", ret));
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
        OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                             "ras:alps:read_nodename: got hostname %s", hostname));
        
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
        OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                             "ras:alps:read_nodename: not found -- added to list"));
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
    OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                         "ras:alps:finalize: success (nothing to do)"));
    return ORTE_SUCCESS;
}

