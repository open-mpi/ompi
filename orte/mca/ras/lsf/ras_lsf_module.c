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
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "orte_config.h"
#include "orte/constants.h"

#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>

#define SR1_PJOBS
#include <lsf/lsbatch.h>

#include "opal/util/argv.h"
#include "opal/mca/hwloc/hwloc.h"

#include "orte/mca/rmaps/rmaps_types.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/show_help.h"

#include "orte/mca/ras/base/ras_private.h"
#include "ras_lsf.h"


/*
 * Local functions
 */
static int allocate(orte_job_t *jdata, opal_list_t *nodes);
static int finalize(void);


/*
 * Global variable
 */
orte_ras_base_module_t orte_ras_lsf_module = {
    NULL,
    allocate,
    NULL,
    finalize
};

static char *orte_getline(FILE *fp)
{
    char *ret, *buff;
    char input[1024];

    ret = fgets(input, 1024, fp);
    if (NULL != ret) {
	   input[strlen(input)-1] = '\0';  /* remove newline */
	   buff = strdup(input);
	   return buff;
    }
    
    return NULL;
}


static int allocate(orte_job_t *jdata, opal_list_t *nodes)
{
    char **nodelist;
    orte_node_t *node;
    int i, num_nodes;
    char *affinity_file, *hstname;
    bool found;
    struct stat buf;
    orte_app_context_t *app;
    
    /* get the list of allocated nodes */
    if ((num_nodes = lsb_getalloc(&nodelist)) < 0) {
        orte_show_help("help-ras-lsf.txt", "nodelist-failed", true);
        return ORTE_ERR_NOT_AVAILABLE;
    }
    
    node = NULL;
    
    /* step through the list */
    for (i = 0; i < num_nodes; i++) {
        /* is this a repeat of the current node? */
        if (NULL != node && 0 == strcmp(nodelist[i], node->name)) {
            /* it is a repeat - just bump the slot count */
            ++node->slots;
            continue;
        }
        
        /* not a repeat - create a node entry for it */
        node = OBJ_NEW(orte_node_t);
        node->name = strdup(nodelist[i]);
        node->slots_inuse = 0;
        node->slots_max = 0;
        node->slots = 1;
        opal_list_append(nodes, &node->super);
    }
        
    /* release the nodelist from lsf */
    opal_argv_free(nodelist);

    /* check for an affinity file */
    if (NULL != (affinity_file = getenv("LSB_AFFINITY_HOSTFILE"))) {
        /* check to see if the file is empty - if it is,
         * then affinity wasn't actually set for this job */
        if (0 != stat(affinity_file, &buf)) {
            orte_show_help("help-ras-lsf.txt", "affinity-file-not-found", true, affinity_file);
            return ORTE_ERR_SILENT;
        }
        if (0 == buf.st_size) {
            /* no affinity, so just return */
            return ORTE_SUCCESS;
        }
        /* the affinity file sequentially lists rank locations, with
         * cpusets given as physical cpu-ids. Setup the job object
         * so it knows to process this accordingly */
        if (NULL == jdata->map) {
            jdata->map = OBJ_NEW(orte_job_map_t);
        }
        ORTE_SET_MAPPING_POLICY(jdata->map->mapping, ORTE_MAPPING_SEQ);
        jdata->map->req_mapper = strdup("seq"); // need sequential mapper
        /* tell the sequential mapper that all cpusets are to be treated as "physical" */
        orte_set_attribute(&jdata->attributes, ORTE_JOB_PHYSICAL_CPUIDS, true, NULL, OPAL_BOOL);
        /* LSF provides its info as hwthreads, so set the hwthread-as-cpus flag */
        opal_hwloc_use_hwthreads_as_cpus = true;
        /* don't override something provided by the user, but default to bind-to hwthread */
        if (!OPAL_BINDING_POLICY_IS_SET(opal_hwloc_binding_policy)) {
            OPAL_SET_BINDING_POLICY(opal_hwloc_binding_policy, OPAL_BIND_TO_HWTHREAD);
        }
        /* get the apps and set the hostfile attribute in each to point to
         * the hostfile */
        for (i=0; i < jdata->apps->size; i++) {
            if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, i))) {
                continue;
            }
            orte_set_attribute(&app->attributes, ORTE_APP_HOSTFILE, true, (void*)affinity_file, OPAL_STRING);
        }

        return ORTE_SUCCESS;
    }
    
    return ORTE_SUCCESS;
}

static int finalize(void)
{
    return ORTE_SUCCESS;
}
