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
#include "ras_slurm.h"


/*
 * Local functions
 */
static int orte_ras_slurm_allocate(orte_jobid_t jobid, opal_list_t *attributes);
static int orte_ras_slurm_deallocate(orte_jobid_t jobid);
static int orte_ras_slurm_finalize(void);

static int orte_ras_slurm_discover(char *regexp, char* tasks_per_node,
                                   opal_list_t *nodelist);
static int orte_ras_slurm_parse_ranges(char *base, char *ranges, char ***nodelist);
static int orte_ras_slurm_parse_range(char *base, char *range, char ***nodelist);



/*
 * Global variable
 */
orte_ras_base_module_t orte_ras_slurm_module = {
    orte_ras_slurm_allocate,
    orte_ras_base_node_insert,
    orte_ras_base_node_query,
    orte_ras_base_node_query_alloc,
    orte_ras_base_node_lookup,
    orte_ras_slurm_deallocate,
    orte_ras_slurm_finalize
};

/**
 * Discover available (pre-allocated) nodes.  Allocate the
 * requested number of nodes/process slots to the job.
 *  
 */
static int orte_ras_slurm_allocate(orte_jobid_t jobid, opal_list_t *attributes)
{
    int ret;
    char *slurm_node_str, *regexp;
    char *tasks_per_node, *node_tasks;
    opal_list_t nodes;
    opal_list_item_t* item;
  
    slurm_node_str = getenv("SLURM_NODELIST");
    if (NULL == slurm_node_str) {
        opal_show_help("help-ras-slurm.txt", "slurm-env-var-not-found", 1,
                       "SLURM_NODELIST");
        return ORTE_ERR_NOT_FOUND;
    }
    regexp = strdup(slurm_node_str);
    
    tasks_per_node = getenv("SLURM_JOB_CPUS_PER_NODE");
    if (NULL == tasks_per_node) {
        /* didn't find SLURM 1.2 or above envar - look for prior
         * version
         */
        tasks_per_node = getenv("SLURM_TASKS_PER_NODE");
        if (NULL == tasks_per_node) {
            /* couldn't find any version - abort */
            opal_show_help("help-ras-slurm.txt", "slurm-env-var-not-found", 1,
                           "SLURM_JOB_CPUS_PER NODE or SLURM_TASKS_PER_NODE");
            return ORTE_ERR_NOT_FOUND;
        }
    }
    node_tasks = strdup(tasks_per_node);

    if(NULL == regexp || NULL == node_tasks) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    OBJ_CONSTRUCT(&nodes, opal_list_t);
 
    ret = orte_ras_slurm_discover(regexp, node_tasks, &nodes);
    free(regexp);
    free(node_tasks);
    if (ORTE_SUCCESS != ret) {
        opal_output(orte_ras_base.ras_output,
                    "ras:slurm:allocate: discover failed!");
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
                    "ras:slurm:allocate: success");
    } else {
        opal_output(orte_ras_base.ras_output, 
                    "ras:slurm:allocate: failure (base_allocate_nodes=%d)", ret);
    }
    return ret;
}

/*
 * There's really nothing to do here
 */
static int orte_ras_slurm_deallocate(orte_jobid_t jobid)
{
    opal_output(orte_ras_base.ras_output, 
                "ras:slurm:deallocate: success (nothing to do)");
    return ORTE_SUCCESS;
}


/*
 * There's really nothing to do here
 */
static int orte_ras_slurm_finalize(void)
{
    opal_output(orte_ras_base.ras_output, 
                "ras:slurm:finalize: success (nothing to do)");
    return ORTE_SUCCESS;
}


/**
 * Discover the available resources.
 * 
 * In order to fully support slurm, we need to be able to handle 
 * node regexp/task_per_node strings such as:
 * foo,bar    5,3
 * foo        5
 * foo[2-10,12,99-105],bar,foobar[3-11] 2(x10),5,100(x16)
 *
 * @param *regexp A node regular expression from SLURM (i.e. SLURM_NODELIST)
 * @param *tasks_per_node A tasks per node expression from SLURM
 *                        (i.e. SLURM_TASKS_PER_NODE)
 * @param *nodelist A list which has already been constucted to return
 *                  the found nodes in
 */
static int orte_ras_slurm_discover(char *regexp, char *tasks_per_node,
                                   opal_list_t* nodelist)
{
    int i, j, len, ret, count, reps, num_nodes;
    char *base, **names = NULL;
    char *begptr, *endptr, *orig;
    int *slots;
    bool found_range = false;
    bool more_to_come = false;
    
    orig = base = strdup(regexp);
    if (NULL == base) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    opal_output(orte_ras_base.ras_output, 
                "ras:slurm:allocate:discover: checking nodelist: %s", regexp);
    
    do {
        /* Find the base */
        len = strlen(base);
        for (i = 0; i <= len; ++i) {
            if (base[i] == '[') {
                /* we found a range. this gets dealt with below */
                base[i] = '\0';
                found_range = true;
                break;
            }
            if (base[i] == ',') {
                /* we found a singleton node, and there are more to come */
                base[i] = '\0';
                found_range = false;
                more_to_come = true;
                break;
            }
            if (base[i] == '\0') {
                /* we found a singleton node */
                found_range = false;
                more_to_come = false;
                break;
            }
        }
        if(i == 0) {
            /* we found a special character at the beginning of the string */
            opal_show_help("help-ras-slurm.txt", "slurm-env-var-bad-value",
                           1, regexp, tasks_per_node, "SLURM_NODELIST");
            ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
            free(orig);
            return ORTE_ERR_BAD_PARAM;
        }
        
        if (found_range) {
            /* If we found a range, now find the end of the range */
            for (j = i; j < len; ++j) {
                if (base[j] == ']') {
                    base[j] = '\0';
                    break;
                }
            }
            if (j >= len) {
                /* we didn't find the end of the range */
                opal_show_help("help-ras-slurm.txt", "slurm-env-var-bad-value",
                               1, regexp, tasks_per_node, "SLURM_NODELIST");
                ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
                free(orig);
                return ORTE_ERR_BAD_PARAM;
            }
            
            ret = orte_ras_slurm_parse_ranges(base, base + i + 1, &names);
            if(ORTE_SUCCESS != ret) {
                opal_show_help("help-ras-slurm.txt", "slurm-env-var-bad-value",
                               1, regexp, tasks_per_node, "SLURM_NODELIST");
                ORTE_ERROR_LOG(ret);
                free(orig);
                return ret;
            }    
            if(base[j + 1] == ',') {
                more_to_come = true;
                base = &base[j + 2];
            } else {
                more_to_come = false;
            }
        } else {
            /* If we didn't find a range, just add the node */
            opal_output(orte_ras_base.ras_output, 
                        "ras:slurm:allocate:discover: found node %s", base);
            if(ORTE_SUCCESS != (ret = opal_argv_append_nosize(&names, base))) {
                ORTE_ERROR_LOG(ret);
                free(orig);
                return ret;
            }
            /* set base equal to the (possible) next base to look at */
            base = &base[i + 1];
        }
    } while(more_to_come);
   
    free(orig);
    
    num_nodes = opal_argv_count(names);

    /* Find the number of slots per node */

    slots = malloc(sizeof(int) * num_nodes);
    if (NULL == slots) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    memset(slots, 0, sizeof(int) * num_nodes);
    
    orig = begptr = strdup(tasks_per_node);
    if (NULL == begptr) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        free(slots);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    j = 0;
    while (begptr) {
        count = strtol(begptr, &endptr, 10);
        if ((endptr[0] == '(') && (endptr[1] == 'x')) {
            reps = strtol((endptr+2), &endptr, 10);
            if (endptr[0] == ')') {
                endptr++;
            }
        } else {
            reps = 1;
        }

        /**
         * TBP: it seems like it would be an error to have more slot 
         * descriptions than nodes. Turns out that this valid, and SLURM will
         * return such a thing. For instance, if I did:
         * srun -A -N 30 -w odin001
         * I would get SLURM_NODELIST=odin001 SLURM_TASKS_PER_NODE=4(x30)
         * That is, I am allocated 30 nodes, but since I only requested
         * one specific node, that's what is in the nodelist.
         * I'm not sure this is what users would expect, but I think it is
         * more of a SLURM issue than a orte issue, since SLURM is OK with it,
         * I'm ok with it
         */
        for (i = 0; i < reps && j < num_nodes; i++) {
            slots[j++] = count;
        }
            
        if (*endptr == ',') {
            begptr = endptr + 1;
        } else if (*endptr == '\0' || j >= num_nodes) {
            break;
        } else {
            opal_show_help("help-ras-slurm.txt", "slurm-env-var-bad-value", 1,
                           regexp, tasks_per_node, "SLURM_TASKS_PER_NODE");
            ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
            free(slots);
            free(orig);
            return ORTE_ERR_BAD_PARAM;
        }
    }

    free(orig);

    /* Convert the argv of node names to a list of ras_base_node_t's */

    for (i = 0; NULL != names && NULL != names[i]; ++i) {
        orte_ras_node_t *node;
        
        opal_output(orte_ras_base.ras_output, 
                    "ras:slurm:allocate:discover: adding node %s (%d slot%s)",
                    names[i], slots[i], (1 == slots[i]) ? "" : "s");
        node = OBJ_NEW(orte_ras_node_t);
        if (NULL == node) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            free(slots);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        node->node_name = strdup(names[i]);
        node->node_arch = NULL;
        node->node_state = ORTE_NODE_STATE_UP;
        /* JMS: this should not be hard-wired to 0, but there's no
           other value to put it to [yet]... */
        node->node_cellid = 0;
        node->node_slots_inuse = 0;
        node->node_slots_max = 0;
        node->node_slots = slots[i];
        opal_list_append(nodelist, &node->super);
    }
    free(slots);
    opal_argv_free(names);

    /* Now add the nodes to the registry */

    ret = orte_ras_base_node_insert(nodelist);
    if(ORTE_SUCCESS != ret) {
        ORTE_ERROR_LOG(ret);
    }

    /* All done */
    return ret;
}


/*
 * Parse one or more ranges in a set
 *
 * @param base     The base text of the node name
 * @param *ranges  A pointer to a range. This can contain multiple ranges
 *                 (i.e. "1-3,10" or "5" or "9,0100-0130,250") 
 * @param ***names An argv array to add the newly discovered nodes to
 */
static int orte_ras_slurm_parse_ranges(char *base, char *ranges, char ***names)
{
    int i, len, ret;
    char *start, *orig;
    
    /* Look for commas, the separator between ranges */

    len = strlen(ranges);
    for (orig = start = ranges, i = 0; i < len; ++i) {
        if (',' == ranges[i]) {
            ranges[i] = '\0';
            ret = orte_ras_slurm_parse_range(base, start, names);
            if (ORTE_SUCCESS != ret) {
                ORTE_ERROR_LOG(ret);
                return ret;
            }
            start = ranges + i + 1;
        }
    }

    /* Pick up the last range, if it exists */

    if (start < orig + len) {
        opal_output(orte_ras_base.ras_output, 
                    "ras:slurm:allocate:discover: parse range %s (2)",
                    start);
        ret = orte_ras_slurm_parse_range(base, start, names);
        if (ORTE_SUCCESS != ret) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
    }

    /* All done */
    return ORTE_SUCCESS;
}


/*
 * Parse a single range in a set and add the full names of the nodes
 * found to the names argv
 *
 * @param base     The base text of the node name
 * @param *ranges  A pointer to a single range. (i.e. "1-3" or "5") 
 * @param ***names An argv array to add the newly discovered nodes to
 */
static int orte_ras_slurm_parse_range(char *base, char *range, char ***names)
{
    char *str, temp1[BUFSIZ];
    size_t i, j, start, end;
    size_t base_len, len, num_len;
    size_t str_start, str_end;
    size_t num_str_len;
    bool found;
    int ret;
    
    len = strlen(range);
    base_len = strlen(base);
    /* Silence compiler warnings; start and end are always assigned
       properly, below */
    start = end = 0;
    
    /* Look for the beginning of the first number */
    
    for (found = false, i = 0; i < len; ++i) {
        if (isdigit((int) range[i])) {
            if (!found) {
                str_start = i;
                start = atoi(range + i);
                found = true;
                break;
            }
        }
    }
    if (!found) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    
    /* Look for the end of the first number */
    
    for (found = false, num_str_len = 0; i < len; ++i, ++num_str_len) {
        if (!isdigit((int) range[i])) {
            break;
        }
    }
    
    /* Was there no range, just a single number? */
    
    if (i >= len) {
        str_end = len;
        end = start;
        found = true;
    }
    
    /* Nope, there was a range.  Look for the beginning of the second
       number */
    
    else {
        str_end = i - 1;
        for (; i < len; ++i) {
            if (isdigit((int) range[i])) {
                end = atoi(range + i);
                found = true;
                break;
            }
        }
    }
    if (!found) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    
    /* Make strings for all values in the range */
    
    len = base_len + num_str_len + 32;
    str = malloc(len);
    if (NULL == str) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    strcpy(str, base);
    for (i = start; i <= end; ++i) {
        str[base_len] = '\0';
        snprintf(temp1, BUFSIZ - 1, "%lu", (long) i);
        
        /* Do we need zero pading? */
        
        if ((num_len = strlen(temp1)) < num_str_len) {
            for (j = base_len; j < base_len + (num_str_len - num_len); ++j) {
                str[j] = '0';
            }
            str[j] = '\0';
        }
        strcat(str, temp1);
        ret = opal_argv_append_nosize(names, str);
        if(ORTE_SUCCESS != ret) {
            ORTE_ERROR_LOG(ret);
            free(str);
            return ret;
        }
    }
    free(str);
    
    /* All done */
    return ORTE_SUCCESS;
}

