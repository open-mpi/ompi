/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include <unistd.h>
#include <string.h>
#include <ctype.h>

#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "orte/include/orte_constants.h"
#include "orte/include/orte_types.h"
#include "orte/mca/ras/base/base.h"
#include "orte/mca/ras/base/ras_base_node.h"
#include "ras_slurm.h"


/*
 * Local functions
 */
static int allocate(orte_jobid_t jobid);
static int node_insert(opal_list_t *);
static int node_query(opal_list_t *);
static int deallocate(orte_jobid_t jobid);
static int finalize(void);

static int discover(char *regexp, opal_list_t *nodelist);
static int parse_ranges(char *base, char *ranges, char ***nodelist);
static int parse_range(char *base, char *range, char ***nodelist);



/*
 * Global variable
 */
orte_ras_base_module_t orte_ras_slurm_module = {
    allocate,
    node_insert,
    node_query,
    deallocate,
    finalize
};


/**
 * Discover available (pre-allocated) nodes.  Allocate the
 * requested number of nodes/process slots to the job.
 *  
 */
static int allocate(orte_jobid_t jobid)
{
    int ret;
    char *slurm_node_str;
    opal_list_t nodes;
    opal_list_item_t* item;

    slurm_node_str = getenv("SLURM_NODELIST");
    if (NULL == slurm_node_str) {
        opal_show_help("help-ras-slurm.txt", "env-var-not-found",
                       "SLURM_NODELIST");
        return ORTE_ERR_NOT_FOUND;
    }

    OBJ_CONSTRUCT(&nodes, opal_list_t);
    if (ORTE_SUCCESS != (ret = discover(slurm_node_str, &nodes))) {
        opal_output(orte_ras_base.ras_output,
                    "ras:slurm:allocate: discover failed!");
        return ret;
    }
    ret = orte_ras_base_allocate_nodes_by_slot(jobid, &nodes);

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

static int node_insert(opal_list_t *nodes)
{
    return orte_ras_base_node_insert(nodes);
}

static int node_query(opal_list_t *nodes)
{
    return orte_ras_base_node_query(nodes);
}

/*
 * There's really nothing to do here
 */
static int deallocate(orte_jobid_t jobid)
{
    opal_output(orte_ras_base.ras_output, 
                "ras:slurm:deallocate: success (nothing to do)");
    return ORTE_SUCCESS;
}


/*
 * There's really nothing to do here
 */
static int finalize(void)
{
    opal_output(orte_ras_base.ras_output, 
                "ras:slurm:finalize: success (nothing to do)");
    return ORTE_SUCCESS;
}


/**
 * Discover the available resources.  Obtain directly from SLURM (and
 * therefore have no need to validate) -- ignore hostfile or any other
 * user-specified parameters.
 *
 *  - validate any nodes specified via hostfile/commandline
 *  - check for additional nodes that have already been allocated
 */

static int discover(char *regexp, opal_list_t* nodelist)
{
    int i, j, len, ret, count, reps;
    char *base, **names = NULL;
    char *begptr, *endptr, *tasks_per_node;
    int *slots;
    
    if (NULL == regexp) {
        return ORTE_SUCCESS;
    }
    base = strdup(regexp);
    if (NULL == base) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    opal_output(orte_ras_base.ras_output, 
                "ras:slurm:allocate:discover: checking nodelist: %s", regexp);
    
    /* Find the base */
    
    len = strlen(regexp);
    for (i = 0; i < len; ++i) {
        if (base[i] == '[') {
            base[i] = '\0';
            break;
        }
    }
    
    /* If we didn't find a range, then this is it */
    
    if (i >= len) {
        orte_ras_node_t *node;
        opal_output(orte_ras_base.ras_output, 
                    "ras:slurm:allocate:discover: found single node");
        node = OBJ_NEW(orte_ras_node_t);
        if (NULL == node) {
            ret = ORTE_ERR_OUT_OF_RESOURCE;
        } else {
            opal_argv_append_nosize(&names, base);
            ret = ORTE_SUCCESS;
        }
        free(base);
    } else {
        
        /* If we did find a range, find the end of the range */
        
        for (j = i; j < len; ++j) {
            if (base[j] == ']') {
                base[j] = '\0';
                break;
            }
        }
        if (j >= len) {
            free(base);
            return ORTE_ERR_NOT_FOUND;
        }
        
        ret = parse_ranges(base, base + i + 1, &names);
    }

    /* Find the number of slots per node */

    slots = malloc(sizeof(int) * opal_argv_count(names));
    if (NULL == slots) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    memset(slots, 0, sizeof(int) * opal_argv_count(names));
    tasks_per_node = getenv("SLURM_TASKS_PER_NODE");
    if (NULL == tasks_per_node) {
        opal_show_help("help-ras-slurm.txt", "env-var-not-found",
                       "SLURM_TASKS_PER_NODE");
        return ORTE_ERR_NOT_FOUND;
    }
    begptr = tasks_per_node;
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
        for (i = 0; i < reps; i++) {
            slots[j++] = count;
        }
        
        if (*endptr == ',') {
            begptr = endptr + 1;
        } else if (*endptr == '\0') {
            break;
        } else {
            opal_show_help("help-ras-slurm.txt", "env-var-bad-value",
                           "SLURM_TASKS_PER_NODE", tasks_per_node);
            return ORTE_ERR_NOT_FOUND;
        }
    }

    /* Convert the argv of node names to a list of ras_base_node_t's */

    if (ORTE_SUCCESS == ret) {
        for (i = 0; NULL != names && NULL != names[i]; ++i) {
            orte_ras_node_t *node;
            
            opal_output(orte_ras_base.ras_output, 
                        "ras:slurm:allocate:discover: adding node %s (%d slot%s)",
                        names[i], slots[i], (1 == slots[i]) ? "" : "s");
            node = OBJ_NEW(orte_ras_node_t);
            if (NULL == node) {
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
    }

    /* All done */

    return ret;
}


/*
 * Parse one or more ranges in a set
 */
static int parse_ranges(char *base, char *ranges, char ***names)
{
    int i, len, ret;
    char *start, *orig;
    
    /* Look for commas, the separator between ranges */

    len = strlen(ranges);
    for (orig = start = ranges, i = 0; i < len; ++i) {
        if (',' == ranges[i]) {
            ranges[i] = '\0';
            if (ORTE_SUCCESS != (ret = parse_range(base, start, names))) {
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
        if (ORTE_SUCCESS != (ret = parse_range(base, start, names))) {
            return ret;
        }
    }

    /* All done */

    return ORTE_SUCCESS;
}


/*
 * Parse a single range in a set
 */
static int parse_range(char *base, char *range, char ***names)
{
    char *str, temp1[BUFSIZ], temp2[BUFSIZ];
    size_t i, j, start, end;
    size_t base_len, len;
    size_t str_start, str_end;
    size_t num_str_len;
    bool found;
    
    len = strlen(range);
    base_len = strlen(base);
    
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
        return ORTE_ERR_NOT_FOUND;
    }
    
    /* Make strings for all values in the range */
    
    for (i = start; i <= end; ++i) {
        len = base_len + 32;
        str = malloc(len);
        if (NULL == str) {
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        
        str[0] = '\0';
        snprintf(temp1, BUFSIZ - 1, "%s", base);
        snprintf(temp2, BUFSIZ - 1, "%lu", (long) i);
        temp1[BUFSIZ - 1] = temp2[BUFSIZ - 1] = '\0';
        
        /* Do we need zero pading? */
        
        if (strlen(temp2) < num_str_len) {
            for (j = 0; j < num_str_len - strlen(temp2); ++j) {
                strcat(temp1, "0");
            }
        }
        snprintf(str, len - 1, "%s%s", temp1, temp2);
        str[len - 1] = '\0';
        opal_argv_append_nosize(names, str);
    }
    
    /* All done */
    
    return ORTE_SUCCESS;
}
