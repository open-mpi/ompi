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
 *
 */

#include "orte_config.h"

#include <unistd.h>
#include <string.h>
#include <ctype.h>

#include "orte/include/orte_constants.h"
#include "orte/mca/sds/sds.h"
#include "orte/mca/sds/base/base.h"
#include "orte/mca/sds/slurm/sds_slurm.h"
#include "orte/util/proc_info.h"
#include "opal/util/opal_environ.h"
#include "opal/mca/base/mca_base_param.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ns/base/base.h"
#include "orte/util/sys_info.h"
#include "opal/util/argv.h"

orte_sds_base_module_t orte_sds_slurm_module = {
    orte_sds_base_basic_contact_universe,
    orte_sds_slurm_set_name,
    orte_sds_slurm_finalize,
};

static char *get_slurm_nodename(int nodeid);
static int parse_ranges(char *base, char *ranges, char ***nodelist);
static int parse_range(char *base, char *range, char ***nodelist);


int
orte_sds_slurm_set_name(void)
{
    int rc;
    int id;
    int vpid_start;
    int num_procs;
    char* name_string = NULL;
    int slurm_nodeid;

    /* start by getting our cellid, jobid, and vpid (which is the
       starting vpid for the list of daemons) */
    id = mca_base_param_register_string("ns", "nds", "name", NULL, NULL);
    mca_base_param_lookup_string(id, &name_string);

    if(name_string != NULL) {
        if (ORTE_SUCCESS != 
            (rc = orte_ns_base_convert_string_to_process_name(&(orte_process_info.my_name),
                                                              name_string))) {
            ORTE_ERROR_LOG(rc);
            free(name_string);
            return rc;
        }
        free(name_string);

    } else {
        orte_cellid_t cellid;
        orte_jobid_t jobid;
        orte_vpid_t vpid;
        char* cellid_string;
        char* jobid_string;
        char* vpid_string;
      
        id = mca_base_param_register_string("ns", "nds", "cellid", NULL, NULL);
        mca_base_param_lookup_string(id, &cellid_string);
        if (NULL == cellid_string) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        if (ORTE_SUCCESS != (rc = orte_ns.convert_string_to_cellid(&cellid, cellid_string))) {
            ORTE_ERROR_LOG(rc);
            return(rc);
        }
            
        id = mca_base_param_register_string("ns", "nds", "jobid", NULL, NULL);
        mca_base_param_lookup_string(id, &jobid_string);
        if (NULL == jobid_string) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        if (ORTE_SUCCESS != (rc = orte_ns.convert_string_to_jobid(&jobid, jobid_string))) {
            ORTE_ERROR_LOG(rc);
            return(rc);
        }
        
        id = mca_base_param_register_string("ns", "nds", "vpid", NULL, NULL);
        mca_base_param_lookup_string(id, &vpid_string);
        if (NULL == vpid_string) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        if (ORTE_SUCCESS != (rc = orte_ns.convert_string_to_vpid(&vpid, vpid_string))) {
            ORTE_ERROR_LOG(rc);
            return(rc);
        }

        if (ORTE_SUCCESS != (rc = orte_ns.create_process_name(&(orte_process_info.my_name),
                                                              cellid,
                                                              jobid,
                                                              vpid))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    /* fix up the base name and make it the "real" name */
    slurm_nodeid = atoi(getenv("SLURM_NODEID"));
    orte_process_info.my_name->vpid += slurm_nodeid;

    /* fix up the system info nodename to match exactly what slurm returned */
    if (NULL != orte_system_info.nodename) {
        free(orte_system_info.nodename);
    }
    orte_system_info.nodename = get_slurm_nodename(slurm_nodeid);

    id = mca_base_param_register_int("ns", "nds", "vpid_start", NULL, -1);
    mca_base_param_lookup_int(id, &vpid_start);
    if (vpid_start < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }

    id = mca_base_param_register_int("ns", "nds", "num_procs", NULL, -1);
    mca_base_param_lookup_int(id, &num_procs);
    if (num_procs < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    
    orte_process_info.vpid_start = (orte_vpid_t)vpid_start;
    orte_process_info.num_procs = (size_t)num_procs;
    return ORTE_SUCCESS;
}


int 
orte_sds_slurm_finalize(void)
{
    return ORTE_SUCCESS;
}


static char *
get_slurm_nodename(int nodeid)
{
    int i, j, len;
    char *base, **names = NULL;
    char *slurm_nodelist;
    char *ret;

    slurm_nodelist = getenv("SLURM_NODELIST");
    
    if (NULL == slurm_nodelist) {
        return NULL;
    }
    base = strdup(slurm_nodelist);
    if (NULL == base) {
        return NULL;
    }
    
    /* Find the base */
    len = strlen(slurm_nodelist);
    for (i = 0; i < len; ++i) {
        if (base[i] == '[') {
            base[i] = '\0';
            break;
        }
    }
    
    /* If we didn't find a range, then this is it */
    if (i >= len) {
        ret = strdup(base);
        free(base);
        if (0 == nodeid) {
            return ret;
        } else {
            return NULL;
        }
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
            return NULL;
        }
        
        parse_ranges(base, base + i + 1, &names);
    }

    /* find our entry */
    ret = strdup(names[nodeid]);

    opal_argv_free(names);

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
