/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
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

#include <sys/types.h>
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "opal/util/argv.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/dss/dss_internal.h"

#include "orte/mca/rmgr/base/rmgr_private.h"

/*
 * APP CONTEXT
 */
int orte_rmgr_base_print_app_context(char **output, char *prefix, orte_app_context_t *src, orte_data_type_t type)
{
    char *tmp, *tmp2, *tmp3, *pfx, *pfx2;
    orte_std_cntr_t j;
    int i, rc, count;

    /* set default result */
    *output = NULL;

    /* protect against NULL prefix */
    if (NULL == prefix) {
        asprintf(&pfx2, " ");
    } else {
        asprintf(&pfx2, "%s", prefix);
    }

    asprintf(&tmp, "%sData for app_context: index %lu\tapp: %s\n%s\tNum procs: %lu",
                    pfx2, (unsigned long)src->idx, src->app,
                    pfx2, (unsigned long)src->num_procs);

    count = opal_argv_count(src->argv);
    for (i=0; i < count; i++) {
        asprintf(&tmp2, "%s\n%s\tArgv[%d]: %s", tmp, pfx2, i, src->argv[i]);
        free(tmp);
        tmp = tmp2;
    }

    count = opal_argv_count(src->env);
    for (i=0; i < count; i++) {
        asprintf(&tmp2, "%s\n%s\tEnv[%lu]: %s", tmp, pfx2, (unsigned long)i, src->env[i]);
        free(tmp);
        tmp = tmp2;
    }

    asprintf(&tmp2, "%s\n%s\tWorking dir: %s (user: %d)\n%s\tNum maps: %lu", tmp, pfx2, src->cwd, (int) src->user_specified_cwd,
                    pfx2, (unsigned long)src->num_map);
    free(tmp);
    tmp = tmp2;

    asprintf(&pfx, "%s\t", pfx2);

    for (j = 0; j < src->num_map; ++j) {
        if (ORTE_SUCCESS != (rc = orte_rmgr_base_print_app_context_map(&tmp2, pfx, src->map_data[j], ORTE_APP_CONTEXT_MAP))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        asprintf(&tmp3, "%s\n%s", tmp, tmp2);
        free(tmp);
        free(tmp2);
        tmp = tmp3;
    }

    /* set the return */
    *output = tmp;

    free(pfx2);
    return ORTE_SUCCESS;
}

/*
 * APP CONTEXT MAP
 */
int orte_rmgr_base_print_app_context_map(char **output, char *prefix, orte_app_context_map_t *src, orte_data_type_t type)
{
    asprintf(output, "%sData for app_context_map: Type: %lu\tData: %s",
                    prefix, (unsigned long)src->map_type, src->map_data);

    return ORTE_SUCCESS;
}


/*
 * ATTRIBUTE
 */
int orte_rmgr_base_print_attribute(char **output, char *prefix, orte_attribute_t *src, orte_data_type_t type)
{
    char *tmp, *pfx2, *pfx3;
    int rc;
    
    /* set default result */
    *output = NULL;
    
    /* protect against NULL prefix */
    if (NULL == prefix) {
        asprintf(&pfx2, " ");
    } else {
        asprintf(&pfx2, "%s", prefix);
    }
    
    asprintf(&pfx3, "%s\t", pfx2);
    if (ORTE_SUCCESS != (rc = orte_dss.print(&tmp, pfx3, src, ORTE_GPR_KEYVAL))) {
        ORTE_ERROR_LOG(rc);
        free(pfx2);
        free(pfx3);
        return rc;
    }
    
    asprintf(output, "%sAttribute:\n%s", pfx2, tmp);
    
    free(pfx2);
    free(pfx3);
    return ORTE_SUCCESS;
}

int orte_rmgr_base_print_attr_list(char **output, char *prefix, opal_list_t *src, orte_data_type_t type)
{
    opal_list_item_t *item;
    char *tmp, *tmp2, *tmp3, *pfx2, *pfx3;
    int rc;
    
    /* set default result */
    *output = NULL;
    
    /* protect against NULL prefix */
    if (NULL == prefix) {
        asprintf(&pfx2, " ");
    } else {
        asprintf(&pfx2, "%s", prefix);
    }
    
    asprintf(&tmp, "%sList of %ld Attributes:\n", pfx2, (long)opal_list_get_size(src));
    
    asprintf(&pfx3, "%s\t", pfx2);
    for (item = opal_list_get_first(src);
         item != opal_list_get_end(src);
         item = opal_list_get_next(item)) {
        if (ORTE_SUCCESS != (rc = orte_rmgr_base_print_attribute(&tmp2, pfx3, (orte_attribute_t*)item, ORTE_ATTRIBUTE))) {
            ORTE_ERROR_LOG(rc);
            free(pfx2);
            free(pfx3);
            return rc;
        }
        asprintf(&tmp3, "%s%s\n", tmp, tmp2);
        free(tmp);
        free(tmp2);
        tmp = tmp3;
    }
    
    *output = tmp;
    free(pfx2);
    free(pfx3);
    
    return ORTE_SUCCESS;
}

