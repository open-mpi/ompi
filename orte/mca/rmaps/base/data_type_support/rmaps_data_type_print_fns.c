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
#include "opal/class/opal_list.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/dss/dss.h"

#include "orte/mca/rmaps/base/rmaps_private.h"

/*
 * JOB_MAP
 */
int orte_rmaps_base_print_map(char **output, char *prefix, orte_rmaps_base_map_t *src, orte_data_type_t type)
{
    char *tmp, *tmp2, *tmp3, *pfx, *pfx2;
    orte_rmaps_base_node_t *srcnode;
    orte_std_cntr_t i, num_nodes;
    opal_list_item_t *item;
    int rc;
    
    /* set default result */
    *output = NULL;

    /* protect against NULL prefix */
    if (NULL == prefix) {
        asprintf(&pfx2, " ");
    } else {
        asprintf(&pfx2, "%s", prefix);
    }

    asprintf(&tmp, "%sMap for app_context:", pfx2);

    asprintf(&pfx, "%s\t", pfx2);
    free(pfx2);
    
    if (ORTE_SUCCESS != (rc = orte_dss.print(&tmp2, pfx, src->app, ORTE_APP_CONTEXT))) {
        ORTE_ERROR_LOG(rc);
        free(pfx);
        free(tmp);
        return rc;
    }
    asprintf(&tmp3, "%s\n%s\n%sNum elements in procs array: %ld", tmp, tmp2, pfx, (long)src->num_procs);
    free(tmp);
    free(tmp2);
    
    for (i=0; i < src->num_procs; i++) {
        if (ORTE_SUCCESS != (rc = orte_rmaps_base_print_mapped_proc(&tmp, pfx, src->procs[i], ORTE_MAPPED_PROC))) {
            ORTE_ERROR_LOG(rc);
            free(pfx);
            free(tmp3);
            return rc;
        }
        asprintf(&tmp2, "%s\n%s", tmp3, tmp);
        free(tmp);
        free(tmp3);
        tmp3 = tmp2;
    }
    
    num_nodes = (orte_std_cntr_t)opal_list_get_size(&(src->nodes));
    asprintf(&tmp, "%s\n%sNum elements in nodes list: %ld", tmp3, pfx, (long)num_nodes);
    
    for (item = opal_list_get_first(&(src->nodes));
         item != opal_list_get_end(&(src->nodes));
         item = opal_list_get_next(item)) {
        srcnode = (orte_rmaps_base_node_t*)item;
        if (ORTE_SUCCESS != (rc = orte_rmaps_base_print_mapped_node(&tmp2, pfx, srcnode, ORTE_MAPPED_NODE))) {
            ORTE_ERROR_LOG(rc);
            free(pfx);
            free(tmp);
            return rc;
        }
        asprintf(&tmp3, "%s\n%s", tmp, tmp2);
        free(tmp);
        free(tmp2);
        tmp = tmp3;
    }
    
    /* set the return */
    *output = tmp;

    free(pfx);
    return ORTE_SUCCESS;
}


/*
 * MAPPED_PROC
 */
int orte_rmaps_base_print_mapped_proc(char **output, char *prefix, orte_rmaps_base_proc_t *src, orte_data_type_t type)
{
    char *tmp, *tmp2, *tmp3, *pfx, *pfx2;
    int rc;
    
    /* set default result */
    *output = NULL;
    
    /* protect against NULL prefix */
    if (NULL == prefix) {
        asprintf(&pfx2, " ");
    } else {
        asprintf(&pfx2, "%s", prefix);
    }
    
    asprintf(&tmp, "%sMapped proc:", pfx2);
    
    asprintf(&pfx, "%s\t", pfx2);
    
    if (NULL != src->app) {
        asprintf(&tmp2, "%s\n%sApp name: %s", tmp, pfx, src->app);
    } else {
        asprintf(&tmp2, "%s\n%sApplication has NULL name", tmp, pfx);
    }
    free(tmp);        
    
    if (ORTE_SUCCESS != (rc = orte_rmaps_base_print_mapped_node(&tmp, pfx, src->proc_node, ORTE_MAPPED_NODE))) {
        ORTE_ERROR_LOG(rc);
        free(pfx);
        free(tmp2);
        return rc;
    }
    asprintf(&tmp3, "%s\n%s\n%s\n%sProc Name:", tmp2, pfx, tmp, pfx);
    free(tmp2);
    free(tmp);
    
    if (ORTE_SUCCESS != (rc = orte_dss.print(&tmp2, pfx, &(src->proc_name), ORTE_NAME))) {
        ORTE_ERROR_LOG(rc);
        free(pfx);
        free(tmp3);
        return rc;
    }
    asprintf(&tmp, "%s\n%s\n%sProc Rank: %ld\tPLS pid: %ld\tLocal PID: %ld\n", tmp3, tmp2, pfx,
             (long)src->proc_rank, (long)src->pid, (long)src->local_pid);
    free(tmp2);
    free(tmp3);
    
    /* set the return */
    *output = tmp;
    
    free(pfx);
    free(pfx2);
    return ORTE_SUCCESS;
}

/*
 * MAPPED_NODE
 */
int orte_rmaps_base_print_mapped_node(char **output, char *prefix, orte_rmaps_base_node_t *src, orte_data_type_t type)
{
    int rc;
    char *tmp, *tmp2, *tmp3, *pfx, *pfx2;
    orte_std_cntr_t num_procs;
#if 0
    opal_list_item_t *item;
    orte_rmaps_base_proc_t *srcproc;
#endif
    
    /* set default result */
    *output = NULL;
    
    /* protect against NULL prefix */
    if (NULL == prefix) {
        asprintf(&pfx2, " ");
    } else {
        asprintf(&pfx2, "%s", prefix);
    }
    
    asprintf(&tmp, "%sMapped node:", pfx2);
    
    asprintf(&pfx, "%s\t", pfx2);
    free(pfx2);
    
    if (ORTE_SUCCESS != (rc = orte_dss.print(&tmp2, pfx, src->node, ORTE_RAS_NODE))) {
        ORTE_ERROR_LOG(rc);
        free(pfx);
        free(tmp);
        return rc;
    }
    
    num_procs = (orte_std_cntr_t)opal_list_get_size(&(src->node_procs));
    asprintf(&tmp3, "%s\n%s\n%sNum elements in procs list: %ld", tmp, tmp2, pfx, (long)num_procs);
    free(tmp);
    free(tmp2);
#if 0
   for (item = opal_list_get_first(&(src->node_procs));
         item != opal_list_get_end(&(src->node_procs));
         item = opal_list_get_next(item)) {
        srcproc = (orte_rmaps_base_proc_t*)item;
        if (ORTE_SUCCESS != (rc = orte_rmaps_base_print_mapped_proc(&tmp2, pfx, srcproc, ORTE_MAPPED_PROC))) {
            ORTE_ERROR_LOG(rc);
            free(pfx);
            free(tmp);
            return rc;
        }
        asprintf(&tmp, "%s\n%s", tmp3, tmp2);
        free(tmp3);
        free(tmp2);
        tmp3 = tmp;
    }
#endif
    /* set the return */
    *output = tmp3;
    
    free(pfx);
    return ORTE_SUCCESS;
}

