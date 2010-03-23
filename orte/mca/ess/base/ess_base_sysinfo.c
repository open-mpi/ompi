/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdlib.h>

#include "opal/util/opal_environ.h"
#include "opal/util/if.h"
#include "opal/class/opal_list.h"
#include "opal/mca/sysinfo/sysinfo_types.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/odls/base/odls_private.h"
#include "orte/util/proc_info.h"
#include "orte/util/nidmap.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/ess/base/base.h"

int orte_ess_base_query_sys_info(char *node, char **keys, opal_list_t *values)
{
    orte_nid_t *nid;
    opal_sysinfo_value_t *sys, *sys2;
    opal_list_item_t *item;
    int i;
    
    /* we currently only support the local node */
    if (NULL != node &&
        (0 != strcmp(node, orte_process_info.nodename) || !opal_ifislocal(node))) {
        return ORTE_SUCCESS;
    }
    
    if (ORTE_PROC_IS_HNP || ORTE_PROC_IS_DAEMON) {
        /* cycle through local sysinfo */
        for (i=0; NULL != keys[i]; i++) {
            for (item = opal_list_get_first(&orte_odls_globals.sysinfo);
                 item != opal_list_get_end(&orte_odls_globals.sysinfo);
                 item = opal_list_get_next(item)) {
                sys = (opal_sysinfo_value_t*)item;
                if (0 != strcmp(keys[i], sys->key)) {
                    continue;
                }
                /* matched - pass the value back */
                sys2 = OBJ_NEW(opal_sysinfo_value_t);
                sys2->key = strdup(sys->key);
                sys2->type = sys->type;
                if (OPAL_STRING == sys->type) {
                    sys2->data.str = strdup(sys->data.str);
                } else {
                    sys2->data.i64 = sys->data.i64;
                }
            }
        }
        return ORTE_SUCCESS;
    }
    
    /* get the nid of our local node */
    if (NULL == (nid = orte_util_lookup_nid(ORTE_PROC_MY_NAME))) {
        /* can't get it */
        return ORTE_ERR_NOT_FOUND;
    }
    
    /* cycle through this node's attributes looking for the keys */
    for (i=0; NULL != keys[i]; i++) {
        for (item = opal_list_get_first(&nid->sysinfo);
             item != opal_list_get_end(&nid->sysinfo);
             item = opal_list_get_next(item)) {
            sys = (opal_sysinfo_value_t*)item;
            
            if (0 != strcmp(keys[i], sys->key)) {
                continue;
            }
            /* matched - pass the value back */
            sys2 = OBJ_NEW(opal_sysinfo_value_t);
            sys2->key = strdup(sys->key);
            sys2->type = sys->type;
            if (OPAL_STRING == sys->type) {
                sys2->data.str = strdup(sys->data.str);
            } else {
                sys2->data.i64 = sys->data.i64;
            }
            opal_list_append(values, &sys2->super);
            /* can only be one entry for each key */
            break;
        }
    }
    
    return ORTE_SUCCESS;
}
