/*
 * Copyright (c) 2016-2018 Intel, Inc.  All rights reserved.
 * Copyright (c) 2019      IBM Corporation.  All rights reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/types.h"
#include "opal/types.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <ctype.h>

#include "opal/util/argv.h"
#include "opal/util/basename.h"
#include "opal/util/opal_environ.h"

#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"
#include "orte/util/show_help.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rmaps/base/base.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/regx/base/base.h"

#include "regx_naive.h"

static int nidmap_create(opal_pointer_array_t *pool, char **regex);

orte_regx_base_module_t orte_regx_naive_module = {
    .nidmap_create = nidmap_create,
    .nidmap_parse = orte_regx_base_nidmap_parse,
    .extract_node_names = orte_regx_base_extract_node_names,
    .encode_nodemap = orte_regx_base_encode_nodemap,
    .decode_daemon_nodemap = orte_regx_base_decode_daemon_nodemap,
    .generate_ppn = orte_regx_base_generate_ppn,
    .parse_ppn = orte_regx_base_parse_ppn
};

static int nidmap_create(opal_pointer_array_t *pool, char **regex)
{
    char *node;
    int n;
    char *nodenames;
    orte_regex_range_t *rng;
    opal_list_t dvpids;
    opal_list_item_t *item;
    char **regexargs = NULL, **vpidargs = NULL, *tmp, *tmp2;
    orte_node_t *nptr;
    orte_vpid_t vpid;

    if (mca_regx_naive_component.compress_vpids) {
        OBJ_CONSTRUCT(&dvpids, opal_list_t);
    }

    rng = NULL;
    for (n=0; n < pool->size; n++) {
        if (NULL == (nptr = (orte_node_t*)opal_pointer_array_get_item(pool, n))) {
            continue;
        }
        /* if no daemon has been assigned, then this node is not being used */
        if (NULL == nptr->daemon) {
            vpid = -1;  // indicates no daemon assigned
        } else {
            vpid = nptr->daemon->name.vpid;
        }

        if (mca_regx_naive_component.compress_vpids) {
            /* deal with the daemon vpid - see if it is next in the
             * current range */
            if (NULL == rng) {
                /* just starting */
                rng = OBJ_NEW(orte_regex_range_t);
                rng->vpid = vpid;
                rng->cnt = 1;
                opal_list_append(&dvpids, &rng->super);
            } else if (UINT32_MAX == vpid) {
                if (-1 == rng->vpid) {
                    rng->cnt++;
                } else {
                    /* need to start another range */
                    rng = OBJ_NEW(orte_regex_range_t);
                    rng->vpid = vpid;
                    rng->cnt = 1;
                    opal_list_append(&dvpids, &rng->super);
                }
            } else if (-1 == rng->vpid) {
                /* need to start another range */
                rng = OBJ_NEW(orte_regex_range_t);
                rng->vpid = vpid;
                rng->cnt = 1;
                opal_list_append(&dvpids, &rng->super);
            } else {
                /* is this the next in line */
                if (vpid == (orte_vpid_t)(rng->vpid + rng->cnt)) {
                    rng->cnt++;
                } else {
                    /* need to start another range */
                    rng = OBJ_NEW(orte_regex_range_t);
                    rng->vpid = vpid;
                    rng->cnt = 1;
                    opal_list_append(&dvpids, &rng->super);
                }
            }
        }
        else {
            asprintf(&tmp, "%u", vpid);
            opal_argv_append_nosize(&vpidargs, tmp);
            free(tmp);
        }

        node = nptr->name;
        opal_output_verbose(5, orte_regx_base_framework.framework_output,
                            "%s PROCESS NODE <%s>",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            node);

        /* Don't compress the name - just add it to the list */
        if (NULL != node) {
            /* solitary node */
            opal_argv_append_nosize(&regexargs, node);
        }
    }

    /* assemble final result */
    nodenames = opal_argv_join(regexargs, ',');
    /* cleanup */
    opal_argv_free(regexargs);

    if (mca_regx_naive_component.compress_vpids) {
        /* do the same for the vpids */
        tmp = NULL;
        while (NULL != (item = opal_list_remove_first(&dvpids))) {
            rng = (orte_regex_range_t*)item;
            if (1 < rng->cnt) {
                if (NULL == tmp) {
                    asprintf(&tmp, "%u(%u)", rng->vpid, rng->cnt);
                } else {
                    asprintf(&tmp2, "%s,%u(%u)", tmp, rng->vpid, rng->cnt);
                    free(tmp);
                    tmp = tmp2;
                }
            } else {
                if (NULL == tmp) {
                    asprintf(&tmp, "%u", rng->vpid);
                } else {
                    asprintf(&tmp2, "%s,%u", tmp, rng->vpid);
                    free(tmp);
                    tmp = tmp2;
                }
            }
            OBJ_RELEASE(rng);
        }
        OPAL_LIST_DESTRUCT(&dvpids);
    }
    else {
        tmp = opal_argv_join(vpidargs, ',');
        /* cleanup */
        opal_argv_free(vpidargs);
    }

    /* now concatenate the results into one string */
    asprintf(&tmp2, "%s@%s", nodenames, tmp);
    free(nodenames);
    free(tmp);
    *regex = tmp2;
    opal_output_verbose(5, orte_regx_base_framework.framework_output,
                        "%s Final regex: <%s>",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        *regex);
    return ORTE_SUCCESS;
}
