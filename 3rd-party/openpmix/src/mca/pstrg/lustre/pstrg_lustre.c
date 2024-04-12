/*
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 *
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"

#include <string.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#    include <sys/stat.h>
#endif
#ifdef HAVE_FCNTL_H
#    include <fcntl.h>
#endif
#include <time.h>

#include "pmix_common.h"

#include "src/class/pmix_list.h"
#include "src/include/pmix_globals.h"
#include "src/include/pmix_socket_errno.h"
#include "src/mca/base/pmix_mca_base_var.h"
#include "src/mca/preg/preg.h"
#include "src/util/pmix_alfg.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_error.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_environ.h"
#include "src/util/pmix_printf.h"

#include "pstrg_lustre.h"
#include "src/mca/pstrg/base/base.h"
#include "src/mca/pstrg/pstrg.h"

static pmix_status_t lustre_init(void);
static void lustre_finalize(void);
static pmix_status_t query(pmix_query_t queries[], size_t nqueries, pmix_list_t *results,
                           pmix_pstrg_query_cbfunc_t cbfunc, void *cbdata);

pmix_pstrg_base_module_t pmix_pstrg_lustre_module = {.name = "lustre",
                                                     .init = lustre_init,
                                                     .finalize = lustre_finalize,
                                                     .query = query};

static pmix_status_t lustre_init(void)
{
    pmix_output_verbose(2, pmix_pstrg_base_framework.framework_output, "pstrg: lustre init");

    /* ADD HERE:
     *
     * Discover/connect to any available Lustre systems. Return an error
     * if none are preset, or you are unable to connect to them
     */
    return PMIX_SUCCESS;
}

static void lustre_finalize(void)
{
    pmix_output_verbose(2, pmix_pstrg_base_framework.framework_output, "pstrg: lustre finalize");

    /* ADD HERE:
     *
     * Disconnect from any Lustre systems to which you have connected
     */
}

static pmix_status_t query(pmix_query_t queries[], size_t nqueries, pmix_list_t *results,
                           pmix_pstrg_query_cbfunc_t cbfunc, void *cbdata)
{
    size_t n, m, k;
    char **sid, **mountpt;
    bool takeus;
    uid_t uid = UINT32_MAX;
    gid_t gid = UINT32_MAX;

    pmix_output_verbose(2, pmix_pstrg_base_framework.framework_output, "pstrg: lustre query");
    PMIX_HIDE_UNUSED_PARAMS(results, cbfunc, cbdata);
    
    /* just put something here so that Travis will pass its tests
     * because it treats warnings as errors, and wants to warn about
     * unused variables */
    sid = PMIx_Argv_split("foo,bar", ',');
    PMIx_Argv_free(sid);
    sid = NULL;
    mountpt = PMIx_Argv_split("foo,bar", ',');
    PMIx_Argv_free(mountpt);
    mountpt = NULL;

    /* now on to the real code */

    for (n = 0; n < nqueries; n++) {
        /* did they specify a storage type for this query? */
        takeus = true;
        for (k = 0; k < queries[n].nqual; k++) {
            if (0 == strcmp(queries[n].qualifiers[k].key, PMIX_STORAGE_TYPE)) {

                /* NOTE: I only included "lustre" as an accepted type, but we might
                 * want to consider other types as well - e.g., "parallel", "persistent",...) */

                if (NULL == strcasestr("lustre", queries[n].qualifiers[k].value.data.string)) {
                    /* they are not interested in us */
                    takeus = false;
                    ;
                }
                break;
            }
        }
        if (!takeus) {
            continue;
        }

        /* see if they want the list of storage systems - this doesn't take any
         * qualifiers */
        for (m = 0; NULL != queries[n].keys[m]; m++) {
            if (0 == strcmp(queries[n].keys[m], PMIX_QUERY_STORAGE_LIST)) {
                /* ADD HERE:
                 *
                 * Obtain a list of all available Lustre storage systems. The IDs
                 * we return should be those that we want the user to provide when
                 * asking about a specific Lustre system. Please get the corresponding
                 * mount pts for each identifier so I can track them for further queries.
                 */

                /* I will package the data for return once I see what Lustre provides */
                continue;
            }

            /* the remaining query keys all accept the storage ID and/or path qualifiers */
            sid = NULL;
            mountpt = NULL;
            for (k = 0; k < queries[n].nqual; k++) {
                if (0 == strcmp(queries[n].qualifiers[k].key, PMIX_STORAGE_ID)) {
                    /* there may be more than one (comma-delimited) storage ID, so
                     * split them into a NULL-terminated argv-type array */
                    if (NULL != sid) {
                        PMIx_Argv_free(sid);
                    }
                    sid = PMIx_Argv_split(queries[n].qualifiers[k].value.data.string, ',');
                } else if (0 == strcmp(queries[n].qualifiers[k].key, PMIX_STORAGE_PATH)) {
                    /* there may be more than one (comma-delimited) mount pt, so
                     * split them into a NULL-terminated argv-type array */
                    if (NULL != mountpt) {
                        PMIx_Argv_free(mountpt);
                    }
                    mountpt = PMIx_Argv_split(queries[n].qualifiers[k].value.data.string, ',');
                } else if (0 == strcmp(queries[n].qualifiers[k].key, PMIX_USERID)) {
                    uid = queries[n].qualifiers[k].value.data.uint32;
                } else if (0 == strcmp(queries[n].qualifiers[k].key, PMIX_GRPID)) {
                    gid = queries[n].qualifiers[k].value.data.uint32;
                }
            }

            /* just some nonsense to get rid of compile warnings until we fully implement */
            if (NULL != sid) {
                PMIx_Argv_free(sid);
            }
            if (NULL != mountpt) {
                PMIx_Argv_free(mountpt);
            }
            k = uid - gid;

            if (0 == strcmp(queries[n].keys[m], PMIX_STORAGE_CAPACITY_LIMIT)) {
                /* ADD HERE:
                 *
                 * Get the capacity of the Lustre storage systems. If they ask for
                 * a specific one(s), then get just those.
                 */

                /* I will package the data for return once I see what Lustre provides */
                continue;

            } else if (0 == strcmp(queries[n].keys[m], PMIX_STORAGE_OBJECT_LIMIT)) {
                /* ADD HERE:
                 *
                 * Get the limit on number of objects in the Lustre storage systems. If they ask for
                 * a specific one(s), then get just those.
                 */

                /* I will package the data for return once I see what Lustre provides */
                continue;

            } else if (0 == strcmp(queries[n].keys[m], PMIX_STORAGE_ID)) {
                /* don't worry about this one for now - once I see how to get the
                 * storage list and mount pts, I will construct the response here
                 *
                 * NOTE to self: could me a comma-delimited set of storage IDs, so
                 * return the mount pt for each of them
                 */

                /* I will package the data for return once I see what Lustre provides */
                continue;
            } else if (0 == strcmp(queries[n].keys[m], PMIX_STORAGE_PATH)) {
                /* don't worry about this one for now - once I see how to get the
                 * storage list and mount pts, I will construct the response here
                 *
                 * NOTE to self: could me a comma-delimited set of paths, so
                 * return the ID for each of them
                 */

                /* I will package the data for return once I see what Lustre provides */
                continue;
            }
        }
    }
    return PMIX_ERR_NOT_FOUND;
}
