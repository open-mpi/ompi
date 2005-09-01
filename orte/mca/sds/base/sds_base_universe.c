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

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/mca/base/mca_base_param.h"
#include "opal/util/output.h"
#include "orte/include/orte_constants.h"
#include "orte/mca/sds/base/base.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/runtime.h"
#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/ns/base/base.h"

int
orte_sds_base_basic_contact_universe(void)
{
    int ret, rc, exit_if_not_exist;
    orte_universe_t univ;
    char *universe;
    pid_t pid;

    /* if we were NOT given registry and name service replicas (i.e., we
     * weren't told a universe contact point), check for some
     * existing universe to join */
    if (NULL == orte_process_info.ns_replica_uri || NULL == orte_process_info.gpr_replica_uri) {
        if (ORTE_SUCCESS == (ret = orte_universe_exists(&univ))) {
            /* copy universe info into our universe structure */
            orte_universe_info.name = univ.name;
            orte_universe_info.host = univ.host;
            orte_universe_info.uid = univ.uid;
            orte_universe_info.persistence = univ.persistence;
            orte_universe_info.scope = univ.scope;
            orte_universe_info.console = univ.console;
            orte_universe_info.seed_uri = univ.seed_uri;
            orte_universe_info.console_connected = univ.console_connected;
            orte_universe_info.scriptfile = univ.scriptfile;
            /* define the replica contact points */
            orte_process_info.ns_replica_uri = strdup(univ.seed_uri);
            orte_process_info.gpr_replica_uri = strdup(univ.seed_uri);
        } else {
            /* if an existing universe is not detected, check the
             * relevant MCA parameter to see if the caller wants
             * us to abort in this situation
             */
            if (0 > (rc =  mca_base_param_register_int("orte", "univ", "exist", NULL, 0))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            if (ORTE_SUCCESS != (rc = mca_base_param_lookup_int(rc, &exit_if_not_exist))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            if (exit_if_not_exist) {
                /* cleanup the subsystems that were already opened */
                orte_system_finalize();
                return ORTE_ERR_UNREACH;
            }
            if (ORTE_ERR_NOT_FOUND != ret) {
                /* if it exists but no contact could be established,
                 * we first check to see if this was a default universe
                 * or one that the user specifically requested. If
                 * it's the default, then we quietly generate a unique
                 * new name and start a new universe behind the scenes.
                 * If it was not the default (i.e., the user specifically
                 * directed us to a named universe), then we return an
                 * error code and abort.
                 */
                if (0 == strcmp(ORTE_DEFAULT_UNIVERSE, orte_universe_info.name)) {
                    /* default universe - generate unique name and proceed */
                    universe = strdup(orte_universe_info.name);
                    free(orte_universe_info.name);
                    orte_universe_info.name = NULL;
                    pid = getpid();
                    if (0 > asprintf(&orte_universe_info.name, "%s-%d", universe, (int)pid)) {
                        opal_output(0, "orte_init: failed to create unique universe name");
                        free(universe);
                        return ret;
                    }
                    free(universe);
                } else { /* user-specified name - abort */
                    opal_output(0, "orte_init: could not contact the specified universe name %s",
                        orte_universe_info.name);
                    return ORTE_ERR_UNREACH;
                }
            }
            orte_process_info.seed = true;
            /* since we are seed, ensure that all replica info is NULL'd */
            if (NULL != orte_process_info.ns_replica_uri) {
                free(orte_process_info.ns_replica_uri);
                orte_process_info.ns_replica_uri = NULL;
            }
            if (NULL != orte_process_info.ns_replica) {
                    free(orte_process_info.ns_replica);
                    orte_process_info.ns_replica = NULL;
            }

            if (NULL != orte_process_info.gpr_replica_uri) {
                free(orte_process_info.gpr_replica_uri);
                orte_process_info.gpr_replica_uri = NULL;
            }
            if (NULL != orte_process_info.gpr_replica) {
                    free(orte_process_info.gpr_replica);
                    orte_process_info.gpr_replica = NULL;
            }
        }
    }

    return ORTE_SUCCESS;
}


int
orte_sds_base_seed_set_name(void)
{
    int id, flag, rc;

    /* if we're a seed and we're not infrastructure, we're also a
       singleton.  So set the singleton flag in that case */
    id = mca_base_param_find("orte", NULL, "infrastructure");
    mca_base_param_lookup_int(id, &flag);
    if (!flag) {
        orte_process_info.singleton = true;
    }
    /* now need to create our name in a manner that puts our job info on the name service
     * tracker. This is necessary so that
     * functions like get_job_peers will work. Since we are the seed, these
     * functions will always return the proper jobid=0, vpid=0 values
     */
    if (ORTE_SUCCESS != (rc = orte_ns.create_my_name())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;
}
