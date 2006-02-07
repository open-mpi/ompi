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
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** @file **/

#include "orte_config.h"

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "orte/include/orte_constants.h"

#include "opal/event/event.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/threads/mutex.h"
#include "opal/runtime/opal.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/os_path.h"
#include "opal/util/cmd_line.h"
#include "opal/util/malloc.h"

#include "orte/dss/dss.h"
#include "orte/mca/rml/base/base.h"
#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/iof/base/base.h"
#include "orte/mca/ns/base/base.h"
#include "orte/mca/sds/base/base.h"
#include "orte/mca/gpr/base/base.h"
#include "orte/mca/ras/base/base.h"
#include "orte/mca/ras/base/ras_base_node.h"
#include "orte/mca/rds/base/base.h"
#include "orte/mca/rmgr/base/base.h"
#include "orte/mca/rmaps/base/base.h"
#include "orte/mca/schema/base/base.h"
#include "orte/mca/soh/base/base.h"
#include "orte/util/univ_info.h"
#include "orte/util/proc_info.h"
#include "orte/util/session_dir.h"
#include "orte/util/sys_info.h"
#include "orte/util/universe_setup_file_io.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/runtime_internal.h"
#include "orte/runtime/orte_wait.h"

int orte_init_stage1(bool infrastructure)
{
    int ret;
    char *error = NULL;
    char *jobid_str = NULL;
    char *procid_str = NULL;
    char *contact_path = NULL;
    orte_jobid_t my_jobid;
    orte_cellid_t my_cellid;

    /* register handler for errnum -> string converstion */
    opal_error_register("ORTE", ORTE_ERR_BASE, ORTE_ERR_MAX, orte_err2str);

    /* Register all MCA Params */
    /* AWF - uh, is ORTE_ERROR_LOG really available yet? */
    if (ORTE_SUCCESS != (ret = orte_register_params(infrastructure))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_register_params";
        goto error;
    }

    /* Ensure the system_info structure is instantiated and initialized */
    if (ORTE_SUCCESS != (ret = orte_sys_info())) {
        error = "orte_sys_info";
        goto error;
    }

    /* Ensure the process info structure is instantiated and initialized */
    if (ORTE_SUCCESS != (ret = orte_proc_info())) {
        error = "orte_proc_info";
        goto error;
    }

    /* Ensure the universe_info structure is instantiated and initialized */
    if (ORTE_SUCCESS != (ret = orte_univ_info())) {
        error = "orte_univ_info";
        goto error;
    }

    /*
     * Initialize the data storage service.
     */
    if (ORTE_SUCCESS != (ret = orte_dss_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_dss_open";
        goto error;
    }

    /*
     * Open the name services to ensure access to local functions
     */
    if (OMPI_SUCCESS != (ret = orte_ns_base_open())) {
        error = "orte_ns_base_open";
        goto error;
    }

    /* Open the error manager to activate error logging - needs local name services */
    if (ORTE_SUCCESS != (ret = orte_errmgr_base_open())) {
        error = "orte_errmgr_base_open";
        goto error;
    }

    /*****   ERROR LOGGING NOW AVAILABLE *****/

    /* check for debug flag */
    if (0 > (ret =  mca_base_param_register_int("orte", "debug", NULL, NULL, 0))) {
        ORTE_ERROR_LOG(ret);
        error = "mca_base_param_register_int";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = mca_base_param_lookup_int(ret, &orte_debug_flag))) {
        ORTE_ERROR_LOG(ret);
        error = "mca_base_param_lookup_int";
        goto error;
    }

    /*
     * Initialize the event library
    */
    if (OMPI_SUCCESS != (ret = opal_event_init())) {
        ORTE_ERROR_LOG(ret);
        error = "opal_event_init";
        goto error;
    }

    /*
     * Intialize the general progress engine
     */
    if (OMPI_SUCCESS != (ret = opal_progress_init())) {
        ORTE_ERROR_LOG(ret);
        error = "opal_progress_init";
        goto error;
    }

    /*
     * Internal startup
     */
    if (OMPI_SUCCESS != (ret = orte_wait_init())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_wait_init";
        goto error;
    }

    /*
     * Runtime Messaging Layer
     */
    if (OMPI_SUCCESS != (ret = orte_rml_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rml_base_open";
        goto error;
    }

    /*
     * Runtime Messaging Layer
     */
    if (OMPI_SUCCESS != (ret = orte_rml_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rml_base_select";
        goto error;
    }

    /*
     * Registry
     */
    if (ORTE_SUCCESS != (ret = orte_gpr_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_gpr_base_open";
        goto error;
    }

    /*
     * Initialize schema utilities
     */
    if (ORTE_SUCCESS != (ret = orte_schema_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_schema_base_open";
        goto error;
    }

    /*
     * Initialize and select the Startup Discovery Service
     */
    if (ORTE_SUCCESS != (ret = orte_sds_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_sds_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_sds_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_sds_base_select";
        goto error;
    }

    /* Try to connect to the universe */
    if (ORTE_SUCCESS != (ret = orte_sds_base_contact_universe())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_sds_base_contact_universe";
        goto error;
    }

    /*
     * Name Server
     */
    if (OMPI_SUCCESS != (ret = orte_ns_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_ns_base_select";
        goto error;
    }

    /*
     * Registry
     */
    if (ORTE_SUCCESS != (ret = orte_gpr_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_gpr_base_select";
        goto error;
    }

    /* set contact info for ns/gpr */
    if(NULL != orte_process_info.ns_replica_uri) {
        orte_rml.set_uri(orte_process_info.ns_replica_uri);
    }
    if(NULL != orte_process_info.gpr_replica_uri) {
        orte_rml.set_uri(orte_process_info.gpr_replica_uri);
    }

    /* set my name and the names of the procs I was started with */
    if (ORTE_SUCCESS != (ret = orte_sds_base_set_name())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_sds_base_set_name";
        goto error;
    }

    /* all done with sds - clean up and call it a day */
    orte_sds_base_close();

    /* if I'm the seed, set the seed uri to be me! */
    if (orte_process_info.seed) {
        if (NULL != orte_universe_info.seed_uri) {
            free(orte_universe_info.seed_uri);
        }
        orte_universe_info.seed_uri = orte_rml.get_uri();
    }

    /* setup my session directory */
    if (ORTE_SUCCESS != (ret = orte_ns.get_jobid_string(&jobid_str, orte_process_info.my_name))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_ns.get_jobid_string";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_ns.get_vpid_string(&procid_str, orte_process_info.my_name))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_ns.get_vpid_string";
        goto error;
    }

    if (orte_debug_flag) {
        opal_output(0, "[%lu,%lu,%lu] setting up session dir with",
                    ORTE_NAME_ARGS(orte_process_info.my_name));
        if (NULL != orte_process_info.tmpdir_base) {
            opal_output(0, "\ttmpdir %s", orte_process_info.tmpdir_base);
        }
        opal_output(0, "\tuniverse %s", orte_universe_info.name);
        opal_output(0, "\tuser %s", orte_system_info.user);
        opal_output(0, "\thost %s", orte_system_info.nodename);
        opal_output(0, "\tjobid %s", jobid_str);
        opal_output(0, "\tprocid %s", procid_str);
    }
    if (ORTE_SUCCESS != (ret = orte_session_dir(true,
                                orte_process_info.tmpdir_base,
                                orte_system_info.user,
                                orte_system_info.nodename, NULL,
                                orte_universe_info.name,
                                jobid_str, procid_str))) {
        if (jobid_str != NULL) free(jobid_str);
        if (procid_str != NULL) free(procid_str);
        ORTE_ERROR_LOG(ret);
        error = "orte_session_dir";
        goto error;
    }
    if (NULL != jobid_str) {
        free(jobid_str);
    }
    if (NULL != procid_str) {
        free(procid_str);
    }

    /* Once the session directory location has been established, set
       the opal_output default file location to be in the
       proc-specific session directory. */
    opal_output_set_output_file_info(orte_process_info.proc_session_dir,
                                     "output-", NULL, NULL);

    /* if i'm the seed, get my contact info and write my setup file for others to find */
    if (orte_process_info.seed) {
        if (NULL != orte_universe_info.seed_uri) {
            free(orte_universe_info.seed_uri);
            orte_universe_info.seed_uri = NULL;
        }
        if (NULL == (orte_universe_info.seed_uri = orte_rml.get_uri())) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            error = "orte_rml_get_uri";
            ret = ORTE_ERR_NOT_FOUND;
            goto error;
        }
        contact_path = opal_os_path(false, orte_process_info.universe_session_dir,
                    "universe-setup.txt", NULL);
        if (orte_debug_flag) {
            opal_output(0, "[%lu,%lu,%lu] contact_file %s",
                        ORTE_NAME_ARGS(orte_process_info.my_name), contact_path);
        }

        if (OMPI_SUCCESS != (ret = orte_write_universe_setup_file(contact_path, &orte_universe_info))) {
            if (orte_debug_flag) {
                opal_output(0, "[%lu,%lu,%lu] couldn't write setup file", ORTE_NAME_ARGS(orte_process_info.my_name));
            }
        } else if (orte_debug_flag) {
            opal_output(0, "[%lu,%lu,%lu] wrote setup file", ORTE_NAME_ARGS(orte_process_info.my_name));
        }
        free(contact_path);
    }

    /*
     * setup the resource manager
     */

    if (ORTE_SUCCESS != (ret = orte_rmgr_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rmgr_base_open";
        goto error;
    }

    if (ORTE_SUCCESS != (ret = orte_rmgr_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rmgr_base_select";
        goto error;
    }

    /*
     * setup the state-of-health monitor
     */
    if (ORTE_SUCCESS != (ret = orte_soh_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_soh_base_open";
        goto error;
    }

    if (ORTE_SUCCESS != (ret = orte_soh_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_soh_base_select";
        goto error;
    }

     /* if we are a singleton or the seed, setup the infrastructure for our job */

    if(orte_process_info.singleton || orte_process_info.seed) {
        char *site, *resource;

        if (ORTE_SUCCESS != (ret = orte_ns.get_jobid(&my_jobid, orte_process_info.my_name))) {
            ORTE_ERROR_LOG(ret);
            error = "orte_ns.get_jobid";
            goto error;
        }

        /* If there is no existing cellid, create one */
        my_cellid = 0; /* JJH Assertion/Repair until cellid's are fixed */
        ret = orte_ns.get_cell_info(my_cellid, &site, &resource);
        if (ORTE_ERR_NOT_FOUND == ret) {
            /* Create a new Cell ID */
            ret = orte_ns.create_cellid(&my_cellid, "unknown", orte_system_info.nodename);
            if (ORTE_SUCCESS != ret ) {
                ORTE_ERROR_LOG(ret);
                error = "orte_ns.create_cellid";
                goto error;
            }

            if(my_cellid != 0) { /* JJH Assertion/Repair until cellid's are fixed */
                my_cellid = 0;
            }
        }
        else if (ORTE_SUCCESS != ret) {
            ORTE_ERROR_LOG(ret);
            error = "orte_ns.get_cell_info";
            goto error;
        }

        if (ORTE_SUCCESS != (ret = orte_ns.get_cellid(&my_cellid, orte_process_info.my_name))) {
            ORTE_ERROR_LOG(ret);
            error = "orte_ns.get_cellid";
            goto error;
        }

        if (orte_process_info.singleton) {
            /* setup a fake node structure - this is required to support
             * the MPI attributes function that is sitting on a trigger
             * waiting for info on available node slots. since we can't
             * really know that info for a singleton, we make the assumption
             * that the allocation is unity and place a structure on the
             * registry for it
             *
             * THIS ONLY SHOULD BE DONE FOR SINGLETONS - DO NOT DO IT
             * FOR ANY OTHER CASE
             */
            opal_list_t single_host, rds_single_host;
            orte_rds_cell_desc_t *rds_item;
            orte_rds_cell_attr_t *new_attr;
            orte_ras_node_t *ras_item;
            orte_ras_base_module_t *module;

            OBJ_CONSTRUCT(&single_host, opal_list_t);
            OBJ_CONSTRUCT(&rds_single_host, opal_list_t);
            ras_item = OBJ_NEW(orte_ras_node_t);
            rds_item = OBJ_NEW(orte_rds_cell_desc_t);
            if (NULL == ras_item || NULL == rds_item) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                error = "singleton node structure construction";
                ret = ORTE_ERR_OUT_OF_RESOURCE;
                goto error;
            }

            rds_item->site   = strdup("Singleton");
            rds_item->name   = strdup(orte_system_info.nodename);
            rds_item->cellid = my_cellid;

            /* Set up data structure for RAS item */
            ras_item->node_name        = strdup(rds_item->name);
            ras_item->node_arch        = strdup("unknown");
            ras_item->node_cellid      = rds_item->cellid;
            ras_item->node_slots_inuse = 0;
            ras_item->node_slots       = 1;

            opal_list_append(&single_host, &ras_item->super);

            /* Set up data structure for RDS item */
            new_attr = OBJ_NEW(orte_rds_cell_attr_t);
            if (NULL == new_attr) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                error = "OBJ_NEW(orte_rds_cell_attr_t) for ORTE_RDS_NAME";
                ret = ORTE_ERR_OUT_OF_RESOURCE;
                goto error;
            }
            new_attr->keyval.key          = strdup(ORTE_RDS_NAME);
            new_attr->keyval.value = OBJ_NEW(orte_data_value_t);
            if (NULL == new_attr->keyval.value) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                error = "OBJ_NEW(orte_data_value_t) for ORTE_RDS_NAME";
                ret = ORTE_ERR_OUT_OF_RESOURCE;
                goto error;
            }
            new_attr->keyval.value->type   = ORTE_STRING;
            new_attr->keyval.value->data   = strdup(ras_item->node_name);
            opal_list_append(&(rds_item->attributes), &new_attr->super);

            new_attr = OBJ_NEW(orte_rds_cell_attr_t);
            if (NULL == new_attr) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                error = "OBJ_NEW(orte_rds_cell_attr_t) for ORTE_CELLID_KEY";
                ret = ORTE_ERR_OUT_OF_RESOURCE;
                goto error;
            }
            new_attr->keyval.key          = strdup(ORTE_CELLID_KEY);
            new_attr->keyval.value = OBJ_NEW(orte_data_value_t);
            if (NULL == new_attr->keyval.value) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                error = "OBJ_NEW(orte_data_value_t) for ORTE_CELLID";
                ret = ORTE_ERR_OUT_OF_RESOURCE;
                goto error;
            }
            new_attr->keyval.value->type   = ORTE_CELLID;
            if (ORTE_SUCCESS != (ret = orte_dss.copy(&(new_attr->keyval.value->data), &(rds_item->cellid), ORTE_CELLID))) {
                ORTE_ERROR_LOG(ret);
                error = "orte_dss.copy for ORTE_CELLID";
                goto error;
            }
            opal_list_append(&(rds_item->attributes), &new_attr->super);

            opal_list_append(&rds_single_host, &rds_item->super);

            /* Store into registry */
            ret = orte_rds_base_store_resource(&rds_single_host);
            if (ORTE_SUCCESS != ret ) {
                ORTE_ERROR_LOG(ret);
                error = "orte_rds.store_resource";
                goto error;
            }

            /* JMS: This isn't quite right and should be fixed after
               1.0 -- we shouldn't be doing this manually here.  We
               should somehow be invoking a real RAS component to do
               this for us. */
            ret = orte_ras_base_node_insert(&single_host);
            if (ORTE_SUCCESS != ret ) {
                ORTE_ERROR_LOG(ret);
                error = "orte_ras.node_insert";
                goto error;;
            }

            /* JMS: Same as above -- fix this after 1.0: force a
               selection so that orte_ras has initialized pointers in
               case anywhere else tries to use it.  This may end up
               putting a bunch more nodes on the node segment (e.g.,
               if you're in a SLURM allocation and you "./a.out",
               you'll end up with the localhost *and* all the other
               nodes in your allocation on the node segment -- which
               is probably fine) */
            orte_ras_base_allocate(my_jobid, &module);
            if (NULL == module) {
                error = "orte_ras NULL module";
                goto error;
            }
            orte_ras = *module;

            OBJ_DESTRUCT(&single_host);
            OBJ_DESTRUCT(&rds_single_host);
        }

        /* set the rest of the infrastructure */
        if (ORTE_SUCCESS != (ret = orte_rmgr_base_set_job_slots(my_jobid,1))) {
            ORTE_ERROR_LOG(ret);
            error = "orte_rmgr_base_set_job_slots";
            goto error;
        }
        if (ORTE_SUCCESS != (ret = orte_rmaps_base_set_vpid_range(my_jobid,0,1))) {
            ORTE_ERROR_LOG(ret);
            error = "orte_rmaps_base_set_vpid_range";
            goto error;
        }
        if (ORTE_SUCCESS != (ret = orte_rmgr_base_proc_stage_gate_init(my_jobid))) {
            ORTE_ERROR_LOG(ret);
            error = "orte_rmgr_base_proc_stage_gate_init";
            goto error;
        }
    }

error:
    if (ret != ORTE_SUCCESS) {
        opal_show_help("help-orte-runtime",
                       "orte_init:startup:internal-failure",
                       true, error, ret);
    }

    return ret;
}
