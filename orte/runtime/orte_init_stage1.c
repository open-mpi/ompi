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
 * Copyright (c) 2006      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 *
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

#include "orte/orte_constants.h"

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
#include "orte/mca/rds/base/base.h"
#include "orte/mca/pls/base/base.h"
#include "orte/mca/rmgr/base/base.h"
#include "orte/mca/odls/base/base.h"

#include "orte/mca/rmaps/base/base.h"
#include "orte/mca/schema/base/base.h"
#include "orte/mca/smr/base/base.h"
#include "orte/util/univ_info.h"
#include "orte/util/proc_info.h"
#include "orte/util/session_dir.h"
#include "orte/util/sys_info.h"
#include "orte/util/universe_setup_file_io.h"

/* these are to be cleaned up for 2.0 */
#include "orte/mca/ras/base/ras_private.h"
#include "orte/mca/rmgr/base/rmgr_private.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/runtime_internal.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/params.h"

int orte_init_stage1(bool infrastructure)
{
    int ret;
    char *error = NULL;
    char *jobid_str = NULL;
    char *procid_str = NULL;
    char *contact_path = NULL;
    orte_jobid_t my_jobid;
    orte_cellid_t my_cellid;

    if (orte_initialized) {
        return ORTE_SUCCESS;
    }

    /* register handler for errnum -> string conversion */
    opal_error_register("ORTE", ORTE_ERR_BASE, ORTE_ERR_MAX, orte_err2str);

    /* Register all MCA Params */
    if (ORTE_SUCCESS != (ret = orte_register_params(infrastructure))) {
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
        error = "orte_dss_open";
        goto error;
    }

    /*
     * Open the name services to ensure access to local functions
     */
    if (ORTE_SUCCESS != (ret = orte_ns_base_open())) {
        error = "orte_ns_base_open";
        goto error;
    }

    /* Open the error manager to activate error logging - needs local name services */
    if (ORTE_SUCCESS != (ret = orte_errmgr_base_open())) {
        error = "orte_errmgr_base_open";
        goto error;
    }

    /*****   ERROR LOGGING NOW AVAILABLE *****/

    /*
     * Initialize the event library
    */
    if (ORTE_SUCCESS != (ret = opal_event_init())) {
        ORTE_ERROR_LOG(ret);
        error = "opal_event_init";
        goto error;
    }

    /*
     * Intialize the general progress engine
     */
    if (ORTE_SUCCESS != (ret = opal_progress_init())) {
        ORTE_ERROR_LOG(ret);
        error = "opal_progress_init";
        goto error;
    }

    /*
     * Internal startup
     */
    if (ORTE_SUCCESS != (ret = orte_wait_init())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_wait_init";
        goto error;
    }

    /*
     * Runtime Messaging Layer
     */
    if (ORTE_SUCCESS != (ret = orte_rml_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rml_base_open";
        goto error;
    }

    /*
     * Runtime Messaging Layer
     */
    if (ORTE_SUCCESS != (ret = orte_rml_base_select())) {
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
    if (ORTE_SUCCESS != (ret = orte_ns_base_select())) {
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

    /*
     * Now that we know for certain if we are an HNP and/or a daemon,
     * setup the resource management frameworks. This includes opening
     * and selecting the daemon launch framework - that framework "knows"
     * what to do if it isn't in a daemon, and everyone needs that framework
     * to at least register its datatypes.
     */
    if (ORTE_SUCCESS != (ret = orte_rds_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rds_base_open";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_rds_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rds_base_select";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_ras_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_ras_base_open";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_ras_base_find_available())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_ras_base_find_available";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_rmaps_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rmaps_base_open";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_rmaps_base_find_available())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rmaps_base_find_available";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_pls_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_pls_base_open";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_pls_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_pls_base_select";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_odls_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_odls_base_open";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_odls_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_odls_base_select";
        goto error;
    }
    
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
     * setup the state monitor
     */
    if (ORTE_SUCCESS != (ret = orte_smr_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_smr_base_open";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_smr_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_smr_base_select";
        goto error;
    }
    
    /*
     * setup the errmgr -- open has been done way before
     */
    if (ORTE_SUCCESS != (ret = orte_errmgr_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_errmgr_base_select";
        goto error;
    }
    
    /* if we are a singleton or the seed, setup the infrastructure for our job */
    
    if(orte_process_info.singleton || orte_process_info.seed) {
        char *site, *resource;
        orte_app_context_t *app;
        
        my_jobid = ORTE_PROC_MY_NAME->jobid;
        
        /* If there is no existing cellid, create one */
        my_cellid = 0; /* JJH Assertion/Repair until cellid's are fixed */
        ret = orte_ns.get_cell_info(my_cellid, &site, &resource);
        if (ORTE_ERR_NOT_FOUND == ret) {
            /* Create a new Cell ID */
            ret = orte_ns.create_cellid(&my_cellid, "unknown", orte_system_info.nodename);
            if (ORTE_SUCCESS != ret ) {
                ORTE_ERROR_LOG(ret);
                error = "orte_ns.create_cellid for singleton/seed";
                goto error;
            }
            
            if(my_cellid != 0) { /* JJH Assertion/Repair until cellid's are fixed */
                my_cellid = 0;
            }
        }
        else if (ORTE_SUCCESS != ret) {
            ORTE_ERROR_LOG(ret);
            error = "orte_ns.get_cell_inf for singleton/seedo";
            goto error;
        }
        
        my_cellid = ORTE_PROC_MY_NAME->cellid;
        
        /* set the rest of the infrastructure */
        app = OBJ_NEW(orte_app_context_t);
        app->app = strdup("unknown");
        app->num_procs = 1;
        if (ORTE_SUCCESS != (ret = orte_rmgr_base_put_app_context(my_jobid, &app, 1))) {
            ORTE_ERROR_LOG(ret);
            error = "orte_rmgr_base_put_app_context for singleton/seed";
            goto error;
        }
        OBJ_RELEASE(app);
        
        if (ORTE_SUCCESS != (ret = orte_rmgr.set_vpid_range(my_jobid,0,1))) {
            ORTE_ERROR_LOG(ret);
            error = "orte_rmgr.set_vpid_range for singleton/seed";
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
            opal_list_t attrs;
            opal_list_item_t *item;
            
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
                error = "singleton OBJ_NEW(orte_rds_cell_attr_t) for ORTE_RDS_NAME";
                ret = ORTE_ERR_OUT_OF_RESOURCE;
                goto error;
            }
            new_attr->keyval.key          = strdup(ORTE_RDS_NAME);
            new_attr->keyval.value = OBJ_NEW(orte_data_value_t);
            if (NULL == new_attr->keyval.value) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                error = "singleton OBJ_NEW(orte_data_value_t) for ORTE_RDS_NAME";
                ret = ORTE_ERR_OUT_OF_RESOURCE;
                goto error;
            }
            new_attr->keyval.value->type   = ORTE_STRING;
            new_attr->keyval.value->data   = strdup(ras_item->node_name);
            opal_list_append(&(rds_item->attributes), &new_attr->super);
            
            new_attr = OBJ_NEW(orte_rds_cell_attr_t);
            if (NULL == new_attr) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                error = "singleton OBJ_NEW(orte_rds_cell_attr_t) for ORTE_CELLID_KEY";
                ret = ORTE_ERR_OUT_OF_RESOURCE;
                goto error;
            }
            new_attr->keyval.key          = strdup(ORTE_CELLID_KEY);
            new_attr->keyval.value = OBJ_NEW(orte_data_value_t);
            if (NULL == new_attr->keyval.value) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                error = "singleton OBJ_NEW(orte_data_value_t) for ORTE_CELLID";
                ret = ORTE_ERR_OUT_OF_RESOURCE;
                goto error;
            }
            new_attr->keyval.value->type   = ORTE_CELLID;
            if (ORTE_SUCCESS != (ret = orte_dss.copy(&(new_attr->keyval.value->data), &(rds_item->cellid), ORTE_CELLID))) {
                ORTE_ERROR_LOG(ret);
                error = "singleton orte_dss.copy for ORTE_CELLID";
                goto error;
            }
            opal_list_append(&(rds_item->attributes), &new_attr->super);
            
            opal_list_append(&rds_single_host, &rds_item->super);
            
            /* Store into registry */
            ret = orte_rds.store_resource(&rds_single_host);
            if (ORTE_SUCCESS != ret ) {
                ORTE_ERROR_LOG(ret);
                error = "singleton orte_rds.store_resource";
                goto error;
            }
            
            /* JMS: This isn't quite right and should be fixed after
                1.0 -- we shouldn't be doing this manually here.  We
                should somehow be invoking a real RAS component to do
                this for us. */
            ret = orte_ras_base_node_insert(&single_host);
            if (ORTE_SUCCESS != ret ) {
                ORTE_ERROR_LOG(ret);
                error = "singleton orte_ras.node_insert";
                goto error;;
            }
            
            /* JMS: Same as above -- fix this after 1.0: force a
                selection so that orte_ras has initialized pointers in
                case anywhere else tries to use it.  This may end up
                putting a bunch more nodes on the node segment - e.g.,
                if you're in a SLURM allocation and you "./a.out",
                you'll end up with the localhost *and* all the other
                nodes in your allocation on the node segment -- which
                is probably fine */
            if (ORTE_SUCCESS != (ret = orte_ras.allocate_job(my_jobid, NULL))) {
                ORTE_ERROR_LOG(ret);
                error = "allocate for a singleton";
                goto error;
            }
            
            /* even though the map in this case is trivial, we still
             * need to call the RMAPS framework so the proper data
             * structures get set into the registry
             */
            OBJ_CONSTRUCT(&attrs, opal_list_t);
            if (ORTE_SUCCESS != (ret = orte_rmgr.add_attribute(&attrs, ORTE_RMAPS_NO_ALLOC_RANGE,
                                                               ORTE_UNDEF, NULL, ORTE_RMGR_ATTR_OVERRIDE))) {
                ORTE_ERROR_LOG(ret);
                error = "could not create attribute for map";
                goto error;
            }
            if (ORTE_SUCCESS != (ret = orte_rmaps.map_job(my_jobid, &attrs))) {
                ORTE_ERROR_LOG(ret);
                error = "map for a singleton";
                goto error;
            }
            while (NULL != (item = opal_list_remove_first(&attrs))) OBJ_RELEASE(item);
            OBJ_DESTRUCT(&attrs);
            
            /* cleanup data structs */
            OBJ_DESTRUCT(&single_host);
            OBJ_DESTRUCT(&rds_single_host);
        }
        
        if (ORTE_SUCCESS != (ret = orte_rmgr_base_proc_stage_gate_init(my_jobid))) {
            ORTE_ERROR_LOG(ret);
            error = "singleton orte_rmgr_base_proc_stage_gate_init";
            goto error;
        }
        
        /* set our state to LAUNCHED */
        if (ORTE_SUCCESS != (ret = orte_smr.set_proc_state(orte_process_info.my_name, ORTE_PROC_STATE_LAUNCHED, 0))) {
            ORTE_ERROR_LOG(ret);
            error = "singleton could not set launched state";
            goto error;
        }
    }

    /* initialize the rml module so it can open its interfaces - this
     * is needed so that we can get a uri for ourselves if we are an
     * HNP
     */
    if (ORTE_SUCCESS != (ret = orte_rml.init())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rml.init";
        goto error;
    }

    /* if I'm the seed, set the seed uri to be me! */
    if (orte_process_info.seed) {
        if (NULL != orte_universe_info.seed_uri) {
            free(orte_universe_info.seed_uri);
        }
        orte_universe_info.seed_uri = orte_rml.get_uri();
        /* and make sure that the daemon flag is NOT set so that
         * components unique to non-HNP orteds can be selected
         */
        orte_process_info.daemon = false;
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

        if (ORTE_SUCCESS != (ret = orte_write_universe_setup_file(contact_path, &orte_universe_info))) {
            if (orte_debug_flag) {
                opal_output(0, "[%lu,%lu,%lu] couldn't write setup file", ORTE_NAME_ARGS(orte_process_info.my_name));
            }
        } else if (orte_debug_flag) {
            opal_output(0, "[%lu,%lu,%lu] wrote setup file", ORTE_NAME_ARGS(orte_process_info.my_name));
        }
        free(contact_path);
    }


error:
    if (ret != ORTE_SUCCESS) {
        opal_show_help("help-orte-runtime",
                       "orte_init:startup:internal-failure",
                       true, error, ret);
    }

    return ret;
}
