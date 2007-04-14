/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdlib.h>
#include <string.h>

#include "opal/util/show_help.h"
#include "orte/mca/ns/ns.h"
#include "ompi/runtime/ompi_cr.h"
#include "ompi/class/ompi_bitmap.h"
#include "ompi/mca/bml/bml.h"
#include "ompi/mca/bml/base/base.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/base.h"
#include "ompi/mca/bml/base/bml_base_endpoint.h" 
#include "ompi/mca/bml/base/bml_base_btl.h" 
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/pml/base/base.h"
#include "ompi/mca/pml/base/pml_base_module_exchange.h"
#include "orte/mca/smr/smr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/class/orte_proc_table.h" 
#include "ompi/proc/proc.h"

#include "bml_r2.h"
#include "bml_r2_ft.h"

static int mca_bml_r2_ft_readd_procs(int num_procs, ompi_proc_t** procs);

int mca_bml_r2_ft_event(int state) {
    ompi_proc_t** procs;
    size_t num_procs;
    size_t btl_idx;
    int ret, p;
    
    if(OPAL_CRS_CHECKPOINT == state) {
        /* Do nothing for now */
    }
    else if(OPAL_CRS_CONTINUE == state) {
        /* Since nothing in Checkpoint, we are fine here */
    }
    else if(OPAL_CRS_RESTART == state) {
        /*
         * Need to del_procs to clean out the stale association between
         * endpoints and former BTL modules.
         * - Similar to mca_bml_r2_del_btl
         */
        procs = ompi_proc_all(&num_procs);
        if(NULL == procs) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

#if 0
        {
        opal_list_item_t* w_item, *n_item;

        opal_output(0, "bml:r2: ft_event(Reboot): Del procs");
        for (w_item =  opal_list_get_first(&mca_btl_base_modules_initialized);
             w_item != opal_list_get_end(&mca_btl_base_modules_initialized);
             w_item =  n_item) {
            mca_btl_base_selected_module_t *sm = (mca_btl_base_selected_module_t *) w_item;

            n_item = opal_list_get_next(w_item);

#if 1
            mca_bml_r2_del_btl(sm->btl_module);
#else
            /* dont use this btl for any peers */
            for(p=0; p<num_procs; p++) {
                ompi_proc_t* proc = procs[p];
                mca_bml_r2_del_proc_btl(proc, sm->btl_module);
            }
#endif
        }
        }
#endif

        /*
         * Clean out the modex information since it is invalid now.
         */
        opal_output_verbose(10, ompi_cr_output,
                            "bml:r2: ft_event(Reboot): Reboot Modex information");
        if (OMPI_SUCCESS != (ret = mca_pml_base_modex_finalize())) {
            opal_output(0,
                        "bml:r2: ft_event(Restart): modex_finalize Failed %d",
                        ret);
            return ret;
        }

        for(p = 0; p < (int)num_procs; ++p) {
            OBJ_RELEASE(procs[p]->proc_modex);
            procs[p]->proc_modex = NULL;
        }

        if (OMPI_SUCCESS != (ret = mca_pml_base_modex_init())) {
            opal_output(0,
                        "bml:r2: ft_event(Restart): modex_init Failed %d",
                        ret);
            return ret;
        }
    }
    else if(OPAL_CRS_TERM == state ) {
        ;
    }
    else {
        ;
    }
    
    /*
     * Call ft_event in:
     * - BTL modules
     * - MPool modules
     *
     * These should be cleaning out stale state, and memory references in 
     * preparation for being shut down.
     */
    for(btl_idx = 0; btl_idx < mca_bml_r2.num_btl_modules; btl_idx++) {
        /*
         * Notify BTL
         */
        if( NULL != (mca_bml_r2.btl_modules[btl_idx])->btl_ft_event) {
            opal_output_verbose(10, ompi_cr_output,
                                "bml:r2: ft_event: Notify the %s BTL.\n",
                                (mca_bml_r2.btl_modules[btl_idx])->btl_component->btl_version.mca_component_name);
            if(OMPI_SUCCESS != (ret = (mca_bml_r2.btl_modules[btl_idx])->btl_ft_event(state) ) ) {
                continue;
            }
        }
        
        /*
         * Notify Mpool
         */
        if( NULL != (mca_bml_r2.btl_modules[btl_idx])->btl_mpool) {
            opal_output_verbose(10, ompi_cr_output,
                                "bml:r2: ft_event: Notify the %s MPool.\n",
                                (mca_bml_r2.btl_modules[btl_idx])->btl_mpool->mpool_component->mpool_version.mca_component_name);
            if(OMPI_SUCCESS != (ret = (mca_bml_r2.btl_modules[btl_idx])->btl_mpool->mpool_ft_event(state) ) ) {
                continue;
            }
        }
    }
    
    if(OPAL_CRS_CHECKPOINT == state) {
        ;
    }
    else if(OPAL_CRS_CONTINUE == state) {
        ;
    }
    else if(OPAL_CRS_RESTART == state) {

        mca_bml_r2.num_btl_modules  = 0;
        mca_bml_r2.num_btl_progress = 0;

        if( NULL != mca_bml_r2.btl_modules) {
            free( mca_bml_r2.btl_modules);
        }
        if( NULL != mca_bml_r2.btl_progress ) {
            free( mca_bml_r2.btl_progress);
        }

        opal_output_verbose(10, ompi_cr_output,
                            "bml:r2: ft_event(reboot): Reselect BTLs\n");

        /*
         * Close the BTLs
         *
         * Need to do this because we may have BTL components that were
         * unloaded in the first selection that may be available now.
         * Conversely we may have BTL components loaded now that
         * are not available now.
         */
        if( OMPI_SUCCESS != (ret = mca_btl_base_close())) {
            opal_output(0, "bml:r2: ft_event(Restart): Failed to close BTL framework\n");
            return ret;
        }

        /*
         * Re-open the BTL framework to get the full list of components.
         */
        if( OMPI_SUCCESS != (ret = mca_btl_base_open()) ) {
            opal_output(0, "bml:r2: ft_event(Restart): Failed to open BTL framework\n");
            return ret;
        }
        
        /*
         * Re-select the BTL components/modules
         * This will cause the BTL components to discover the available
         * network options on this machine, and post proper modex informaiton.
         */
        if( OMPI_SUCCESS != (ret = mca_btl_base_select(OMPI_ENABLE_PROGRESS_THREADS,
                                                       OMPI_ENABLE_MPI_THREADS) ) ) {
            opal_output(0, "bml:r2: ft_event(Restart): Failed to select in BTL framework\n");
            return ret;
        }

        /*
         * Re-exchange the Modex, and go through the stage gates
         */
        if (OMPI_SUCCESS != (ret = mca_pml_base_modex_exchange())) {
            opal_output(0,
                        "bml:r2: ft_event(Restart): modex_exchange Failed %d",
                        ret);
            return ret;
        }

        opal_output_verbose(10, ompi_cr_output,
                            "bml:r2: ft_event(Reboot): Enter Stage Gate 1");
        if (ORTE_SUCCESS != (ret = orte_smr.set_proc_state(orte_process_info.my_name,
                                                           ORTE_PROC_STATE_AT_STG1, 0))) {
            opal_output(0,
                        "bml:r2: ft_event(Restart): Stage Gate 1 Failed %d",
                        ret);
            return ret;
        }

        if (ORTE_SUCCESS != (ret = orte_rml.xcast(ORTE_PROC_MY_NAME->jobid, true,
                                                  NULL, orte_gpr.deliver_notify_msg))) {
            opal_output(0,
                        "bml:r2: ft_event(Restart): Stage Gate 1 Failed %d",
                        ret);
            return ret;
        }

        /*
         * Set the STAGE 2 State
         */
        opal_output_verbose(10, ompi_cr_output,
                            "bml:r2: ft_event(Reboot): Enter Stage Gate 2");
        if (ORTE_SUCCESS != (ret = orte_smr.set_proc_state(orte_process_info.my_name,
                                                           ORTE_PROC_STATE_AT_STG2, 0))) {
            opal_output(0,"bml:r2: ft_event(Restart): Stage Gate 1 Failed %d",
                        ret);
            return ret;
        }

        if (ORTE_SUCCESS != (ret = orte_rml.xcast(ORTE_PROC_MY_NAME->jobid, false,
                                                  NULL, orte_gpr.deliver_notify_msg))) {
            opal_output(0,"bml:r2: ft_event(Restart): Stage Gate 1 Failed %d",
                        ret);
            return ret;
        }

        /*
         * add-procs again.
         * Now that we have:
         * - A new list of valid BTL components and modules
         * - A full set of modex information to exchange
         */
        opal_output_verbose(10, ompi_cr_output,
                            "bml:r2: ft_event(Reboot): Re-add procs");
        if( OMPI_SUCCESS != (ret = mca_bml_r2_ft_readd_procs(num_procs, procs) ) ) {
            opal_output(0, "bml:r2: ft_event(Restart): Re-add Procs Failed %d",
                        ret);
            return ret;
        }

        if( NULL != procs ) {
            free(procs);
            procs = NULL;
        }
    }
    else if(OPAL_CRS_TERM == state ) {
        ;
    }
    else {
        ;
    }
    
    return OMPI_SUCCESS;
}

static int mca_bml_r2_ft_readd_procs(int num_procs, ompi_proc_t** procs)
{
    int ret;
    int p;
    struct mca_bml_base_endpoint_t** bml_endpoints = NULL;
    struct ompi_bitmap_t reachable;

    mca_bml_r2.btls_added = false;

    for(p = 0; p < num_procs; ++p) {
        OBJ_RELEASE(procs[p]->proc_bml);
        procs[p]->proc_bml = NULL;

        OBJ_RELEASE(procs[p]);
    }

    bml_endpoints = (struct mca_bml_base_endpoint_t**) malloc(num_procs * sizeof(struct mca_bml_base_endpoint_t*));
    if( NULL == bml_endpoints ) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    OBJ_CONSTRUCT(&reachable, ompi_bitmap_t);
    if( OMPI_SUCCESS != (ret = ompi_bitmap_init(&reachable, (int)num_procs) ) ) {
        opal_output(0, "bml:r2: readd_procs: Failed in bitmap init (%d)", ret);
        return ret;
    }

    if( OMPI_SUCCESS != (ret = mca_bml_r2_add_procs(num_procs, procs, bml_endpoints, &reachable)) ) {
        opal_output(0, "bml:r2: readd_procs: Failed in add_procs (%d)", ret);
        return ret;
    }


    for(p = 0; p < num_procs; ++p) {
        procs[p]->proc_pml = NULL;
    }

    if( NULL != bml_endpoints) {
        free(bml_endpoints);
    }

    OBJ_DESTRUCT(&reachable);

    return OMPI_SUCCESS;
}
