/* -*- C -*-
 *
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
/** @file:
 *
 * The Open MPI General Purpose Registry - Replica component
 *
 */

/*
 * includes
 */
#include "orte_config.h"

#include "class/orte_bitmap.h"
#include "opal/class/opal_object.h"
#include "util/output.h"
#include "util/proc_info.h"

#include "mca/rml/rml.h"

#include "gpr_replica.h"
#include "mca/gpr/replica/api_layer/gpr_replica_api.h"
#include "mca/gpr/replica/communications/gpr_replica_comm.h"
#include "mca/errmgr/errmgr.h"


/*
 * Struct of function pointers that need to be initialized
 */
OMPI_COMP_EXPORT mca_gpr_base_component_t mca_gpr_replica_component = {
    {
	MCA_GPR_BASE_VERSION_1_0_0,

	"replica", /* MCA module name */
	1,  /* MCA module major version */
	0,  /* MCA module minor version */
	0,  /* MCA module release version */
	orte_gpr_replica_open,  /* module open */
	orte_gpr_replica_close /* module close */
    },
    {
	false /* checkpoint / restart */
    },
    orte_gpr_replica_init,    /* module init */
    orte_gpr_replica_finalize /* module shutdown */
};

/*
 * setup the function pointers for the module
 */
static orte_gpr_base_module_t orte_gpr_replica_module = {
    /* INIT */
    orte_gpr_replica_module_init,
    /* BLOCKING OPERATIONS */
    orte_gpr_replica_get,
    orte_gpr_replica_put,
    orte_gpr_replica_delete_entries,
    orte_gpr_replica_delete_segment,
    orte_gpr_replica_index,
    /* NON-BLOCKING OPERATIONS */
    orte_gpr_replica_get_nb,
    orte_gpr_replica_put_nb,
    orte_gpr_replica_delete_entries_nb,
    orte_gpr_replica_delete_segment_nb,
    orte_gpr_replica_index_nb,
    /* GENERAL OPERATIONS */
    orte_gpr_replica_preallocate_segment,
    orte_gpr_base_xfer_payload,
    /* ARITHMETIC OPERATIONS */
    orte_gpr_replica_increment_value,
    orte_gpr_replica_decrement_value,
    /* SUBSCRIBE OPERATIONS */
    orte_gpr_replica_subscribe,
    orte_gpr_replica_unsubscribe,
    orte_gpr_replica_cancel_trigger,
    /* COMPOUND COMMANDS */
    orte_gpr_replica_begin_compound_cmd,
    orte_gpr_replica_stop_compound_cmd,
    orte_gpr_replica_exec_compound_cmd,
    /* DIAGNOSTIC OPERATIONS */
    orte_gpr_replica_dump_all,
    orte_gpr_replica_dump_segments,
    orte_gpr_replica_dump_triggers,
    orte_gpr_replica_dump_subscriptions,
    orte_gpr_replica_dump_callbacks,
    orte_gpr_replica_dump_notify_msg,
    orte_gpr_replica_dump_notify_data,
    orte_gpr_replica_dump_value,
    /* CLEANUP OPERATIONS */
    orte_gpr_replica_cleanup_job,
    orte_gpr_replica_cleanup_proc
};

/*
 * Whether or not we allowed this component to be selected
 */
static bool initialized = false;


/*
 * globals needed within replica component
 */
orte_gpr_replica_t orte_gpr_replica;

orte_gpr_replica_globals_t orte_gpr_replica_globals;

/* instantiate the classes */
#include "mca/gpr/replica/gpr_replica_class_instances.h"

int orte_gpr_replica_open(void)
{
    int id, tmp;

    id = mca_base_param_register_int("gpr", "replica", "debug", NULL, 0);
    mca_base_param_lookup_int(id, &tmp);
    if (tmp) {
        orte_gpr_replica_globals.debug = true;
    } else {
        orte_gpr_replica_globals.debug = false;
    }
    
    id = mca_base_param_register_int("gpr", "replica", "maxsize", NULL,
                                     ORTE_GPR_REPLICA_MAX_SIZE);
    mca_base_param_lookup_int(id, &tmp);
    orte_gpr_replica_globals.max_size = (size_t)tmp;
    
    id = mca_base_param_register_int("gpr", "replica", "blocksize", NULL,
                                     ORTE_GPR_REPLICA_BLOCK_SIZE);
    mca_base_param_lookup_int(id, &tmp);
    orte_gpr_replica_globals.block_size = (size_t)tmp;
    
    id = mca_base_param_register_int("gpr", "replica", "isolate", NULL, 0);
    mca_base_param_lookup_int(id, &tmp);
    if (tmp) {
        orte_gpr_replica_globals.isolate = true;
    } else {
        orte_gpr_replica_globals.isolate = false;
    }

    return ORTE_SUCCESS;
}

/*
 * close function
 */
int orte_gpr_replica_close(void)
{
    return ORTE_SUCCESS;
}

orte_gpr_base_module_t *orte_gpr_replica_init(bool *allow_multi_user_threads, bool *have_hidden_threads, int *priority)
{
    int rc;
    
    /* If we are to host a replica, then we want to be selected, so do all the
       setup and return the module */

    if (NULL == orte_process_info.gpr_replica_uri) {

    	/* Return a module (choose an arbitrary, positive priority --
    	   it's only relevant compared to other ns components).  If
    	   we're not the seed, then we don't want to be selected, so
    	   return NULL. */
    
    	*priority = 50;
    
    	/* We allow multi user threads but don't have any hidden threads */
    
    	*allow_multi_user_threads = true;
    	*have_hidden_threads = false;
    
    	/* setup the thread locks and condition variables */
    	OBJ_CONSTRUCT(&orte_gpr_replica_globals.mutex, ompi_mutex_t);

    	/* initialize the registry head */
        if (ORTE_SUCCESS != (rc = orte_pointer_array_init(&(orte_gpr_replica.segments),
                                orte_gpr_replica_globals.block_size,
                                orte_gpr_replica_globals.max_size,
                                orte_gpr_replica_globals.block_size))) {
            ORTE_ERROR_LOG(rc);
            return NULL;
        }
        orte_gpr_replica.num_segs = 0;
        
        if (ORTE_SUCCESS != (rc = orte_pointer_array_init(&(orte_gpr_replica.triggers),
                                orte_gpr_replica_globals.block_size,
                                orte_gpr_replica_globals.max_size,
                                orte_gpr_replica_globals.block_size))) {
            ORTE_ERROR_LOG(rc);
            return NULL;
        }
        orte_gpr_replica.num_trigs = 0;
        
        if (ORTE_SUCCESS != (rc = orte_pointer_array_init(&(orte_gpr_replica.subscriptions),
                                orte_gpr_replica_globals.block_size,
                                orte_gpr_replica_globals.max_size,
                                orte_gpr_replica_globals.block_size))) {
            ORTE_ERROR_LOG(rc);
            return NULL;
        }
        orte_gpr_replica.num_subs = 0;
        
    	/* initialize the callback list head */
    	OBJ_CONSTRUCT(&orte_gpr_replica.callbacks, ompi_list_t);
        orte_gpr_replica.processing_callbacks = false;
    
        /* initialize the local subscription and trigger trackers */
        if (ORTE_SUCCESS != (rc = orte_pointer_array_init(
                                &(orte_gpr_replica_globals.local_subscriptions),
                                orte_gpr_replica_globals.block_size,
                                orte_gpr_replica_globals.max_size,
                                orte_gpr_replica_globals.block_size))) {
            ORTE_ERROR_LOG(rc);
            return NULL;
        }
        orte_gpr_replica_globals.num_local_subs = 0;
        orte_gpr_replica_globals.trig_cntr = 0;
        
        /* initialize the search arrays for temporarily storing search results */
        if (ORTE_SUCCESS != (rc = orte_pointer_array_init(&(orte_gpr_replica_globals.sub_ptrs),
                                100, orte_gpr_replica_globals.max_size, 100))) {
            ORTE_ERROR_LOG(rc);
            return NULL;
        }

        if (ORTE_SUCCESS != (rc = orte_pointer_array_init(&(orte_gpr_replica_globals.srch_cptr),
                                100, orte_gpr_replica_globals.max_size, 100))) {
            ORTE_ERROR_LOG(rc);
            return NULL;
        }
        orte_gpr_replica_globals.num_srch_cptr = 0;
        
        if (ORTE_SUCCESS != (rc = orte_pointer_array_init(&(orte_gpr_replica_globals.srch_ival),
                                100, orte_gpr_replica_globals.max_size, 100))) {
            ORTE_ERROR_LOG(rc);
            return NULL;
        }
        orte_gpr_replica_globals.num_srch_ival = 0;

        if (ORTE_SUCCESS != (rc = orte_pointer_array_init(&(orte_gpr_replica_globals.acted_upon),
                                100, orte_gpr_replica_globals.max_size, 100))) {
            ORTE_ERROR_LOG(rc);
            return NULL;
        }
        orte_gpr_replica_globals.num_acted_upon = 0;
        
        if (ORTE_SUCCESS != (rc = orte_bitmap_init(&(orte_gpr_replica_globals.srch_itag), 64))) {
            ORTE_ERROR_LOG(rc);
            return NULL;
        }
        
        if (orte_gpr_replica_globals.debug) {
            ompi_output(0, "nb receive setup");
        }
       
       	/* Return the module */
       
        initialized = true;
        return &orte_gpr_replica_module;
    } else {
    	return NULL;
    }
}


int orte_gpr_replica_module_init(void)
{
    /* issue the non-blocking receive */ 
    if (!orte_gpr_replica_globals.isolate) {
        int rc = orte_rml.recv_buffer_nb(ORTE_RML_NAME_ANY, ORTE_RML_TAG_GPR, 0, orte_gpr_replica_recv, NULL);
        if(rc < 0) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    return ORTE_SUCCESS;
}
        

/*
 * finalize routine
 */
int orte_gpr_replica_finalize(void)
{
    size_t i, j;
    orte_gpr_replica_segment_t** seg;
    orte_gpr_replica_trigger_t** trig;
    orte_gpr_replica_callbacks_t* cb;
    
    if (orte_gpr_replica_globals.debug) {
	    ompi_output(0, "finalizing gpr replica");
    }

    seg = (orte_gpr_replica_segment_t**)(orte_gpr_replica.segments)->addr;
    for (i=0, j=0; j < orte_gpr_replica.num_segs &&
                   i < (orte_gpr_replica.segments)->size; i++) {
         if (NULL != seg[i]) {
             j++;
             OBJ_RELEASE(seg[i]);
         }
    }
    OBJ_RELEASE(orte_gpr_replica.segments);
    
    trig = (orte_gpr_replica_trigger_t**)(orte_gpr_replica.triggers)->addr;
    for (i=0, j=0; j < orte_gpr_replica.num_trigs &&
                   i < (orte_gpr_replica.triggers)->size; i++) {
         if (NULL != trig[i]) {
             j++;
             OBJ_RELEASE(trig[i]);
         }
    }
    OBJ_RELEASE(orte_gpr_replica.triggers);
    
    while (NULL != (cb = (orte_gpr_replica_callbacks_t*)ompi_list_remove_first(&orte_gpr_replica.callbacks))) {
        OBJ_RELEASE(cb);
    }
    OBJ_DESTRUCT(&orte_gpr_replica.callbacks);


    /* clean up the globals */
    
    if (NULL != orte_gpr_replica_globals.srch_cptr) {
        OBJ_RELEASE(orte_gpr_replica_globals.srch_cptr);
    }
    
    if (NULL != orte_gpr_replica_globals.srch_ival) {
        OBJ_RELEASE(orte_gpr_replica_globals.srch_ival);
    }

    if (NULL != orte_gpr_replica_globals.acted_upon) {
        OBJ_RELEASE(orte_gpr_replica_globals.acted_upon);
    }

    /* All done */
    if (orte_gpr_replica_globals.isolate) {
        return ORTE_SUCCESS;
    }
    
	orte_rml.recv_cancel(ORTE_RML_NAME_ANY, ORTE_RML_TAG_GPR);
    return ORTE_SUCCESS;
}
