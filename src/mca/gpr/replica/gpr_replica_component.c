/* -*- C -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
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

#include "class/ompi_object.h"
#include "util/output.h"
#include "util/proc_info.h"

#include "mca/rml/rml.h"

#include "gpr_replica.h"
#include "mca/gpr/replica/api_layer/gpr_replica_api.h"
#include "mca/gpr/replica/communications/gpr_replica_comm.h"


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
    orte_gpr_replica_deliver_notify_msg,
    /* ARITHMETIC OPERATIONS */
    orte_gpr_replica_increment_value,
    orte_gpr_replica_decrement_value,
    /* SUBSCRIBE OPERATIONS */
    orte_gpr_replica_subscribe,
    orte_gpr_replica_unsubscribe,
    /* COMPOUND COMMANDS */
    orte_gpr_replica_begin_compound_cmd,
    orte_gpr_replica_stop_compound_cmd,
    orte_gpr_replica_exec_compound_cmd,
    /* DIAGNOSTIC OPERATIONS */
    orte_gpr_replica_dump_all,
    orte_gpr_replica_dump_segments,
    orte_gpr_replica_dump_triggers,
    orte_gpr_base_dump_notify_msg,
    orte_gpr_base_dump_notify_data,
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

/*
 * CONSTRUCTORS AND DESTRUCTORS
 */

/*  SEGMENT */
/* constructor - used to initialize state of segment instance */
static void orte_gpr_replica_segment_construct(orte_gpr_replica_segment_t* seg)
{
    seg->name = NULL;
    seg->itag = ORTE_GPR_REPLICA_ITAG_MAX;
    
    orte_pointer_array_init(&(seg->dict), orte_gpr_replica_globals.block_size,
                            orte_gpr_replica_globals.max_size,
                            orte_gpr_replica_globals.block_size);

    orte_pointer_array_init(&(seg->containers), orte_gpr_replica_globals.block_size,
                            orte_gpr_replica_globals.max_size,
                            orte_gpr_replica_globals.block_size);

}

/* destructor - used to free any resources held by instance */
static void orte_gpr_replica_segment_destructor(orte_gpr_replica_segment_t* seg)
{
    int i;
    orte_gpr_replica_dict_t **dptr;
    orte_gpr_replica_container_t **cptr;
    
    if (NULL != seg->name) {
        free(seg->name);
    }

    if (NULL != seg->dict) {
        dptr = (orte_gpr_replica_dict_t**)((seg->dict)->addr);
        for (i=0; i < (seg->dict)->size; i++) {
            if (NULL != dptr[i]) {
                free(dptr[i]);
            }
        }
        OBJ_RELEASE(seg->dict);
    }
    
    if (NULL != seg->containers) {
        cptr = (orte_gpr_replica_container_t**)((seg->containers)->addr);
        for (i=0; i < (seg->containers)->size; i++) {
            if (NULL != cptr[i]) {
                OBJ_RELEASE(cptr[i]);
            }
        }
        OBJ_RELEASE(seg->containers);
    }
}

/* define instance of orte_gpr_replica_segment_t */
OBJ_CLASS_INSTANCE(
          orte_gpr_replica_segment_t,  /* type name */
          ompi_object_t, /* parent "class" name */
          orte_gpr_replica_segment_construct, /* constructor */
          orte_gpr_replica_segment_destructor); /* destructor */


/* CONTAINER */
/* constructor - used to initialize state of registry container instance */
static void orte_gpr_replica_container_construct(orte_gpr_replica_container_t* reg)
{
    orte_pointer_array_init(&(reg->itagvals), orte_gpr_replica_globals.block_size,
                            orte_gpr_replica_globals.max_size,
                            orte_gpr_replica_globals.block_size);

    OBJ_CONSTRUCT(&(reg->itaglist), orte_value_array_t);
    orte_value_array_init(&(reg->itaglist), sizeof(orte_gpr_replica_itag_t));

    reg->itags = NULL;
    reg->num_itags = 0;
    
}

/* destructor - used to free any resources held by instance */
static void orte_gpr_replica_container_destructor(orte_gpr_replica_container_t* reg)
{
    orte_gpr_replica_itagval_t **ptr;
    int i;

    if (NULL != reg->itags) {
         free(reg->itags);
    }

    if (NULL != reg->itagvals) {
        ptr = (orte_gpr_replica_itagval_t**)((reg->itagvals)->addr);
        for (i=0; i < (reg->itagvals)->size; i++) {
            if (NULL != ptr[i]) {
                OBJ_RELEASE(ptr[i]);
            }
        }
        OBJ_RELEASE(reg->itagvals);
    }

    OBJ_DESTRUCT(&(reg->itaglist));

}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
         orte_gpr_replica_container_t,  /* type name */
         ompi_object_t, /* parent "class" name */
         orte_gpr_replica_container_construct, /* constructor */
         orte_gpr_replica_container_destructor); /* destructor */


/* ITAG-VALUE PAIR */
/* constructor - used to initialize state of itagval instance */
static void orte_gpr_replica_itagval_construct(orte_gpr_replica_itagval_t* ptr)
{
    ptr->index = 0;
    ptr->itag = ORTE_GPR_REPLICA_ITAG_MAX;
    ptr->type = ORTE_NULL;
    (ptr->value).strptr = NULL;
}

/* destructor - used to free any resources held by instance */
static void orte_gpr_replica_itagval_destructor(orte_gpr_replica_itagval_t* ptr)
{
    if (ORTE_BYTE_OBJECT == ptr->type) {
        free(((ptr->value).byteobject).bytes);
    }
}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
         orte_gpr_replica_itagval_t,  /* type name */
         ompi_object_t, /* parent "class" name */
         orte_gpr_replica_itagval_construct, /* constructor */
         orte_gpr_replica_itagval_destructor); /* destructor */


/* COUNTERS */
/* constructor - used to initialize state of counter instance */
static void orte_gpr_replica_counter_construct(orte_gpr_replica_counter_t* cntr)
{
    cntr->seg = NULL;
    cntr->cptr = NULL;
    cntr->iptr = NULL;
    cntr->trigger_level = 0;
}

/* destructor - used to free any resources held by instance */
static void orte_gpr_replica_counter_destructor(orte_gpr_replica_counter_t* targ)
{
}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
         orte_gpr_replica_counter_t,           /* type name */
         ompi_object_t,                 /* parent "class" name */
         orte_gpr_replica_counter_construct,   /* constructor */
         orte_gpr_replica_counter_destructor); /* destructor */


/* SUBSCRIBED DATA */
/* constructor - used to initialize state of subscribed data instance */
static void orte_gpr_replica_subscribed_data_construct(orte_gpr_replica_subscribed_data_t* data)
{
    data->seg = NULL;
    data->addr_mode = 0;
    
    OBJ_CONSTRUCT(&(data->tokentags), orte_value_array_t);
    orte_value_array_init(&(data->tokentags), sizeof(orte_gpr_replica_itag_t));

    OBJ_CONSTRUCT(&(data->keytags), orte_value_array_t);
    orte_value_array_init(&(data->keytags), sizeof(orte_gpr_replica_itag_t));

    data->callback = NULL;
    data->user_tag = NULL;
}

/* destructor - used to free any resources held by instance */
static void orte_gpr_replica_subscribed_data_destructor(orte_gpr_replica_subscribed_data_t* data)
{

    OBJ_DESTRUCT(&(data->tokentags));
    
    OBJ_DESTRUCT(&(data->keytags));

}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
         orte_gpr_replica_subscribed_data_t,           /* type name */
         ompi_object_t,                 /* parent "class" name */
         orte_gpr_replica_subscribed_data_construct,   /* constructor */
         orte_gpr_replica_subscribed_data_destructor); /* destructor */


/* TRIGGERS */
/* constructor - used to initialize state of trigger instance */
static void orte_gpr_replica_trigger_construct(orte_gpr_replica_triggers_t* trig)
{
    trig->index = 0;
    trig->action = 0;
    
    trig->requestor = NULL;
    trig->remote_idtag = ORTE_GPR_NOTIFY_ID_MAX;
    
    trig->num_subscribed_data = 0;
    orte_pointer_array_init(&(trig->subscribed_data), 10,
                            orte_gpr_replica_globals.max_size, 10);

    trig->num_counters = 0;
    orte_pointer_array_init(&(trig->counters), 1, orte_gpr_replica_globals.max_size, 1);
}

/* destructor - used to free any resources held by instance */
static void orte_gpr_replica_trigger_destructor(orte_gpr_replica_triggers_t* trig)
{
    int i;
    orte_gpr_replica_subscribed_data_t **data;
    orte_gpr_replica_counter_t **cntrs;
    
    if (NULL != trig->requestor) {
        free(trig->requestor);
    }

    if (NULL != trig->subscribed_data) {
       data = (orte_gpr_replica_subscribed_data_t**)((trig->subscribed_data)->addr);
       for (i=0; i < (trig->subscribed_data)->size; i++) {
            if (NULL != data[i]) OBJ_RELEASE(data[i]);
       }
       OBJ_RELEASE(trig->subscribed_data);
    }
    
    if (NULL != trig->counters) {
        cntrs = (orte_gpr_replica_counter_t**)((trig->counters)->addr);
        for (i=0; i < (trig->counters)->size; i++) {
            if (NULL != cntrs[i]) OBJ_RELEASE(cntrs[i]);
        }
        OBJ_RELEASE(trig->counters);
    }
}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
         orte_gpr_replica_triggers_t,           /* type name */
         ompi_object_t,                 /* parent "class" name */
         orte_gpr_replica_trigger_construct,   /* constructor */
         orte_gpr_replica_trigger_destructor); /* destructor */


/* CALLBACKS */
/* constructor - used to initialize state of callback list instance */
static void orte_gpr_replica_callbacks_construct(orte_gpr_replica_callbacks_t* cb)
{
    cb->message = NULL;
    cb->requestor = NULL;
    cb->remote_idtag = 0;
}

/* destructor - used to free any resources held by instance */
static void orte_gpr_replica_callbacks_destructor(orte_gpr_replica_callbacks_t* cb)
{
    if (NULL != cb->requestor) {
        free(cb->requestor);
        cb->requestor = NULL;
    }
}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
         orte_gpr_replica_callbacks_t,           /* type name */
         ompi_list_item_t,            /* parent "class" name */
         orte_gpr_replica_callbacks_construct,   /* constructor */
         orte_gpr_replica_callbacks_destructor); /* destructor */


/* REPLICA LIST - NOT IMPLEMENTED YET! */
/* constructor - used to initialize state of replica list instance */
static void orte_gpr_replica_list_construct(orte_gpr_replica_list_t* replica)
{
    replica->replica = NULL;
}

/* destructor - used to free any resources held by instance */
static void orte_gpr_replica_list_destructor(orte_gpr_replica_list_t* replica)
{
    if (NULL != replica->replica) {
	   free(replica->replica);
	   replica->replica = NULL;
    }
}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
		   orte_gpr_replica_list_t,           /* type name */
		   ompi_list_item_t,                 /* parent "class" name */
		   orte_gpr_replica_list_construct,   /* constructor */
		   orte_gpr_replica_list_destructor); /* destructor */


/* WRITE INVALIDATE - NOT IMPLEMENTED YET! */
/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
		   orte_gpr_replica_write_invalidate_t,            /* type name */
		   ompi_list_item_t,                          /* parent "class" name */
		   NULL,    /* constructor */
		   NULL);  /* destructor */



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
    mca_base_param_lookup_int(id, &orte_gpr_replica_globals.max_size);

    id = mca_base_param_register_int("gpr", "replica", "blocksize", NULL,
                                     ORTE_GPR_REPLICA_BLOCK_SIZE);
    mca_base_param_lookup_int(id, &orte_gpr_replica_globals.block_size);

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
    /* If we are to host a replica, then we want to be selected, so do all the
       setup and return the module */

    if (NULL == orte_process_info.gpr_replica) {

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
        	OBJ_CONSTRUCT(&orte_gpr_replica_globals.wait_for_compound_mutex, ompi_mutex_t);
        	OBJ_CONSTRUCT(&orte_gpr_replica_globals.compound_cmd_condition, ompi_condition_t);
        
        	/* initialize the registry compound mode */
        	orte_gpr_replica_globals.compound_cmd_mode = false;
        	orte_gpr_replica_globals.exec_compound_cmd_mode = false;
        	orte_gpr_replica_globals.compound_cmd_waiting = 0;
        	orte_gpr_replica_globals.compound_cmd = NULL;
        
        	/* initialize the registry head */
            if (ORTE_SUCCESS != orte_pointer_array_init(&(orte_gpr_replica.segments),
                                    orte_gpr_replica_globals.block_size,
                                    orte_gpr_replica_globals.max_size,
                                    orte_gpr_replica_globals.block_size)) {
                return NULL;
            }
        
            if (ORTE_SUCCESS != orte_pointer_array_init(&(orte_gpr_replica.triggers),
                                    orte_gpr_replica_globals.block_size,
                                    orte_gpr_replica_globals.max_size,
                                    orte_gpr_replica_globals.block_size)) {
                return NULL;
            }
            
        	/* initialize the callback list head */
        	OBJ_CONSTRUCT(&orte_gpr_replica.callbacks, ompi_list_t);
        
        /* initialize the search arrays for temporarily storing search results */
        if (ORTE_SUCCESS != orte_pointer_array_init(&(orte_gpr_replica_globals.srch_cptr),
                                100, orte_gpr_replica_globals.max_size, 100)) {
            return NULL;
        }
        if (ORTE_SUCCESS != orte_pointer_array_init(&(orte_gpr_replica_globals.srch_ival),
                                100, orte_gpr_replica_globals.max_size, 100)) {
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
        return orte_rml.recv_buffer_nb(ORTE_RML_NAME_ANY, ORTE_RML_TAG_GPR, 0, orte_gpr_replica_recv, NULL);
    }
    return ORTE_SUCCESS;
}
        

/*
 * finalize routine
 */
int orte_gpr_replica_finalize(void)
{
    int i;
    orte_gpr_replica_segment_t** seg;
    orte_gpr_replica_triggers_t** trig;
    orte_gpr_replica_callbacks_t* cb;
    
    if (orte_gpr_replica_globals.debug) {
	    ompi_output(0, "finalizing gpr replica");
    }

    seg = (orte_gpr_replica_segment_t**)(orte_gpr_replica.segments)->addr;
    for (i=0; i < (orte_gpr_replica.segments)->size; i++) {
         if (NULL != seg[i]) {
             OBJ_RELEASE(seg[i]);
         }
    }
    
    trig = (orte_gpr_replica_triggers_t**)(orte_gpr_replica.triggers)->addr;
    for (i=0; i < (orte_gpr_replica.triggers)->size; i++) {
         if (NULL != trig[i]) {
             OBJ_RELEASE(trig[i]);
         }
    }
    
    while (NULL != (cb = (orte_gpr_replica_callbacks_t*)ompi_list_remove_first(&orte_gpr_replica.callbacks))) {
        OBJ_RELEASE(cb);
    }
    OBJ_DESTRUCT(&orte_gpr_replica.callbacks);


    /* clean up the globals */
    if (NULL != orte_gpr_replica_globals.compound_cmd) {
        OBJ_RELEASE(orte_gpr_replica_globals.compound_cmd);
    }
    
    /* All done */
    if (orte_gpr_replica_globals.isolate) {
        return ORTE_SUCCESS;
    }
    
	orte_rml.recv_cancel(ORTE_RML_NAME_ANY, ORTE_RML_TAG_GPR);
    return ORTE_SUCCESS;
}
