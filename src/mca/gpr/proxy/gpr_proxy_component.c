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
 * The Open MPI General Purpose Registry - Proxy component
 *
 */

/*
 * includes
 */
#include "orte_config.h"

#include "include/orte_constants.h"
#include "include/orte_types.h"
#include "dps/dps.h"
#include "util/output.h"
#include "util/proc_info.h"

#include "mca/ns/ns.h"
#include "mca/errmgr/errmgr.h"
#include "mca/oob/oob_types.h"
#include "mca/rml/rml.h"
#include "mca/errmgr/errmgr.h"

#include "gpr_proxy.h"


/*
 * Struct of function pointers that need to be initialized
 */
mca_gpr_base_component_t mca_gpr_proxy_component = {
    {
	MCA_GPR_BASE_VERSION_1_0_0,

	"proxy", /* MCA module name */
	1,  /* MCA module major version */
	0,  /* MCA module minor version */
	0,  /* MCA module release version */
	orte_gpr_proxy_open,  /* module open */
	orte_gpr_proxy_close /* module close */
    },
    {
	false /* checkpoint / restart */
    },
    orte_gpr_proxy_component_init,    /* module init */
    orte_gpr_proxy_finalize /* module shutdown */
};

/*
 * setup the function pointers for the module
 */
static orte_gpr_base_module_t orte_gpr_proxy = {
   /* INIT */
    orte_gpr_proxy_module_init,
   /* BLOCKING OPERATIONS */
    orte_gpr_proxy_get,
    orte_gpr_proxy_put,
    orte_gpr_proxy_delete_entries,
    orte_gpr_proxy_delete_segment,
    orte_gpr_proxy_index,
    /* NON-BLOCKING OPERATIONS */
    orte_gpr_proxy_get_nb,
    orte_gpr_proxy_put_nb,
    orte_gpr_proxy_delete_entries_nb,
    orte_gpr_proxy_delete_segment_nb,
    orte_gpr_proxy_index_nb,
    /* GENERAL OPERATIONS */
    orte_gpr_proxy_preallocate_segment,
    orte_gpr_proxy_deliver_notify_msg,
    /* ARITHMETIC OPERATIONS */
    orte_gpr_proxy_increment_value,
    orte_gpr_proxy_decrement_value,
    /* SUBSCRIBE OPERATIONS */
    orte_gpr_proxy_subscribe,
    orte_gpr_proxy_unsubscribe,
    /* COMPOUND COMMANDS */
    orte_gpr_proxy_begin_compound_cmd,
    orte_gpr_proxy_stop_compound_cmd,
    orte_gpr_proxy_exec_compound_cmd,
    /* DIAGNOSTIC OPERATIONS */
    orte_gpr_proxy_dump_all,
    orte_gpr_proxy_dump_segments,
    orte_gpr_proxy_dump_triggers,
    orte_gpr_base_dump_notify_msg,
    orte_gpr_base_dump_notify_data,
    /* CLEANUP OPERATIONS */
    orte_gpr_proxy_cleanup_job,
    orte_gpr_proxy_cleanup_proc
};


/*
 * Whether or not we allowed this component to be selected
 */
static bool initialized = false;

/*
 * globals needed within proxy component
 */
orte_gpr_proxy_globals_t orte_gpr_proxy_globals;

/* SUBSCRIBER */
/* constructor - used to initialize subscriber instance */
static void orte_gpr_proxy_subscriber_construct(orte_gpr_proxy_subscriber_t* req)
{
    req->callback = NULL;
    req->user_tag = NULL;
    req->index = 0;
}

/* destructor - used to free any resources held by instance */
static void orte_gpr_proxy_subscriber_destructor(orte_gpr_proxy_subscriber_t* req)
{
}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
            orte_gpr_proxy_subscriber_t,            /* type name */
            ompi_object_t,                          /* parent "class" name */
            orte_gpr_proxy_subscriber_construct,    /* constructor */
            orte_gpr_proxy_subscriber_destructor);  /* destructor */


/* NOTIFY TRACKER */
/* constructor - used to initialize notify message instance */
static void orte_gpr_proxy_notify_tracker_construct(orte_gpr_proxy_notify_tracker_t* req)
{
    req->remote_idtag = ORTE_GPR_NOTIFY_ID_MAX;
    
    orte_pointer_array_init(&(req->callbacks), orte_gpr_proxy_globals.block_size,
                        orte_gpr_proxy_globals.max_size,
                        orte_gpr_proxy_globals.block_size);

}

/* destructor - used to free any resources held by instance */
static void orte_gpr_proxy_notify_tracker_destructor(orte_gpr_proxy_notify_tracker_t* req)
{
}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
		   orte_gpr_proxy_notify_tracker_t,            /* type name */
		   ompi_object_t,                          /* parent "class" name */
		   orte_gpr_proxy_notify_tracker_construct,    /* constructor */
		   orte_gpr_proxy_notify_tracker_destructor);  /* destructor */


/*
 * Open the component
 */
int orte_gpr_proxy_open(void)
{
    int id, tmp;

    id = mca_base_param_register_int("gpr", "proxy", "debug", NULL, 0);
    mca_base_param_lookup_int(id, &tmp);
    if (tmp) {
        orte_gpr_proxy_globals.debug = true;
    } else {
        orte_gpr_proxy_globals.debug = false;
    }

    id = mca_base_param_register_int("gpr", "proxy", "maxsize", NULL,
                                     ORTE_GPR_PROXY_MAX_SIZE);
    mca_base_param_lookup_int(id, &orte_gpr_proxy_globals.max_size);

    id = mca_base_param_register_int("gpr", "proxy", "blocksize", NULL,
                                     ORTE_GPR_PROXY_BLOCK_SIZE);
    mca_base_param_lookup_int(id, &orte_gpr_proxy_globals.block_size);

    return ORTE_SUCCESS;
}

/*
 * Close the component
 */
int orte_gpr_proxy_close(void)
{
    return ORTE_SUCCESS;
}

orte_gpr_base_module_t*
orte_gpr_proxy_component_init(bool *allow_multi_user_threads, bool *have_hidden_threads,
                            int *priority)
{
    if (orte_gpr_proxy_globals.debug) {
	    ompi_output(0, "gpr_proxy_init called");
    }

    /* If we are NOT to host a replica, then we want to be selected, so do all
       the setup and return the module */
    if (NULL != orte_process_info.gpr_replica) {

	if (orte_gpr_proxy_globals.debug) {
	    ompi_output(0, "gpr_proxy_init: proxy selected");
	}

	/* Return a module (choose an arbitrary, positive priority --
	   it's only relevant compared to other ns components).  If
	   we're not the seed, then we don't want to be selected, so
	   return NULL. */

	*priority = 10;

	/* We allow multi user threads but don't have any hidden threads */

	*allow_multi_user_threads = true;
	*have_hidden_threads = false;

	/* setup thread locks and condition variable */
	OBJ_CONSTRUCT(&orte_gpr_proxy_globals.mutex, ompi_mutex_t);
	OBJ_CONSTRUCT(&orte_gpr_proxy_globals.wait_for_compound_mutex, ompi_mutex_t);
	OBJ_CONSTRUCT(&orte_gpr_proxy_globals.compound_cmd_condition, ompi_condition_t);

	/* initialize the registry compound mode */
	orte_gpr_proxy_globals.compound_cmd_mode = false;
	orte_gpr_proxy_globals.compound_cmd_waiting = 0;
	orte_gpr_proxy_globals.compound_cmd = NULL;

	/* initialize the notify request tracker */
        if (ORTE_SUCCESS != orte_pointer_array_init(&(orte_gpr_proxy_globals.notify_tracker),
                                orte_gpr_proxy_globals.block_size,
                                orte_gpr_proxy_globals.max_size,
                                orte_gpr_proxy_globals.block_size)) {
            return NULL;
        }
        initialized = true;
        return &orte_gpr_proxy;
    } else {
        return NULL;
    }
}

int orte_gpr_proxy_module_init(void)
{
    /* issue the non-blocking receive */
    int rc;
    rc = orte_rml.recv_buffer_nb(ORTE_RML_NAME_ANY, ORTE_RML_TAG_GPR_NOTIFY, 0, orte_gpr_proxy_notify_recv, NULL);
    if(rc < 0) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    return ORTE_SUCCESS;
}


/*
 * finalize routine
 */
int orte_gpr_proxy_finalize(void)
{

    if (orte_gpr_proxy_globals.debug) {
	   ompi_output(0, "finalizing gpr proxy");
    }

    if (initialized) {
	   initialized = false;
    }

    /* All done */
    orte_rml.recv_cancel(ORTE_RML_NAME_ANY, ORTE_RML_TAG_GPR_NOTIFY);
    return ORTE_SUCCESS;
}

/* 
 * handle notify messages from replicas
 */

void orte_gpr_proxy_notify_recv(int status, orte_process_name_t* sender,
			       orte_buffer_t *buffer, orte_rml_tag_t tag,
			       void* cbdata)
{
    orte_gpr_cmd_flag_t command;
    orte_gpr_notify_id_t id_tag;
    orte_gpr_notify_data_t **data;
    orte_gpr_proxy_notify_tracker_t *trackptr;
    orte_gpr_proxy_subscriber_t **subs;
    size_t n;
    int rc;
    int32_t cnt, i, j;

    if (orte_gpr_proxy_globals.debug) {
	    ompi_output(0, "[%d,%d,%d] gpr proxy: received trigger message",
				ORTE_NAME_ARGS(orte_process_info.my_name));
    }

    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dps.unpack(buffer, &command, &n, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        goto RETURN_ERROR;
    }

	if (ORTE_GPR_NOTIFY_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
	    goto RETURN_ERROR;
    }

    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dps.unpack(buffer, &id_tag, &n, ORTE_GPR_NOTIFY_ID))) {
        ORTE_ERROR_LOG(rc);
	    goto RETURN_ERROR;
    }
	
    /* locate request corresponding to this message */
    trackptr = (orte_gpr_proxy_notify_tracker_t*)((orte_gpr_proxy_globals.notify_tracker)->addr[id_tag]);
    if (NULL == trackptr) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        goto RETURN_ERROR;
    }
    
    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dps.unpack(buffer, &cnt, &n, ORTE_INT32))) {
        ORTE_ERROR_LOG(rc);
        goto RETURN_ERROR;
    }

    if(cnt > 0) {
        /* allocate space for the array */
        data = (orte_gpr_notify_data_t**)malloc(cnt * sizeof(orte_gpr_notify_data_t*));
        if (NULL == data) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            goto RETURN_ERROR;
        }

        n = cnt;
        if (ORTE_SUCCESS != (rc = orte_dps.unpack(buffer, data, &n, ORTE_GPR_NOTIFY_DATA))) {
            ORTE_ERROR_LOG(rc);
            goto RETURN_ERROR;
        }
    

        for (i=0; i < cnt; i++) {
           /* locate the data callback */
           subs = (orte_gpr_proxy_subscriber_t**)((trackptr->callbacks)->addr);
           for (j=0; j < (trackptr->callbacks)->size; j++) {
               if (NULL != subs[j] && subs[j]->index == data[i]->cb_num) {
                   /* process request */
                   subs[j]->callback(data[i], subs[j]->user_tag);
                   break;
               }
           }
       }
    }
    /* dismantle message and free memory */

 RETURN_ERROR:

    /* reissue non-blocking receive */
    orte_rml.recv_buffer_nb(ORTE_RML_NAME_ANY, ORTE_RML_TAG_GPR_NOTIFY, 0, orte_gpr_proxy_notify_recv, NULL);

}

