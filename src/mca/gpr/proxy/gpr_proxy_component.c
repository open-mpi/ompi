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
 * The Open MPI General Purpose Registry - Proxy component
 *
 */

/*
 * includes
 */
#include "ompi_config.h"

#include "mca/gpr/base/base.h"

#include "gpr_proxy.h"


/*
 * Struct of function pointers that need to be initialized
 */
OMPI_COMP_EXPORT mca_gpr_base_component_t mca_gpr_proxy_component = {
    {
	MCA_GPR_BASE_VERSION_1_0_0,

	"proxy", /* MCA module name */
	1,  /* MCA module major version */
	0,  /* MCA module minor version */
	0,  /* MCA module release version */
	mca_gpr_proxy_open,  /* module open */
	mca_gpr_proxy_close /* module close */
    },
    {
	false /* checkpoint / restart */
    },
    mca_gpr_proxy_init,    /* module init */
    mca_gpr_proxy_finalize /* module shutdown */
};

/*
 * setup the function pointers for the module
 */
static mca_gpr_base_module_t mca_gpr_proxy = {
    mca_gpr_proxy_get,
    mca_gpr_proxy_put,
    mca_gpr_proxy_delete_segment,
    mca_gpr_proxy_subscribe,
    mca_gpr_proxy_unsubscribe,
    mca_gpr_proxy_synchro,
    mca_gpr_proxy_cancel_synchro,
    mca_gpr_proxy_delete_object,
    mca_gpr_proxy_index,
    mca_gpr_proxy_test_internals,
    mca_gpr_proxy_begin_compound_cmd,
    mca_gpr_proxy_stop_compound_cmd,
    mca_gpr_proxy_exec_compound_cmd,
    mca_gpr_proxy_dump,
    mca_gpr_proxy_silent_mode_on,
    mca_gpr_proxy_silent_mode_off,
    mca_gpr_proxy_notify_off,
    mca_gpr_proxy_notify_on,
    mca_gpr_proxy_assign_ownership,
    mca_gpr_proxy_triggers_active,
    mca_gpr_proxy_triggers_inactive,
    mca_gpr_proxy_get_startup_msg,
    mca_gpr_proxy_cleanup_job,
    mca_gpr_proxy_cleanup_proc,
    mca_gpr_proxy_deliver_notify_msg
};


/*
 * Whether or not we allowed this component to be selected
 */
static bool initialized = false;

/*
 * globals needed within proxy component
 */
ompi_process_name_t *mca_gpr_my_replica;
ompi_list_t mca_gpr_proxy_notify_request_tracker;
ompi_registry_notify_id_t mca_gpr_proxy_last_notify_id_tag;
ompi_list_t mca_gpr_proxy_free_notify_id_tags;
int mca_gpr_proxy_debug;
ompi_mutex_t mca_gpr_proxy_mutex;
bool mca_gpr_proxy_compound_cmd_mode;
ompi_buffer_t mca_gpr_proxy_compound_cmd;
ompi_mutex_t mca_gpr_proxy_wait_for_compound_mutex;
ompi_condition_t mca_gpr_proxy_compound_cmd_condition;
int mca_gpr_proxy_compound_cmd_waiting;
bool mca_gpr_proxy_silent_mode;


/* constructor - used to initialize notify message instance */
static void mca_gpr_proxy_notify_request_tracker_construct(mca_gpr_proxy_notify_request_tracker_t* req)
{
    req->callback = NULL;
    req->user_tag = NULL;
    req->local_idtag = OMPI_REGISTRY_NOTIFY_ID_MAX;
    req->remote_idtag = OMPI_REGISTRY_NOTIFY_ID_MAX;
    req->segment = NULL;
    req->action = OMPI_REGISTRY_NOTIFY_NONE;
}

/* destructor - used to free any resources held by instance */
static void mca_gpr_proxy_notify_request_tracker_destructor(mca_gpr_proxy_notify_request_tracker_t* req)
{
    if (NULL != req->segment) {
	free(req->segment);
    }
}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
		   mca_gpr_proxy_notify_request_tracker_t,            /* type name */
		   ompi_list_item_t,                          /* parent "class" name */
		   mca_gpr_proxy_notify_request_tracker_construct,    /* constructor */
		   mca_gpr_proxy_notify_request_tracker_destructor);  /* destructor */


/*
 * Open the component
 */
int mca_gpr_proxy_open(void)
{
    int id;

    id = mca_base_param_register_int("gpr", "proxy", "debug", NULL, 0);
    mca_base_param_lookup_int(id, &mca_gpr_proxy_debug);

    return OMPI_SUCCESS;
}

/*
 * Close the component
 */
int mca_gpr_proxy_close(void)
{
    return OMPI_SUCCESS;
}

mca_gpr_base_module_t* mca_gpr_proxy_init(bool *allow_multi_user_threads, bool *have_hidden_threads, int *priority)
{
    int rc;

    if (mca_gpr_proxy_debug) {
	ompi_output(0, "gpr_proxy_init called");
    }

    /* If we are NOT to host a replica, then we want to be selected, so do all
       the setup and return the module */
    if (NULL != ompi_process_info.gpr_replica) {

	if (mca_gpr_proxy_debug) {
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
	OBJ_CONSTRUCT(&mca_gpr_proxy_mutex, ompi_mutex_t);
	OBJ_CONSTRUCT(&mca_gpr_proxy_wait_for_compound_mutex, ompi_mutex_t);
	OBJ_CONSTRUCT(&mca_gpr_proxy_compound_cmd_condition, ompi_condition_t);

	/* initialize the registry compound mode */
	mca_gpr_proxy_compound_cmd_mode = false;
	mca_gpr_proxy_compound_cmd_waiting = 0;
	mca_gpr_proxy_compound_cmd = NULL;

	/* define the replica for us to use - get it from process_info */
	mca_gpr_my_replica = ompi_name_server.copy_process_name(ompi_process_info.gpr_replica);
	if (NULL == mca_gpr_my_replica) { /* can't function */
	    return NULL;
	}

	/* initialize the notify request tracker */
	OBJ_CONSTRUCT(&mca_gpr_proxy_notify_request_tracker, ompi_list_t);
	mca_gpr_proxy_last_notify_id_tag = 0;
	OBJ_CONSTRUCT(&mca_gpr_proxy_free_notify_id_tags, ompi_list_t);

	/* initialize any local variables */
	mca_gpr_proxy_silent_mode = false;

	/* issue the non-blocking receive */
	rc = mca_oob_recv_packed_nb(MCA_OOB_NAME_ANY, MCA_OOB_TAG_GPR_NOTIFY, 0, mca_gpr_proxy_notify_recv, NULL);
	if(rc != OMPI_SUCCESS && rc != OMPI_ERR_NOT_IMPLEMENTED) {
	    return NULL;
	}

	/* Return the module */

	initialized = true;
	return &mca_gpr_proxy;
    } else {
	return NULL;
    }
}

/*
 * finalize routine
 */
int mca_gpr_proxy_finalize(void)
{

    if (mca_gpr_proxy_debug) {
	ompi_output(0, "finalizing gpr proxy");
    }

    if (initialized) {
	initialized = false;
    }

    /* All done */
    mca_oob_recv_cancel(MCA_OOB_NAME_ANY, MCA_OOB_TAG_GPR_NOTIFY);
    return OMPI_SUCCESS;
}

/* 
 * handle notify messages from replicas
 */

void mca_gpr_proxy_notify_recv(int status, ompi_process_name_t* sender,
			       ompi_buffer_t buffer, int tag,
			       void* cbdata)
{
    char **tokptr;
    mca_gpr_cmd_flag_t command;
    uint32_t num_items;
    uint32_t i;
    ompi_registry_notify_id_t id_tag;
    ompi_registry_value_t *regval;
    ompi_registry_notify_message_t *message;
    bool found;
    mca_gpr_proxy_notify_request_tracker_t *trackptr;

    if (mca_gpr_proxy_debug) {
	ompi_output(0, "[%d,%d,%d] gpr proxy: received trigger message",
				OMPI_NAME_ARGS(*ompi_rte_get_self()));
    }

    message = OBJ_NEW(ompi_registry_notify_message_t);

    if ((OMPI_SUCCESS != ompi_unpack(buffer, &command, 1, MCA_GPR_OOB_PACK_CMD)) ||
	(MCA_GPR_NOTIFY_CMD != command)) {
	goto RETURN_ERROR;
    }

    if (0 > ompi_unpack_string(buffer, &message->segment)) {
	goto RETURN_ERROR;
    }

    if (OMPI_SUCCESS != ompi_unpack(buffer, &i, 1, OMPI_INT32)) {
	goto RETURN_ERROR;
    }
    message->owning_job = (mca_ns_base_jobid_t)i;

    if (OMPI_SUCCESS != ompi_unpack(buffer, &i, 1, OMPI_INT32)) {
	goto RETURN_ERROR;
    }
	id_tag = (ompi_registry_notify_id_t)i;
	
    if (mca_gpr_proxy_debug) {
	ompi_output(0, "[%d,%d,%d] trigger from segment %s id %d",
				OMPI_NAME_ARGS(*ompi_rte_get_self()), message->segment, (int)id_tag);
    }

    if (OMPI_SUCCESS != ompi_unpack(buffer, &message->trig_action, 1, MCA_GPR_OOB_PACK_ACTION)) {
	goto RETURN_ERROR;
    }

    if (OMPI_SUCCESS != ompi_unpack(buffer, &message->trig_synchro, 1, MCA_GPR_OOB_PACK_SYNCHRO_MODE)) {
	goto RETURN_ERROR;
    }

    if (OMPI_SUCCESS != ompi_unpack(buffer, &num_items, 1, OMPI_INT32)) {
	goto RETURN_ERROR;
    }

    for (i=0; i < num_items; i++) {
	regval = OBJ_NEW(ompi_registry_value_t);
	if (OMPI_SUCCESS != ompi_unpack(buffer, &regval->object_size, 1, MCA_GPR_OOB_PACK_OBJECT_SIZE)) {
	    OBJ_RELEASE(regval);
	    goto RETURN_ERROR;
	}
	if((regval->object = malloc(regval->object_size)) == NULL) {
	    OBJ_RELEASE(regval);
	    goto RETURN_ERROR;
	}
	if (OMPI_SUCCESS != ompi_unpack(buffer, regval->object, regval->object_size, OMPI_BYTE)) {
	    OBJ_RELEASE(regval);
	    goto RETURN_ERROR;
	}
	ompi_list_append(&message->data, &regval->item);
    }

    if (OMPI_SUCCESS != ompi_unpack(buffer, &message->num_tokens, 1, OMPI_INT32)) {
	goto RETURN_ERROR;
    }

    if(message->num_tokens > 0) {
        message->tokens = (char**)malloc(message->num_tokens*sizeof(char*));
        for (i=0, tokptr=message->tokens; i < message->num_tokens; i++, tokptr++) {
	    if (ompi_unpack_string(buffer, tokptr) < 0) {
		goto RETURN_ERROR;
	    }
        }
    } else {
        message->tokens = NULL;
    }

    OMPI_THREAD_LOCK(&mca_gpr_proxy_mutex);

    /* find the request corresponding to this notify */
    found = false;
    for (trackptr = (mca_gpr_proxy_notify_request_tracker_t*)ompi_list_get_first(&mca_gpr_proxy_notify_request_tracker);
         trackptr != (mca_gpr_proxy_notify_request_tracker_t*)ompi_list_get_end(&mca_gpr_proxy_notify_request_tracker);
         trackptr = (mca_gpr_proxy_notify_request_tracker_t*)ompi_list_get_next(trackptr)) {
     	if (mca_gpr_proxy_debug) {
     		ompi_output(0, "\tchecking idtag %d for segment %s\n", trackptr->local_idtag, trackptr->segment);
     	}
		if (trackptr->local_idtag == id_tag) {
            found = true;
            break;
        }
    }

    OMPI_THREAD_UNLOCK(&mca_gpr_proxy_mutex);

    if (!found) {  /* didn't find request */
        	ompi_output(0, "[%d,%d,%d] Proxy notification error - received request not found",
                        OMPI_NAME_ARGS(*ompi_rte_get_self()));
        	return;
    }

    /* process request */
    trackptr->callback(message, trackptr->user_tag);

    /* dismantle message and free memory */

 RETURN_ERROR:
    OBJ_RELEASE(message);

    /* reissue non-blocking receive */
    mca_oob_recv_packed_nb(MCA_OOB_NAME_ANY, MCA_OOB_TAG_GPR_NOTIFY, 0, mca_gpr_proxy_notify_recv, NULL);

}

