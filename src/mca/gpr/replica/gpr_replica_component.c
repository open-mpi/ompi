/* -*- C -*-
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
#include "ompi_config.h"

#include <time.h>

#include "include/constants.h"

#include "threads/mutex.h"

#include "util/proc_info.h"
#include "util/output.h"
#include "util/bufpack.h"

#include "mca/mca.h"
#include "mca/base/mca_base_param.h"
#include "mca/oob/base/base.h"
#include "mca/gpr/base/base.h"
#include "gpr_replica.h"
#include "gpr_replica_internals.h"


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
	mca_gpr_replica_open,  /* module open */
	mca_gpr_replica_close /* module close */
    },
    {
	false /* checkpoint / restart */
    },
    mca_gpr_replica_init,    /* module init */
    mca_gpr_replica_finalize /* module shutdown */
};

/*
 * setup the function pointers for the module
 */
static mca_gpr_base_module_t mca_gpr_replica = {
    gpr_replica_get,
    gpr_replica_put,
    gpr_replica_delete_segment,
    gpr_replica_subscribe,
    gpr_replica_unsubscribe,
    gpr_replica_synchro,
    gpr_replica_cancel_synchro,
    gpr_replica_delete_object,
    gpr_replica_index,
    gpr_replica_test_internals,
    gpr_replica_rte_register,
    gpr_replica_rte_unregister
};

/*
 * Whether or not we allowed this component to be selected
 */
static bool initialized = false;


/*
 * globals needed within replica component
 */
mca_gpr_replica_t mca_gpr_replica_head;
ompi_list_t mca_gpr_replica_notify_request_tracker;
ompi_list_t mca_gpr_replica_callbacks;
mca_gpr_notify_id_t mca_gpr_replica_last_notify_id_tag;
ompi_list_t mca_gpr_replica_free_notify_id_tags;
int mca_gpr_replica_debug;
ompi_mutex_t mca_gpr_replica_mutex;


/* constructor - used to initialize state of keytable instance */
static void mca_gpr_replica_keytable_construct(mca_gpr_replica_keytable_t* keytable)
{
    keytable->token = NULL;
    keytable->key = MCA_GPR_REPLICA_KEY_MAX;
}

/* destructor - used to free any resources held by instance */
static void mca_gpr_replica_keytable_destructor(mca_gpr_replica_keytable_t* keytable)
{
}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
		   mca_gpr_replica_keytable_t,            /* type name */
		   ompi_list_item_t,              /* parent "class" name */
		   mca_gpr_replica_keytable_construct,    /* constructor */
		   mca_gpr_replica_keytable_destructor);  /* destructor */


/* constructor - used to initialize state of keylist instance */
static void mca_gpr_replica_keylist_construct(mca_gpr_replica_keylist_t* keylist)
{
    keylist->key = MCA_GPR_REPLICA_KEY_MAX;
}

/* destructor - used to free any resources held by instance */
static void mca_gpr_replica_keylist_destructor(mca_gpr_replica_keylist_t* keylist)
{
}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
		   mca_gpr_replica_keylist_t,           /* type name */
		   ompi_list_item_t,            /* parent "class" name */
		   mca_gpr_replica_keylist_construct,   /* constructor */
		   mca_gpr_replica_keylist_destructor); /* destructor */


/* constructor - used to initialize state of callback list instance */
static void mca_gpr_replica_callbacks_construct(mca_gpr_replica_callbacks_t* cb)
{
    cb->cb_func = NULL;
    cb->message = NULL;
    cb->requestor = NULL;
    cb->remote_idtag = 0;
    cb->user_tag = NULL;
}

/* destructor - used to free any resources held by instance */
static void mca_gpr_replica_callbacks_destructor(mca_gpr_replica_callbacks_t* cb)
{
    if (NULL != cb->requestor) {
	free(cb->requestor);
    }
}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
		   mca_gpr_replica_callbacks_t,           /* type name */
		   ompi_list_item_t,            /* parent "class" name */
		   mca_gpr_replica_callbacks_construct,   /* constructor */
		   mca_gpr_replica_callbacks_destructor); /* destructor */




/* constructor - used to initialize state of trigger list instance */
static void mca_gpr_replica_trigger_list_construct(mca_gpr_replica_trigger_list_t* trig)
{
    trig->synch_mode = OMPI_REGISTRY_SYNCHRO_MODE_NONE;
    trig->action = OMPI_REGISTRY_NOTIFY_NONE;
    trig->addr_mode = OMPI_REGISTRY_NONE;
    trig->num_keys = 0;
    trig->keys = NULL;
    trig->tokens = NULL;
    trig->trigger = 0;
    trig->count = 0;
    trig->id_tag = MCA_GPR_NOTIFY_ID_MAX;
}

/* destructor - used to free any resources held by instance */
static void mca_gpr_replica_trigger_list_destructor(mca_gpr_replica_trigger_list_t* trig)
{
    char **tok;
    int i;

    if (NULL != trig->keys) {
	free(trig->keys);
	trig->keys = NULL;
    }
    if (NULL != trig->tokens) {
	for (i=0, tok=trig->tokens; i< trig->num_keys; i++) {
	    free(*tok);
	    *tok = NULL;
	    tok++;
	}
	free(trig->tokens);
	trig->tokens = NULL;
    }
}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
		   mca_gpr_replica_trigger_list_t,           /* type name */
		   ompi_list_item_t,                 /* parent "class" name */
		   mca_gpr_replica_trigger_list_construct,   /* constructor */
		   mca_gpr_replica_trigger_list_destructor); /* destructor */


/* constructor - used to initialize state of replica list instance */
static void mca_gpr_replica_list_construct(mca_gpr_replica_list_t* replica)
{
    replica->replica = NULL;
}

/* destructor - used to free any resources held by instance */
static void mca_gpr_replica_list_destructor(mca_gpr_replica_list_t* replica)
{
    if (NULL != replica->replica) {
	free(replica->replica);
	replica->replica = NULL;
    }
}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
		   mca_gpr_replica_list_t,           /* type name */
		   ompi_list_item_t,                 /* parent "class" name */
		   mca_gpr_replica_list_construct,   /* constructor */
		   mca_gpr_replica_list_destructor); /* destructor */



/* constructor - used to initialize state of registry core instance */
static void mca_gpr_replica_core_construct(mca_gpr_replica_core_t* reg)
{
    reg->num_keys = 0;
    reg->keys = NULL;
    reg->object_size = 0;
    reg->object = NULL;
    OBJ_CONSTRUCT(&reg->replicas, ompi_list_t);
    reg->write_invalidate.invalidate = false;
    reg->write_invalidate.last_mod = 0;
    reg->write_invalidate.valid_replica = NULL;
}

/* destructor - used to free any resources held by instance */
static void mca_gpr_replica_core_destructor(mca_gpr_replica_core_t* reg)
{
    mca_gpr_replica_list_t *ptr;

    if (NULL != reg->keys) {
	free(reg->keys);
	reg->keys = NULL;
    }

    if (NULL != reg->object) {
	free(reg->object);
	reg->object = NULL;
    }

    while (NULL != (ptr = (mca_gpr_replica_list_t*)ompi_list_remove_first(&reg->replicas))) {
	OBJ_RELEASE(ptr);
    }
    OBJ_DESTRUCT(&reg->replicas);

    if (NULL != reg->write_invalidate.valid_replica) {
	free(reg->write_invalidate.valid_replica);
	reg->write_invalidate.valid_replica = NULL;
    }
}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
		   mca_gpr_replica_core_t,  /* type name */
		   ompi_list_item_t, /* parent "class" name */
		   mca_gpr_replica_core_construct, /* constructor */
		   mca_gpr_replica_core_destructor); /* destructor */



/* constructor - used to initialize state of segment instance */
static void mca_gpr_replica_segment_construct(mca_gpr_replica_segment_t* seg)
{
    seg->segment = 0;
    seg->lastkey = 0;
    OBJ_CONSTRUCT(&seg->registry_entries, ompi_list_t);
    OBJ_CONSTRUCT(&seg->triggers, ompi_list_t);
    OBJ_CONSTRUCT(&seg->keytable, ompi_list_t);
    OBJ_CONSTRUCT(&seg->freekeys, ompi_list_t);
}

/* destructor - used to free any resources held by instance */
static void mca_gpr_replica_segment_destructor(mca_gpr_replica_segment_t* seg)
{
    mca_gpr_replica_core_t *reg;
    mca_gpr_replica_trigger_list_t *tr;
    mca_gpr_replica_keytable_t *kt;
    mca_gpr_replica_keylist_t *kl;

    if (mca_gpr_replica_debug) {
	ompi_output(0, "entered segment destructor");
    }

    while (NULL != (reg = (mca_gpr_replica_core_t*)ompi_list_remove_first(&seg->registry_entries))) {
	OBJ_RELEASE(reg);
    }
    OBJ_DESTRUCT(&seg->registry_entries);

    while (NULL != (tr = (mca_gpr_replica_trigger_list_t*)ompi_list_remove_first(&seg->triggers))) {
	OBJ_RELEASE(tr);
    }
    OBJ_DESTRUCT(&seg->triggers);

    while (NULL != (kt = (mca_gpr_replica_keytable_t*)ompi_list_remove_first(&seg->keytable))) {
	OBJ_RELEASE(kt);
    }
    OBJ_DESTRUCT(&seg->keytable);

    while (NULL != (kl = (mca_gpr_replica_keylist_t*)ompi_list_remove_first(&seg->freekeys))) {
	OBJ_RELEASE(kl);
    }
    OBJ_DESTRUCT(&seg->freekeys);
}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
		   mca_gpr_replica_segment_t,  /* type name */
		   ompi_list_item_t, /* parent "class" name */
		   mca_gpr_replica_segment_construct, /* constructor */
		   mca_gpr_replica_segment_destructor); /* destructor */


/*
 * don't really need this function - could just put NULL in the above structure
 * Just holding the place in case we decide there is something we need to do
 */
int mca_gpr_replica_open(void)
{
    int id;

    id = mca_base_param_register_int("gpr", "replica", "debug", NULL, 0);
    mca_base_param_lookup_int(id, &mca_gpr_replica_debug);

    return OMPI_SUCCESS;
}

/*
 * ditto for this one
 */
int mca_gpr_replica_close(void)
{
    return OMPI_SUCCESS;
}

mca_gpr_base_module_t *mca_gpr_replica_init(bool *allow_multi_user_threads, bool *have_hidden_threads, int *priority)
{

    /* ompi_output(0, "entered replica init"); */
    /* If we are to host a replica, then we want to be selected, so do all the
       setup and return the module */

    if (NULL == ompi_process_info.gpr_replica) {
        int rc;

	/* Return a module (choose an arbitrary, positive priority --
	   it's only relevant compared to other ns components).  If
	   we're not the seed, then we don't want to be selected, so
	   return NULL. */

	*priority = 50;

	/* We allow multi user threads but don't have any hidden threads */

	*allow_multi_user_threads = true;
	*have_hidden_threads = false;

	/* setup the thread lock */
	OBJ_CONSTRUCT(&mca_gpr_replica_mutex, ompi_mutex_t);

	/* initialize the registry head */
	OBJ_CONSTRUCT(&mca_gpr_replica_head.registry, ompi_list_t);

	if (mca_gpr_replica_debug) {
	    ompi_output(0, "registry head setup");
	}

	/* initialize the global dictionary for segment id's */
	OBJ_CONSTRUCT(&mca_gpr_replica_head.segment_dict, ompi_list_t);
	OBJ_CONSTRUCT(&mca_gpr_replica_head.freekeys, ompi_list_t);
	mca_gpr_replica_head.lastkey = 0;

	if (mca_gpr_replica_debug) {
	    ompi_output(0, "global dict setup");
	}

	/* initialize the notify request tracker */
	OBJ_CONSTRUCT(&mca_gpr_replica_notify_request_tracker, ompi_list_t);
	mca_gpr_replica_last_notify_id_tag = 0;
	OBJ_CONSTRUCT(&mca_gpr_replica_free_notify_id_tags, ompi_list_t);

	if (mca_gpr_replica_debug) {
	    ompi_output(0, "req tracker setup");
	}

	/* initialize the callback list head */
	OBJ_CONSTRUCT(&mca_gpr_replica_callbacks, ompi_list_t);

 	/* issue the non-blocking receive */ 
	rc = mca_oob_recv_packed_nb(MCA_OOB_NAME_ANY, MCA_OOB_TAG_GPR, 0, mca_gpr_replica_recv, NULL);
	if(rc != OMPI_SUCCESS && rc != OMPI_ERR_NOT_IMPLEMENTED) { 
	    return NULL;
	}

	if (mca_gpr_replica_debug) {
	    ompi_output(0, "nb receive setup");
	}

	/* Return the module */

	initialized = true;
	return &mca_gpr_replica;
    } else {
	return NULL;
    }
}

/*
 * finalize routine
 */
int mca_gpr_replica_finalize(void)
{
    if (mca_gpr_replica_debug) {
	ompi_output(0, "finalizing gpr replica");
    }

    /*     mca_gpr_replica_segment_t *seg; */
    /*     mca_gpr_replica_keytable_t *kt; */
    /*     mca_gpr_replica_keylist_t *kl; */
    /*     mca_gpr_notify_request_tracker_t *tk; */
    /*     mca_gpr_idtag_list_t *id; */

    /*     /\* free all storage, but only if this component was initialized *\/ */

    /*     if (initialized) { */

    /* 	while (NULL != (seg = (mca_gpr_replica_segment_t*)ompi_list_remove_first(&mca_gpr_replica_head.registry))) { */
    /* 	    OBJ_RELEASE(seg); */
    /* 	} */
    /* 	OBJ_DESTRUCT(&mca_gpr_replica_head.registry); */

    /* 	while (NULL != (kt = (mca_gpr_replica_keytable_t*)ompi_list_remove_first(&mca_gpr_replica_head.segment_dict))) { */
    /* 	    OBJ_RELEASE(kt); */
    /* 	} */
    /* 	OBJ_DESTRUCT(&mca_gpr_replica_head.segment_dict); */

    /* 	while (NULL != (kl = (mca_gpr_replica_keylist_t*)ompi_list_remove_first(&mca_gpr_replica_head.freekeys))) { */
    /* 	    OBJ_RELEASE(kl); */
    /* 	} */
    /* 	OBJ_DESTRUCT(&mca_gpr_replica_head.freekeys); */


    /* 	while (NULL != (tk = (mca_gpr_notify_request_tracker_t*)ompi_list_remove_first(&mca_gpr_replica_notify_request_tracker))) { */
    /* 	    OBJ_RELEASE(tk); */
    /* 	} */
    /* 	OBJ_DESTRUCT(&mca_gpr_replica_notify_request_tracker); */


    /* 	while (NULL != (id = (mca_gpr_idtag_list_t*)ompi_list_remove_first(&mca_gpr_replica_free_notify_id_tags))) { */
    /* 	    OBJ_RELEASE(id); */
    /* 	} */
    /* 	OBJ_DESTRUCT(&mca_gpr_replica_free_notify_id_tags); */
    /* 	initialized = false; */
    /*     } */

    /* All done */

	mca_oob_recv_cancel(MCA_OOB_NAME_ANY, MCA_OOB_TAG_GPR);
    return OMPI_SUCCESS;
}

/* 
 * handle message from proxies
 */

void mca_gpr_replica_recv(int status, ompi_process_name_t* sender,
			  ompi_buffer_t buffer, int tag,
			  void* cbdata)
{
    ompi_buffer_t answer, error_answer, reg_buffer;
    ompi_registry_object_t *object;
    ompi_registry_object_size_t object_size;
    ompi_registry_mode_t mode;
    ompi_registry_notify_action_t action;
    ompi_registry_value_t *regval;
    ompi_list_t *returned_list;
    ompi_registry_internal_test_results_t *testval;
    ompi_registry_index_value_t *indexval;
    char **tokens, **tokptr;
    int32_t num_tokens, test_level, i, trigger, id_tag, num_procs;
    mca_gpr_notify_id_t local_idtag1, local_idtag2, start_idtag, end_idtag;
    pid_t pid;
    mca_gpr_cmd_flag_t command;
    char *segment, *contact_info, *nodename, *proc_name;
    int32_t response, synchro_mode;

    if (mca_gpr_replica_debug) {
	ompi_output(0, "gpr replica: received message");
    }

    if (OMPI_SUCCESS != ompi_buffer_init(&answer, 0)) {
	/* RHC -- not sure what to do if this fails */
    }

    if (OMPI_SUCCESS != ompi_unpack(buffer, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	goto RETURN_ERROR;
    }

    /******    DELETE SEGMENT    *****/
    if (MCA_GPR_DELETE_SEGMENT_CMD == command) {   /* got command to delete a segment */

	if (mca_gpr_replica_debug) {
	    ompi_output(0, "\tdelete segment cmd");
	}

	if (0 > ompi_unpack_string(buffer, &segment)) {
	    goto RETURN_ERROR;
	}

	response = (int32_t)gpr_replica_delete_segment(segment);

	if (OMPI_SUCCESS != ompi_pack(answer, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	    goto RETURN_ERROR;
	}
	if (OMPI_SUCCESS != ompi_pack(answer, &response, 1, OMPI_INT32)) {
	    goto RETURN_ERROR;
	}
	if (0 > mca_oob_send_packed(sender, answer, tag, 0)) {
	    /* RHC -- not sure what to do if the return send fails */
	}

	/*****    PUT    *****/
    } else if (MCA_GPR_PUT_CMD == command) {  /* got command to put object on registry */

	if (mca_gpr_replica_debug) {
	    ompi_output(0, "\tput cmd");
	}

	if (OMPI_SUCCESS != ompi_unpack(buffer, &mode, 1, MCA_GPR_OOB_PACK_MODE)) {
	    goto RETURN_ERROR;
	}

	if (0 > ompi_unpack_string(buffer, &segment)) {
	    goto RETURN_ERROR;
	}

	if (OMPI_SUCCESS != ompi_unpack(buffer, &num_tokens, 1, OMPI_INT32)) {
	    goto RETURN_ERROR;
	}

	if (0 >= num_tokens) {  /** no tokens provided - error for PUT */
	    goto RETURN_ERROR;
	}

	tokens = (char**)malloc((num_tokens+1)*sizeof(char*));

	tokptr = tokens;
	for (i=0; i<num_tokens; i++) {
	    if (0 > ompi_unpack_string(buffer, tokptr)) {
		goto RETURN_ERROR;
	    }
	    tokptr++;
	}
	*tokptr = NULL;

	if (OMPI_SUCCESS != ompi_unpack(buffer, &object_size, 1, MCA_GPR_OOB_PACK_OBJECT_SIZE)) {
	    goto RETURN_ERROR;
	}

	if (0 >= object_size) {  /* error condition - nothing to store */
	    goto RETURN_ERROR;
	}

	object = (ompi_registry_object_t *)malloc(object_size);
	if (OMPI_SUCCESS != ompi_unpack(buffer, object, object_size, OMPI_BYTE)) {
	    goto RETURN_ERROR;
	}

	response = (int32_t)gpr_replica_put(mode, segment, tokens, object, object_size);

	if (OMPI_SUCCESS != ompi_pack(answer, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	    goto RETURN_ERROR;
	}

	if (OMPI_SUCCESS != ompi_pack(answer, &response, 1, OMPI_INT32)) {
	    goto RETURN_ERROR;
	}
	if (0 > mca_oob_send_packed(sender, answer, tag, 0)) {
	    /* RHC -- not sure what to do if the return send fails */
	}

	/*****    GET    *****/
    } else if (MCA_GPR_GET_CMD == command) {  /* got command to put object on registry */

	if (mca_gpr_replica_debug) {
	    ompi_output(0, "\tget cmd");
	}

	if (OMPI_SUCCESS != ompi_unpack(buffer, &mode, 1, MCA_GPR_OOB_PACK_MODE)) {
	    goto RETURN_ERROR;
	}

	if (0 > ompi_unpack_string(buffer, &segment)) {
	    goto RETURN_ERROR;
	}

	if (OMPI_SUCCESS != ompi_unpack(buffer, &num_tokens, 1, OMPI_INT32)) {
	    goto RETURN_ERROR;
	}

	if (0 >= num_tokens) {  /* no tokens provided - wildcard case */
	    tokens = NULL;
	} else {  /* tokens provided */
	    tokens = (char**)malloc((num_tokens+1)*sizeof(char*));
	    tokptr = tokens;
	    for (i=0; i<num_tokens; i++) {
		if (0 > ompi_unpack_string(buffer, tokptr)) {
		    goto RETURN_ERROR;
		}
		tokptr++;
	    }
	    *tokptr = NULL;
	}

	returned_list = gpr_replica_get(mode, segment, tokens);

	if (OMPI_SUCCESS != ompi_pack(answer, (void*)&command, 1, MCA_GPR_OOB_PACK_CMD)) {
	    goto RETURN_ERROR;
	}

	response = (int32_t)ompi_list_get_size(returned_list);
	if (OMPI_SUCCESS != ompi_pack(answer, (void*)&response, 1, OMPI_INT32)) {
	    goto RETURN_ERROR;
	}

	if (0 < response) { /* don't send anything else back if the list is empty */
	    for (regval = (ompi_registry_value_t*)ompi_list_get_first(returned_list);
		 regval != (ompi_registry_value_t*)ompi_list_get_end(returned_list);
		 regval = (ompi_registry_value_t*)ompi_list_get_next(regval)) {  /* traverse the list */
		if (OMPI_SUCCESS != ompi_pack(answer, &regval->object_size, 1, MCA_GPR_OOB_PACK_OBJECT_SIZE)) {
		    goto RETURN_ERROR;
		}
		if (OMPI_SUCCESS != ompi_pack(answer, regval->object, regval->object_size, OMPI_BYTE)) {
		    goto RETURN_ERROR;
		}
	    }
	}
	if (0 > mca_oob_send_packed(sender, answer, tag, 0)) {
	    /* RHC -- not sure what to do if the return send fails */
	}

	/*****     DELETE OBJECT     *****/
    } else if (MCA_GPR_DELETE_OBJECT_CMD == command) {

	if (mca_gpr_replica_debug) {
	    ompi_output(0, "\tdelete object cmd");
	}

	if (OMPI_SUCCESS != ompi_unpack(buffer, &mode, 1, MCA_GPR_OOB_PACK_MODE)) {
	    goto RETURN_ERROR;
	}

	if (0 > ompi_unpack_string(buffer, &segment)) {
	    goto RETURN_ERROR;
	}

	if (OMPI_SUCCESS != ompi_unpack(buffer, &num_tokens, 1, OMPI_INT32)) {
	    goto RETURN_ERROR;
	}

	if (0 >= num_tokens) {  /* no tokens provided - wildcard case */
	    tokens = NULL;
	} else {  /* tokens provided */
	    tokens = (char**)malloc((num_tokens+1)*sizeof(char*));
	    tokptr = tokens;
	    for (i=0; i<num_tokens; i++) {
		if (0 > ompi_unpack_string(buffer, tokptr)) {
		    goto RETURN_ERROR;
		}
		tokptr++;
	    }
	    *tokptr = NULL;
	}

	response = (int32_t)gpr_replica_delete_object(mode, segment, tokens);

	if (OMPI_SUCCESS != ompi_pack(answer, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	    goto RETURN_ERROR;
	}

	if (OMPI_SUCCESS != ompi_pack(answer, &response, 1, OMPI_INT32)) {
	    goto RETURN_ERROR;
	}

	if (0 > mca_oob_send_packed(sender, answer, tag, 0)) {
	    /* RHC -- not sure what to do if the return send fails */
	}

	/*****     INDEX     *****/
    } else if (MCA_GPR_INDEX_CMD == command) {

	if (mca_gpr_replica_debug) {
	    ompi_output(0, "\tindex cmd");
	}

	if (OMPI_SUCCESS != ompi_unpack(buffer, &mode, 1, MCA_GPR_OOB_PACK_MODE)) {
	    goto RETURN_ERROR;
	}

	if (0 == mode) {  /* only want dict of segments */
	    segment = NULL;
	} else {
	    if (0 > ompi_unpack_string(buffer, &segment)) {
		goto RETURN_ERROR;
	    }
	}

	returned_list = gpr_replica_index(segment);

	if (OMPI_SUCCESS != ompi_pack(answer, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	    goto RETURN_ERROR;
	}

	response = (int32_t)ompi_list_get_size(returned_list);
	if (OMPI_SUCCESS != ompi_pack(answer, &response, 1, OMPI_INT32)) {
	    goto RETURN_ERROR;
	}

	if (0 < response) { /* don't send anything else back if the list is empty */
	    for (indexval = (ompi_registry_index_value_t*)ompi_list_get_first(returned_list);
		 indexval != (ompi_registry_index_value_t*)ompi_list_get_end(returned_list);
		 indexval = (ompi_registry_index_value_t*)ompi_list_get_next(indexval)) {  /* traverse the list */
		if (OMPI_SUCCESS != ompi_pack_string(answer, indexval->token)) {
		    goto RETURN_ERROR;
		}
	    }
	}

	if (0 > mca_oob_send_packed(sender, answer, tag, 0)) {
	    /* RHC -- not sure what to do if the return send fails */
	}

	/*****     SUBSCRIBE     *****/
    } else if (MCA_GPR_SUBSCRIBE_CMD == command) {

	if (mca_gpr_replica_debug) {
	    ompi_output(0, "\tsubscribe cmd");
	}

	if (OMPI_SUCCESS != ompi_unpack(buffer, &mode, 1, MCA_GPR_OOB_PACK_MODE)) {
	    goto RETURN_ERROR;
	}

	if (OMPI_SUCCESS != ompi_unpack(buffer, &action, 1, MCA_GPR_OOB_PACK_ACTION)) {
	    goto RETURN_ERROR;
	}

	if (0 > ompi_unpack_string(buffer, &segment)) {
	    goto RETURN_ERROR;
	}

	if (OMPI_SUCCESS != ompi_unpack(buffer, &num_tokens, 1, OMPI_INT32)) {
	    goto RETURN_ERROR;
	}

	if (0 < num_tokens) {  /* tokens provided */ 
	    tokens = (char**)malloc((num_tokens+1)*sizeof(char*));
	    tokptr = tokens;
	    for (i=0; i<num_tokens; i++) {
		if (0 > ompi_unpack_string(buffer, tokptr)) {
		    goto RETURN_ERROR;
		}
		tokptr++;
	    }
	    *tokptr = NULL;
	} else {  /* no tokens provided - wildcard case */
	    tokens = NULL;
	}

	if (OMPI_SUCCESS != ompi_unpack(buffer, &id_tag, 1, OMPI_INT32)) {
	    goto RETURN_ERROR;
	}

	/*******   LOCK    *****/
	OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);

	/* enter request on notify tracking system */
	local_idtag1 = gpr_replica_enter_notify_request(sender, id_tag, NULL, NULL);

	response = (int32_t)gpr_replica_subscribe_nl(mode, action, segment, tokens,
						     local_idtag1);

	OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
	/******     UNLOCK     ******/

	if (OMPI_SUCCESS != ompi_pack(answer, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	    goto RETURN_ERROR;
	}

	if (OMPI_SUCCESS != ompi_pack(answer, &response, 1, OMPI_INT32)) {
	    goto RETURN_ERROR;
	}

	if (0 > mca_oob_send_packed(sender, answer, tag, 0)) {
	    /* RHC -- not sure what to do if the return send fails */
	}

	/* process any resulting callbacks */
	gpr_replica_process_callbacks();

	/*****     UNSUBSCRIBE     *****/
    } else if (MCA_GPR_UNSUBSCRIBE_CMD == command) {

	if (mca_gpr_replica_debug) {
	    ompi_output(0, "\tunsubscribe cmd");
	}

	if (OMPI_SUCCESS != ompi_unpack(buffer, &mode, 1, MCA_GPR_OOB_PACK_MODE)) {
	    goto RETURN_ERROR;
	}

	if (OMPI_SUCCESS != ompi_unpack(buffer, &action, 1, MCA_GPR_OOB_PACK_ACTION)) {
	    goto RETURN_ERROR;
	}

	if (0 > ompi_unpack_string(buffer, &segment)) {
	    goto RETURN_ERROR;
	}

	if (OMPI_SUCCESS != ompi_unpack(buffer, &num_tokens, 1, OMPI_INT32)) {
	    goto RETURN_ERROR;
	}

	if (0 < num_tokens) {  /* tokens provided */ 
	    tokens = (char**)malloc((num_tokens+1)*sizeof(char*));
	    tokptr = tokens;
	    for (i=0; i<num_tokens; i++) {
		if (0 > ompi_unpack_string(buffer, tokptr)) {
		    goto RETURN_ERROR;
		}
		tokptr++;
	    }
	    *tokptr = NULL;
	} else {  /* no tokens provided - wildcard case */
	    tokens = NULL;
	}

	/*******   LOCK    *****/
	OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);

	response = (int32_t)gpr_replica_unsubscribe_nl(mode, action, segment, tokens);

	OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
	/******     UNLOCK     ******/

	if (OMPI_SUCCESS != ompi_pack(answer, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	    goto RETURN_ERROR;
	}

	if (OMPI_SUCCESS != ompi_pack(answer, &response, 1, OMPI_INT32)) {
	    goto RETURN_ERROR;
	}

	if (0 > mca_oob_send_packed(sender, answer, tag, 0)) {
	    /* RHC -- not sure what to do if the return send fails */
	}


	/*****     SYNCHRO     *****/
    } else if (MCA_GPR_SYNCHRO_CMD == command) {

	if (mca_gpr_replica_debug) {
	    ompi_output(0, "\tsynchro cmd");
	}

	if (OMPI_SUCCESS != ompi_unpack(buffer, &synchro_mode, 1, OMPI_INT32)) {
	    goto RETURN_ERROR;
	}

	if (OMPI_REGISTRY_SYNCHRO_MODE_NONE == synchro_mode) {
	    goto RETURN_ERROR;
	}

	if (OMPI_SUCCESS != ompi_unpack(buffer, &mode, 1, MCA_GPR_OOB_PACK_MODE)) {
	    goto RETURN_ERROR;
	}

	if (0 > ompi_unpack_string(buffer, &segment)) {
	    goto RETURN_ERROR;
	}

	if (OMPI_SUCCESS != ompi_unpack(buffer, &num_tokens, 1, OMPI_INT32)) {
	    goto RETURN_ERROR;
	}

	if (0 < num_tokens) {  /* tokens provided */ 
	    tokens = (char**)malloc((num_tokens+1)*sizeof(char*));
	    tokptr = tokens;
	    for (i=0; i<num_tokens; i++) {
		if (0 > ompi_unpack_string(buffer, tokptr)) {
		    goto RETURN_ERROR;
		}
		tokptr++;
	    }
	    *tokptr = NULL;
	} else {  /* no tokens provided - wildcard case, just count entries on segment */
	    tokens = NULL;
	}

	if (OMPI_SUCCESS != ompi_unpack(buffer, &trigger, 1, OMPI_INT32)) {
	    goto RETURN_ERROR;
	}

	if (OMPI_SUCCESS != ompi_unpack(buffer, &id_tag, 1, OMPI_INT32)) {
	    goto RETURN_ERROR;
	}


	/*******   LOCK    *****/
	OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);

	/* enter request on notify tracking system */
	local_idtag1 = gpr_replica_enter_notify_request(sender, id_tag, NULL, NULL);

	response = (int32_t)gpr_replica_synchro_nl(synchro_mode,
						   mode, segment, tokens,
						   trigger, local_idtag1);

	OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
	/******     UNLOCK     ******/

	if (OMPI_SUCCESS != ompi_pack(answer, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	    goto RETURN_ERROR;
	}

	if (OMPI_SUCCESS != ompi_pack(answer, &response, 1, OMPI_INT32)) {
	    goto RETURN_ERROR;
	}

	if (0 > mca_oob_send_packed(sender, answer, tag, 0)) {
	    /* RHC -- not sure what to do if the return send fails */
	}

	/* process any resulting callbacks */
	gpr_replica_process_callbacks();


	/*****     CANCEL SYNCHRO    *****/
    } else if (MCA_GPR_CANCEL_SYNCHRO_CMD == command) {

	if (mca_gpr_replica_debug) {
	    ompi_output(0, "\tcancel synchro cmd");
	}

	if (OMPI_SUCCESS != ompi_unpack(buffer, &synchro_mode, 1, OMPI_INT32)) {
	    goto RETURN_ERROR;
	}

	if (OMPI_REGISTRY_SYNCHRO_MODE_NONE == synchro_mode) {
	    goto RETURN_ERROR;
	}

	if (OMPI_SUCCESS != ompi_unpack(buffer, &mode, 1, MCA_GPR_OOB_PACK_MODE)) {
	    goto RETURN_ERROR;
	}

	if (0 > ompi_unpack_string(buffer, &segment)) {
	    goto RETURN_ERROR;
	}

	if (OMPI_SUCCESS != ompi_unpack(buffer, &num_tokens, 1, OMPI_INT32)) {
	    goto RETURN_ERROR;
	}

	if (0 < num_tokens) {  /* tokens provided */ 
	    tokens = (char**)malloc((num_tokens+1)*sizeof(char*));
	    tokptr = tokens;
	    for (i=0; i<num_tokens; i++) {
		if (0 > ompi_unpack_string(buffer, tokptr)) {
		    goto RETURN_ERROR;
		}
		tokptr++;
	    }
	    *tokptr = NULL;
	} else {  /* no tokens provided - wildcard case, just count entries on segment */
	    tokens = NULL;
	}

	if (OMPI_SUCCESS != ompi_unpack(buffer, &trigger, 1, OMPI_INT32)) {
	    goto RETURN_ERROR;
	}

	/*******   LOCK    *****/
	OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);

	response = (int32_t)gpr_replica_cancel_synchro_nl(synchro_mode, mode,
							  segment, tokens, trigger);

	OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
	/******     UNLOCK     ******/

	if (OMPI_SUCCESS != ompi_pack(answer, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	    goto RETURN_ERROR;
	}

	if (OMPI_SUCCESS != ompi_pack(answer, &response, 1, OMPI_INT32)) {
	    goto RETURN_ERROR;
	}

	if (0 > mca_oob_send_packed(sender, answer, tag, 0)) {
	    /* RHC -- not sure what to do if the return send fails */
	}


	/*****     REGISTER     *****/
    } else if (MCA_GPR_RTE_REGISTER_CMD == command) {

	if (0 > ompi_unpack_string(buffer, &contact_info)) {
	    goto RETURN_ERROR;
	}


	if (OMPI_SUCCESS != ompi_unpack(buffer, &num_procs, 1, OMPI_INT32)) {
	    goto RETURN_ERROR;
	}


	if (OMPI_SUCCESS != ompi_unpack(buffer, &pid, 1, OMPI_INT32)) {
	    goto RETURN_ERROR;
	}


	if (0 > ompi_unpack_string(buffer, &nodename)) {
	    goto RETURN_ERROR;
	}


	ompi_buffer_init(&reg_buffer, 0);
	ompi_pack_string(reg_buffer, contact_info);
	ompi_pack(reg_buffer, &pid, 1, OMPI_INT32);
	ompi_pack_string(buffer, nodename);


	if (OMPI_SUCCESS != ompi_unpack(buffer, &start_idtag, 1, OMPI_INT32)) {
	    goto RETURN_ERROR;
	}

	if (OMPI_SUCCESS != ompi_unpack(buffer, &end_idtag, 1, OMPI_INT32)) {
	    goto RETURN_ERROR;
	}


	/*******   LOCK    *****/
	OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);

	/* enter start request on notify tracking system */
	local_idtag1 = gpr_replica_enter_notify_request(sender, start_idtag, NULL, NULL);

	/* enter end request on notify tracking system */
	local_idtag2 = gpr_replica_enter_notify_request(sender, end_idtag, NULL, NULL);


	/* do registration */
	response = (int32_t)gpr_replica_rte_register_nl(contact_info, buffer,
							num_procs, local_idtag1, local_idtag2);


	OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
	/******     UNLOCK     ******/

	if (OMPI_SUCCESS != ompi_pack(answer, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	    goto RETURN_ERROR;
	}

	if (OMPI_SUCCESS != ompi_pack(answer, &response, 1, OMPI_INT32)) {
	    goto RETURN_ERROR;
	}

	if (0 > mca_oob_send_packed(sender, answer, tag, 0)) {
	    /* RHC -- not sure what to do if the return send fails */
	}

	/* process any resulting callbacks */
	gpr_replica_process_callbacks();



	/*****     UNREGISTER     *****/
    } else if (MCA_GPR_RTE_UNREGISTER_CMD == command) {

	if (0 > ompi_unpack_string(buffer, &proc_name)) {
	    goto RETURN_ERROR;
	}

	/*******   LOCK    *****/
	OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);

	response = (int32_t)gpr_replica_rte_unregister_nl(proc_name);

	OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
	/******     UNLOCK     ******/


	if (OMPI_SUCCESS != ompi_pack(answer, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	    goto RETURN_ERROR;
	}

	if (OMPI_SUCCESS != ompi_pack(answer, &response, 1, OMPI_INT32)) {
	    goto RETURN_ERROR;
	}

	if (0 > mca_oob_send_packed(sender, answer, tag, 0)) {
	    /* RHC -- not sure what to do if the return send fails */
	}

	/* process any resulting callbacks */
	gpr_replica_process_callbacks();



	/*****     TEST INTERNALS     *****/
    } else if (MCA_GPR_TEST_INTERNALS_CMD == command) {


	if ((OMPI_SUCCESS != ompi_unpack(buffer, &test_level, 1, OMPI_INT32)) ||
	    (0 > test_level)) {
	    goto RETURN_ERROR;
	}

	returned_list = gpr_replica_test_internals(test_level);

	if (OMPI_SUCCESS != ompi_pack(answer, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	    goto RETURN_ERROR;
	}

	response = (int32_t)ompi_list_get_size(returned_list);
	if (OMPI_SUCCESS != ompi_pack(answer, &response, 1, OMPI_INT32)) {
	    goto RETURN_ERROR;
	}

	if (0 < response) { /* don't send anything else back if the list is empty */
	    for (testval = (ompi_registry_internal_test_results_t*)ompi_list_get_first(returned_list);
		 testval != (ompi_registry_internal_test_results_t*)ompi_list_get_end(returned_list);
		 testval = (ompi_registry_internal_test_results_t*)ompi_list_get_next(testval)) {  /* traverse the list */
		if (OMPI_SUCCESS != ompi_pack_string(answer, testval->test)) {
		    goto RETURN_ERROR;
		}
		if (OMPI_SUCCESS != ompi_pack_string(answer, testval->message)) {
		    goto RETURN_ERROR;
		}
	    }
	}
	if (0 > mca_oob_send_packed(sender, answer, tag, 0)) {
	    /* RHC -- not sure what to do if the return send fails */
	}
 
	/****    UNRECOGNIZED COMMAND   ****/
    } else {  /* got an unrecognized command */
    RETURN_ERROR:
	ompi_buffer_init(&error_answer, 8);
	command = MCA_GPR_ERROR;
	ompi_pack(error_answer, (void*)&command, 1, MCA_GPR_OOB_PACK_CMD);
	mca_oob_send_packed(sender, error_answer, tag, 0);
	ompi_buffer_free(error_answer);
    }

    ompi_buffer_free(answer);

    /* reissue the non-blocking receive */
    mca_oob_recv_packed_nb(MCA_OOB_NAME_ANY, MCA_OOB_TAG_GPR, 0, mca_gpr_replica_recv, NULL);
}


void gpr_replica_remote_notify(ompi_process_name_t *recipient, int recipient_tag,
			       ompi_registry_notify_message_t *message)
{
    ompi_buffer_t msg;
    mca_gpr_cmd_flag_t command;
    int32_t num_items, i;
    ompi_registry_value_t *regval;
    char **tokptr;
    int recv_tag;

    if (mca_gpr_replica_debug) {
	ompi_output(0, "sending trigger message");
    }

    command = MCA_GPR_NOTIFY_CMD;
    recv_tag = MCA_OOB_TAG_GPR_NOTIFY;

    if (OMPI_SUCCESS != ompi_buffer_init(&msg, 0)) {
	return;
    }

    if (OMPI_SUCCESS != ompi_pack(msg, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	return;
    }

    i = (int32_t)recipient_tag;
    if (OMPI_SUCCESS != ompi_pack(msg, &i, 1, OMPI_INT32)) {
	return;
    }

    if (OMPI_SUCCESS != ompi_pack(msg, &message->trig_action, 1, MCA_GPR_OOB_PACK_ACTION)) {
	return;
    }

    if (OMPI_SUCCESS != ompi_pack(msg, &message->trig_synchro, 1, MCA_GPR_OOB_PACK_SYNCHRO_MODE)) {
	return;
    }

    
    num_items = (int32_t)ompi_list_get_size(&message->data);
    if (OMPI_SUCCESS != ompi_pack(msg, &num_items, 1, OMPI_INT32)) {
	return;
    }

    if (0 < num_items) { /* don't send anything else back if the list is empty */
	while (NULL != (regval = (ompi_registry_value_t*)ompi_list_remove_first(&message->data))) {
	    if (OMPI_SUCCESS != ompi_pack(msg, &regval->object_size, 1, MCA_GPR_OOB_PACK_OBJECT_SIZE)) {
		return;
	    }
	    if (OMPI_SUCCESS != ompi_pack(msg, regval->object, regval->object_size, OMPI_BYTE)) {
		return;
	    }
	    /* TSW - should we add */ 
	    /* OBJ_RELEASE(regval); */
	}
    }
    if (OMPI_SUCCESS != ompi_pack(msg, &message->num_tokens, 1, OMPI_INT32)) {
	return;
    }

    for (i=0, tokptr=message->tokens; i < message->num_tokens; i++, tokptr++) {
	if (OMPI_SUCCESS != ompi_pack_string(msg, *tokptr)) {
	    return;
	}
    }

    if (0 > mca_oob_send_packed(recipient, msg, recv_tag, 0)) {
	return;
    }

    ompi_buffer_free(msg);
    OBJ_RELEASE(message);
}
