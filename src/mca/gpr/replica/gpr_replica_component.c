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
#include "util/proc_info.h"
#include "util/output.h"
#include "util/pack.h"

#include "mca/mca.h"
#include "mca/base/mca_base_param.h"
#include "mca/oob/base/base.h"
#include "mca/gpr/base/base.h"
#include "gpr_replica.h"
#include "gpr_replica_internals.h"


/*
 * Struct of function pointers that need to be initialized
 */
mca_gpr_base_component_t mca_gpr_replica_component = {
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
    gpr_replica_delete_object,
    gpr_replica_index,
    gpr_replica_test_internals
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
mca_gpr_notify_id_t mca_gpr_replica_last_notify_id_tag;
ompi_list_t mca_gpr_replica_free_notify_id_tags;
int mca_gpr_replica_debug;


/* constructor - used to initialize state of keytable instance */
static void mca_gpr_replica_keytable_construct(mca_gpr_replica_keytable_t* keytable)
{
    keytable->token = NULL;
    keytable->key = MCA_GPR_REPLICA_KEY_MAX;
}

/* destructor - used to free any resources held by instance */
static void mca_gpr_replica_keytable_destructor(mca_gpr_replica_keytable_t* keytable)
{
    mca_gpr_replica_keytable_t *keyptr;

    if (mca_gpr_replica_debug) {
	ompi_output(0, "entered keytable destructor");
    }

    while (NULL != (keyptr = (mca_gpr_replica_keytable_t*)ompi_list_remove_first((ompi_list_t*)keytable))) {
	if (NULL != keyptr->token) {
	    free(keyptr->token);
	}
	OBJ_RELEASE(keyptr);
    }
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


/* constructor - used to initialize state of trigger list instance */
static void mca_gpr_replica_trigger_list_construct(mca_gpr_replica_trigger_list_t* trig)
{
    trig->synch_mode = OMPI_REGISTRY_SYNCHRO_MODE_NONE;
    trig->action = OMPI_REGISTRY_NOTIFY_NONE;
    trig->addr_mode = OMPI_REGISTRY_NONE;
    trig->keys = NULL;
    trig->trigger = 0;
    trig->count = 0;
    trig->id_tag = MCA_GPR_NOTIFY_ID_MAX;
}

/* destructor - used to free any resources held by instance */
static void mca_gpr_replica_trigger_list_destructor(mca_gpr_replica_trigger_list_t* trig)
{
    if (NULL != trig->keys) {
	free(trig->keys);
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
    }

    if (NULL != reg->object) {
	free(reg->object);
    }

    while (NULL != (ptr = (mca_gpr_replica_list_t*)ompi_list_remove_first(&reg->replicas))) {
	OBJ_RELEASE(ptr);
    }
    OBJ_DESTRUCT(&reg->replicas);

    if (NULL != reg->write_invalidate.valid_replica) {
	free(reg->write_invalidate.valid_replica);
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
    /* If we're the seed, then we want to be selected, so do all the
       setup and return the module */

    if (ompi_process_info.seed) {
        int rc;

	/* Return a module (choose an arbitrary, positive priority --
	   it's only relevant compared to other ns components).  If
	   we're not the seed, then we don't want to be selected, so
	   return NULL. */

	*priority = 50;

	/* We allow multi user threads but don't have any hidden threads */

	*allow_multi_user_threads = true;
	*have_hidden_threads = false;

	/* initialize the registry head */
	OBJ_CONSTRUCT(&mca_gpr_replica_head.registry, ompi_list_t);

	/* ompi_output(0, "registry head setup"); */

	/* initialize the global dictionary for segment id's */
	OBJ_CONSTRUCT(&mca_gpr_replica_head.segment_dict, ompi_list_t);
	OBJ_CONSTRUCT(&mca_gpr_replica_head.freekeys, ompi_list_t);
	mca_gpr_replica_head.lastkey = 0;

	/* ompi_output(0, "global dict setup"); */

	/* initialize the notify request tracker */
	OBJ_CONSTRUCT(&mca_gpr_replica_notify_request_tracker, ompi_list_t);
	mca_gpr_replica_last_notify_id_tag = 0;
	OBJ_CONSTRUCT(&mca_gpr_replica_free_notify_id_tags, ompi_list_t);

	/* ompi_output(0, "req tracker setup"); */

 	/* issue the non-blocking receive */ 
 	rc = mca_oob_recv_packed_nb(MCA_OOB_NAME_ANY, MCA_OOB_TAG_GPR, 0, mca_gpr_replica_recv, NULL);
 	if(rc != OMPI_SUCCESS && rc != OMPI_ERR_NOT_IMPLEMENTED) { 
 	    return NULL;
 	}

	/* ompi_output(0, "nb receive setup"); */

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
    mca_gpr_replica_segment_t *seg;
    mca_gpr_replica_keytable_t *kt;
    mca_gpr_replica_keylist_t *kl;
    mca_gpr_notify_request_tracker_t *tk;
    mca_gpr_idtag_list_t *id;

    /* free all storage, but only if this component was initialized */

    if (initialized) {

	while (NULL != (seg = (mca_gpr_replica_segment_t*)ompi_list_remove_first(&mca_gpr_replica_head.registry))) {
	    OBJ_RELEASE(seg);
	}
	OBJ_DESTRUCT(&mca_gpr_replica_head.registry);

	while (NULL != (kt = (mca_gpr_replica_keytable_t*)ompi_list_remove_first(&mca_gpr_replica_head.segment_dict))) {
	    OBJ_RELEASE(kt);
	}
	OBJ_DESTRUCT(&mca_gpr_replica_head.segment_dict);

	while (NULL != (kl = (mca_gpr_replica_keylist_t*)ompi_list_remove_first(&mca_gpr_replica_head.freekeys))) {
	    OBJ_RELEASE(kl);
	}
	OBJ_DESTRUCT(&mca_gpr_replica_head.freekeys);


	while (NULL != (tk = (mca_gpr_notify_request_tracker_t*)ompi_list_remove_first(&mca_gpr_replica_notify_request_tracker))) {
	    OBJ_RELEASE(tk);
	}
	OBJ_DESTRUCT(&mca_gpr_replica_notify_request_tracker);


	while (NULL != (id = (mca_gpr_idtag_list_t*)ompi_list_remove_first(&mca_gpr_replica_free_notify_id_tags))) {
	    OBJ_RELEASE(id);
	}
	OBJ_DESTRUCT(&mca_gpr_replica_free_notify_id_tags);
	initialized = false;
    }

    /* All done */

    return OMPI_SUCCESS;
}

/* 
 * handle message from proxies
 */

void mca_gpr_replica_recv(int status, ompi_process_name_t* sender,
			  ompi_buffer_t buffer, int tag,
			  void* cbdata)
{
    ompi_buffer_t answer, error_answer;
    ompi_registry_object_t *object;
    ompi_registry_object_size_t object_size;
    ompi_registry_mode_t mode;
    ompi_registry_notify_action_t action;
    ompi_registry_value_t *regval;
    ompi_list_t *returned_list;
    ompi_registry_internal_test_results_t *testval;
    ompi_registry_index_value_t *indexval;
    char **tokens, **tokptr;
    int32_t num_tokens, test_level, i, trigger, id_tag;
    mca_gpr_cmd_flag_t command;
    char *segment;
    int32_t response, synchro_mode;
    mca_gpr_notify_request_tracker_t *trackptr;
    mca_gpr_idtag_list_t *ptr_free_id;

    if (OMPI_SUCCESS != ompi_buffer_init(&answer, 0)) {
	/* RHC -- not sure what to do if this fails */
    }

    if (OMPI_SUCCESS != ompi_unpack(buffer, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	goto RETURN_ERROR;
    }

    /******    DELETE SEGMENT    *****/
    if (MCA_GPR_DELETE_SEGMENT_CMD == command) {   /* got command to delete a segment */
	if (0 > ompi_unpack_string(buffer, &segment)) {
	    goto RETURN_ERROR;
	}

	response = (int32_t)ompi_registry.delete_segment(segment);

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

	object = (ompi_registry_object_t)malloc(object_size);
	if (OMPI_SUCCESS != ompi_unpack(buffer, object, object_size, OMPI_BYTE)) {
	    goto RETURN_ERROR;
	}

	response = (int32_t)ompi_registry.put(mode, segment, tokens, object, object_size);

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

	returned_list = ompi_registry.get(mode, segment, tokens);

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

	response = (int32_t)ompi_registry.delete_object(mode, segment, tokens);

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

	returned_list = ompi_registry.index(segment);

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
	action = OMPI_REGISTRY_NOTIFY_ALL;
	goto RETURN_ERROR;

	/*****     UNSUBSCRIBE     *****/
    } else if (MCA_GPR_UNSUBSCRIBE_CMD == command) {
	goto RETURN_ERROR;

	/*****     SYNCHRO     *****/
    } else if (MCA_GPR_SYNCHRO_CMD == command) {
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

	/* enter request on notify tracking system */
	trackptr = OBJ_NEW(mca_gpr_notify_request_tracker_t);
	trackptr->requestor = ompi_name_server.copy_process_name(sender);
	trackptr->req_tag = id_tag;
	trackptr->callback = NULL;
	trackptr->user_tag = NULL;
	if (ompi_list_is_empty(&mca_gpr_replica_free_notify_id_tags)) {
	    trackptr->id_tag = mca_gpr_replica_last_notify_id_tag;
	    mca_gpr_replica_last_notify_id_tag++;
	} else {
	    ptr_free_id = (mca_gpr_idtag_list_t*)ompi_list_remove_first(&mca_gpr_replica_free_notify_id_tags);
	    trackptr->id_tag = ptr_free_id->id_tag;
	}
	ompi_list_append(&mca_gpr_replica_notify_request_tracker, &trackptr->item);

	response = (int32_t)gpr_replica_construct_trigger(synchro_mode, OMPI_REGISTRY_NOTIFY_NONE,
							  mode, segment, tokens,
							  trigger, trackptr->id_tag);

	if (OMPI_SUCCESS != ompi_pack(answer, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	    goto RETURN_ERROR;
	}

	if (OMPI_SUCCESS != ompi_pack(answer, &response, 1, OMPI_INT32)) {
	    goto RETURN_ERROR;
	}

	if (0 > mca_oob_send_packed(sender, answer, tag, 0)) {
	    /* RHC -- not sure what to do if the return send fails */
	}

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
 
	/****    UNRECOGNIZED    ****/
    } else {  /* got an unrecognized command */
    RETURN_ERROR:
	ompi_buffer_init(&error_answer, 8);
	command = MCA_GPR_ERROR;
	ompi_pack(error_answer, (void*)&command, 1, MCA_GPR_OOB_PACK_CMD);
	mca_oob_send_packed(sender, error_answer, tag, 0);
	ompi_buffer_free(error_answer);
    }

    ompi_buffer_free(answer);
    ompi_buffer_free(buffer);

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
