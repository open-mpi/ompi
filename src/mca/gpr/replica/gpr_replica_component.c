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

#include "include/constants.h"
#include "util/proc_info.h"
#include "util/output.h"
#include "util/pack.h"

#include "mca/mca.h"
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
mca_gpr_registry_t mca_gpr_replica_head;


/* constructor - used to initialize state of keytable instance */
static void mca_gpr_keytable_construct(mca_gpr_keytable_t* keytable)
{
    keytable->token = NULL;
    keytable->key = 0;
}

/* destructor - used to free any resources held by instance */
static void mca_gpr_keytable_destructor(mca_gpr_keytable_t* keytable)
{
    if (NULL != keytable->token) {
	free(keytable->token);
    }
}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
		   mca_gpr_keytable_t,            /* type name */
		   ompi_list_item_t,              /* parent "class" name */
		   mca_gpr_keytable_construct,    /* constructor */
		   mca_gpr_keytable_destructor);  /* destructor */


/* constructor - used to initialize state of keylist instance */
static void mca_gpr_keylist_construct(mca_gpr_keylist_t* keylist)
{
    keylist->key = 0;
}

/* destructor - used to free any resources held by instance */
static void mca_gpr_keylist_destructor(mca_gpr_keylist_t* keylist)
{
}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
		   mca_gpr_keylist_t,           /* type name */
		   ompi_list_item_t,            /* parent "class" name */
		   mca_gpr_keylist_construct,   /* constructor */
		   mca_gpr_keylist_destructor); /* destructor */


/* constructor - used to initialize state of subscriber list instance */
static void mca_gpr_subscriber_list_construct(mca_gpr_subscriber_list_t* subscriber)
{
    subscriber->subscriber = NULL;
    subscriber->action = 0x00;
}

/* destructor - used to free any resources held by instance */
static void mca_gpr_subscriber_list_destructor(mca_gpr_subscriber_list_t* subscriber)
{
    if (NULL != subscriber->subscriber) {
	free(subscriber->subscriber);
    }
}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
		   mca_gpr_subscriber_list_t,           /* type name */
		   ompi_list_item_t,                    /* parent "class" name */
		   mca_gpr_subscriber_list_construct,   /* constructor */
		   mca_gpr_subscriber_list_destructor); /* destructor */


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
static void mca_gpr_registry_core_construct(mca_gpr_registry_core_t* reg)
{
    OBJ_CONSTRUCT(&reg->keys, ompi_list_t);
    reg->object_size = 0;
    reg->object = NULL;
    OBJ_CONSTRUCT(&reg->subscriber, ompi_list_t);
    OBJ_CONSTRUCT(&reg->replicas, ompi_list_t);
}

/* destructor - used to free any resources held by instance */
static void mca_gpr_registry_core_destructor(mca_gpr_registry_core_t* reg)
{
    OBJ_DESTRUCT(&reg->keys);
    if (NULL != reg->object) {
	free(reg->object);
    }
    OBJ_DESTRUCT(&reg->subscriber);
    OBJ_DESTRUCT(&reg->replicas);
}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
		   mca_gpr_registry_core_t,  /* type name */
		   ompi_list_item_t, /* parent "class" name */
		   mca_gpr_registry_core_construct, /* constructor */
		   mca_gpr_registry_core_destructor); /* destructor */



/* constructor - used to initialize state of segment instance */
static void mca_gpr_registry_segment_construct(mca_gpr_registry_segment_t* seg)
{
    seg->segment = 0;
    seg->lastkey = 0;
    OBJ_CONSTRUCT(&seg->registry_entries, ompi_list_t);
    OBJ_CONSTRUCT(&seg->keytable, ompi_list_t);
    OBJ_CONSTRUCT(&seg->freekeys, ompi_list_t);
}

/* destructor - used to free any resources held by instance */
static void mca_gpr_registry_segment_destructor(mca_gpr_registry_segment_t* seg)
{
    OBJ_DESTRUCT(&seg->registry_entries);
    OBJ_DESTRUCT(&seg->keytable);
    OBJ_DESTRUCT(&seg->freekeys);
}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
		   mca_gpr_registry_segment_t,  /* type name */
		   ompi_list_item_t, /* parent "class" name */
		   mca_gpr_registry_segment_construct, /* constructor */
		   mca_gpr_registry_segment_destructor); /* destructor */


/*
 * don't really need this function - could just put NULL in the above structure
 * Just holding the place in case we decide there is something we need to do
 */
int mca_gpr_replica_open(void)
{
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
    int response;

    /* If we're the seed, then we want to be selected, so do all the
       setup and return the module */

    if (ompi_process_info.seed) {

	ompi_output(mca_gpr_base_output, "entered replica_init");

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

	/* initialize the global dictionary for segment id's */
	OBJ_CONSTRUCT(&mca_gpr_replica_head.segment_dict, ompi_list_t);
	OBJ_CONSTRUCT(&mca_gpr_replica_head.freekeys, ompi_list_t);
	mca_gpr_replica_head.lastkey = 0;

	/* define the "universe" segment */
	response = gpr_replica_define_segment("universe");
	if (0 > response) { /* got error code */
	    ompi_output(mca_gpr_base_output, "registry_init(error): could not create universe segment\n");
	    exit(response);
	}

	ompi_output(mca_gpr_base_output, "issuing receive");

	/* issue the non-blocking receive */
/*       	mca_oob_recv_packed_nb(MCA_OOB_NAME_ANY, MCA_OOB_TAG_GPR, 0, mca_gpr_replica_recv, NULL); */

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
    /* free all storage, but only if this component was initialized */

    if (initialized) {
	OBJ_DESTRUCT(&mca_gpr_replica_head);
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
    char **tokens, **tokptr;
    int32_t num_tokens, level, i;
    mca_gpr_cmd_flag_t command;
    char *segment;
    int32_t response;

    if (OMPI_SUCCESS != ompi_buffer_init(&answer, 0)) {
	/* RHC -- not sure what to do if this fails */
    }

    if (OMPI_SUCCESS != ompi_unpack(buffer, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	goto RETURN_ERROR;
    }

    if (MCA_GPR_DELETE_SEGMENT_CMD == command) {   /* got command to delete a segment */
	if (0 > ompi_unpack_string(buffer, &segment)) {
	    goto RETURN_ERROR;
	}

	response = (int16_t)ompi_registry.delete_segment(segment);

	if (OMPI_SUCCESS != ompi_pack(answer, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	    goto RETURN_ERROR;
	}
	if (OMPI_SUCCESS != ompi_pack(answer, &response, 1, OMPI_INT32)) {
	    goto RETURN_ERROR;
	}
	if (0 > mca_oob_send_packed(sender, answer, tag, 0)) {
	    /* RHC -- not sure what to do if the return send fails */
	}
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

	tokens = (char**)malloc(num_tokens*sizeof(char*));

	tokptr = tokens;
	for (i=0; i<num_tokens; i++) {
	    if (0 > ompi_unpack_string(buffer, tokptr)) {
		goto RETURN_ERROR;
	    }
	    tokptr++;
	}

	if (OMPI_SUCCESS != ompi_unpack(buffer, &object_size, 1, MCA_GPR_OOB_PACK_OBJECT_SIZE)) {
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

	tokens = (char**)malloc(num_tokens*sizeof(char*));
	tokptr = tokens;
	for (i=0; i<num_tokens; i++) {
	    if (0 > ompi_unpack_string(buffer, tokptr)) {
		goto RETURN_ERROR;
	    }
	    tokptr++;
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
