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
#include "ompi_config.h"

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
    mca_gpr_replica_get,
    mca_gpr_replica_put,
    mca_gpr_replica_delete_segment,
    mca_gpr_replica_subscribe,
    mca_gpr_replica_unsubscribe,
    mca_gpr_replica_synchro,
    mca_gpr_replica_cancel_synchro,
    mca_gpr_replica_delete_object,
    mca_gpr_replica_index,
    mca_gpr_replica_test_internals,
    mca_gpr_replica_begin_compound_cmd,
    mca_gpr_replica_stop_compound_cmd,
    mca_gpr_replica_exec_compound_cmd,
    mca_gpr_replica_dump,
    mca_gpr_replica_silent_mode_on,
    mca_gpr_replica_silent_mode_off,
    mca_gpr_replica_notify_off,
    mca_gpr_replica_notify_on,
    mca_gpr_replica_assign_ownership,
    mca_gpr_replica_triggers_active,
    mca_gpr_replica_triggers_inactive,
    mca_gpr_replica_get_startup_msg,
    mca_gpr_replica_cleanup_job,
    mca_gpr_replica_cleanup_proc,
    mca_gpr_replica_deliver_notify_msg
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
ompi_list_t mca_gpr_replica_notify_off_list;
ompi_registry_notify_id_t mca_gpr_replica_last_notify_id_tag;
ompi_list_t mca_gpr_replica_free_notify_id_tags;
int mca_gpr_replica_debug;
ompi_mutex_t mca_gpr_replica_mutex;
bool mca_gpr_replica_compound_cmd_mode;
bool mca_gpr_replica_exec_compound_cmd_mode;
ompi_buffer_t mca_gpr_replica_compound_cmd;
ompi_mutex_t mca_gpr_replica_wait_for_compound_mutex;
ompi_condition_t mca_gpr_replica_compound_cmd_condition;
int mca_gpr_replica_compound_cmd_waiting;
bool mca_gpr_replica_silent_mode;


/* constructor - used to initialize state of notify_off instance */
static void mca_gpr_replica_notify_off_construct(mca_gpr_replica_notify_off_t* off)
{
    off->sub_number = OMPI_REGISTRY_NOTIFY_ID_MAX;
    off->proc = NULL;
}

/* destructor - used to free any resources held by notify_off instance */
static void mca_gpr_replica_notify_off_destructor(mca_gpr_replica_notify_off_t* off)
{
    if (NULL != off->proc) {
	free(off->proc);
    }
}

/* define instance of notify_off class */
OBJ_CLASS_INSTANCE(
		   mca_gpr_replica_notify_off_t,
		   ompi_list_item_t,
		   mca_gpr_replica_notify_off_construct,
		   mca_gpr_replica_notify_off_destructor);


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
    trig->local_idtag = OMPI_REGISTRY_NOTIFY_ID_MAX;
}

/* destructor - used to free any resources held by instance */
static void mca_gpr_replica_trigger_list_destructor(mca_gpr_replica_trigger_list_t* trig)
{
    char **tok;
    uint i;

    if (NULL != trig->keys) {
	free(trig->keys);
	trig->keys = NULL;
    }
    if (NULL != trig->tokens) {
	for (i=0, tok=trig->tokens; i< (uint)trig->num_keys; i++) {
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
    seg->name = NULL;
    seg->owning_job = MCA_NS_BASE_JOBID_MAX;
    seg->key = MCA_GPR_REPLICA_KEY_MAX;
    seg->lastkey = 0;
    seg->triggers_active = false;
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

    if (NULL != seg->name) {
	free(seg->name);
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

/* define instance of mca_gpr_replica_segment_t */
OBJ_CLASS_INSTANCE(
		   mca_gpr_replica_segment_t,  /* type name */
		   ompi_list_item_t, /* parent "class" name */
		   mca_gpr_replica_segment_construct, /* constructor */
		   mca_gpr_replica_segment_destructor); /* destructor */


/* constructor - used to initialize notify message instance */
static void mca_gpr_replica_notify_request_tracker_construct(mca_gpr_replica_notify_request_tracker_t* req)
{
    req->requestor = NULL;
    req->callback = NULL;
    req->user_tag = NULL;
    req->local_idtag = OMPI_REGISTRY_NOTIFY_ID_MAX;
    req->remote_idtag = OMPI_REGISTRY_NOTIFY_ID_MAX;
    req->segptr = NULL;
    req->action = OMPI_REGISTRY_NOTIFY_NONE;
}

/* destructor - used to free any resources held by instance */
static void mca_gpr_replica_notify_request_tracker_destructor(mca_gpr_replica_notify_request_tracker_t* req)
{
    if (NULL != req->requestor) {
	free(req->requestor);
    }
}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
		   mca_gpr_replica_notify_request_tracker_t,            /* type name */
		   ompi_list_item_t,                          /* parent "class" name */
		   mca_gpr_replica_notify_request_tracker_construct,    /* constructor */
		   mca_gpr_replica_notify_request_tracker_destructor);  /* destructor */



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

	/* setup the thread locks and condition variables */
	OBJ_CONSTRUCT(&mca_gpr_replica_mutex, ompi_mutex_t);
	OBJ_CONSTRUCT(&mca_gpr_replica_wait_for_compound_mutex, ompi_mutex_t);
	OBJ_CONSTRUCT(&mca_gpr_replica_compound_cmd_condition, ompi_condition_t);

	/* initialize the registry compound mode */
	mca_gpr_replica_compound_cmd_mode = false;
	mca_gpr_replica_exec_compound_cmd_mode = false;
	mca_gpr_replica_compound_cmd_waiting = 0;
	mca_gpr_replica_compound_cmd = NULL;

	/* initialize the registry head */
/* 	OBJ_CONSTRUCT(&mca_gpr_replica_head.registry, ompi_object_t); */
	OBJ_CONSTRUCT(&mca_gpr_replica_head.registry, ompi_list_t);
	OBJ_CONSTRUCT(&mca_gpr_replica_head.triggers, ompi_list_t);

	/* initialize the global dictionary for segment id's */
	OBJ_CONSTRUCT(&mca_gpr_replica_head.segment_dict, ompi_list_t);
	OBJ_CONSTRUCT(&mca_gpr_replica_head.freekeys, ompi_list_t);
	mca_gpr_replica_head.lastkey = 0;

	/* initialize the notify request tracker */
	OBJ_CONSTRUCT(&mca_gpr_replica_notify_request_tracker, ompi_list_t);
	mca_gpr_replica_last_notify_id_tag = 0;
	OBJ_CONSTRUCT(&mca_gpr_replica_free_notify_id_tags, ompi_list_t);

	if (mca_gpr_replica_debug) {
	    ompi_output(0, "req tracker setup");
	}

	/* initialize the callback list head */
	OBJ_CONSTRUCT(&mca_gpr_replica_callbacks, ompi_list_t);

	/* initialize the mode trackers */
	OBJ_CONSTRUCT(&mca_gpr_replica_notify_off_list, ompi_list_t);

	/* initialize any local variables */
	mca_gpr_replica_silent_mode = false;

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
