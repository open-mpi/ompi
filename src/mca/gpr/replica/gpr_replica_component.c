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
    gpr_replica_define_segment,
    gpr_replica_delete_segment,
    gpr_replica_subscribe,
    gpr_replica_unsubscribe,
    gpr_replica_delete
};

/*
 * Whether or not we allowed this component to be selected
 */
static bool initialized = false;


/*
 * globals needed within replica component
 */
ompi_registry_t mca_gpr_head;


/* constructor - used to initialize state of keytable instance */
static void ompi_keytable_construct(ompi_keytable_t* keytable)
{
    keytable->token = NULL;
    keytable->key = 0;
}

/* destructor - used to free any resources held by instance */
static void ompi_keytable_destructor(ompi_keytable_t* keytable)
{
    if (NULL != keytable->token) {
	free(keytable->token);
    }
}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
		   ompi_keytable_t,  /* type name */
		   ompi_list_item_t, /* parent "class" name */
		   ompi_keytable_construct, /* constructor */
		   ompi_keytable_destructor); /* destructor */


/* constructor - used to initialize state of keytable instance */
static void ompi_keylist_construct(ompi_keylist_t* keylist)
{
    keylist->key = 0;
}

/* destructor - used to free any resources held by instance */
static void ompi_keylist_destructor(ompi_keylist_t* keylist)
{
}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
		   ompi_keylist_t,  /* type name */
		   ompi_list_item_t, /* parent "class" name */
		   ompi_keylist_construct, /* constructor */
		   ompi_keylist_destructor); /* destructor */

/* constructor - used to initialize state of keytable instance */
static void ompi_subscribe_list_construct(ompi_subscribe_list_t* subscriber)
{
    subscriber->id = 0;
    subscriber->action = 0x00;
}

/* destructor - used to free any resources held by instance */
static void ompi_subscribe_list_destructor(ompi_subscribe_list_t* subscriber)
{
}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
		   ompi_subscribe_list_t,  /* type name */
		   ompi_list_item_t, /* parent "class" name */
		   ompi_subscribe_list_construct, /* constructor */
		   ompi_subscribe_list_destructor); /* destructor */


/* constructor - used to initialize state of replica list instance */
static void ompi_replica_list_construct(ompi_replica_list_t* replica)
{
    replica->name = NULL;
}

/* destructor - used to free any resources held by instance */
static void ompi_replica_list_destructor(ompi_replica_list_t* replica)
{
    if (NULL != replica->name) {
	free(replica->name);
    }
}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
		   ompi_replica_list_t,  /* type name */
		   ompi_list_item_t, /* parent "class" name */
		   ompi_replica_list_construct, /* constructor */
		   ompi_replica_list_destructor); /* destructor */



/* constructor - used to initialize state of registry core instance */
static void ompi_registry_core_construct(ompi_registry_core_t* reg)
{
    reg->object = NULL;
    reg->object_size = 0;
    OBJ_CONSTRUCT(&reg->subscriber, ompi_list_t);
    OBJ_CONSTRUCT(&reg->keys, ompi_list_t);
}

/* destructor - used to free any resources held by instance */
static void ompi_registry_core_destructor(ompi_registry_core_t* reg)
{
    if (NULL != reg->object) {
	free(reg->object);
    }
    OBJ_DESTRUCT(&reg->subscriber);
    OBJ_DESTRUCT(&reg->keys);
}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
		   ompi_registry_core_t,  /* type name */
		   ompi_list_item_t, /* parent "class" name */
		   ompi_registry_core_construct, /* constructor */
		   ompi_registry_core_destructor); /* destructor */



/* constructor - used to initialize state of segment instance */
static void ompi_registry_segment_construct(ompi_registry_segment_t* seg)
{
    seg->segment = 0;
    seg->lastkey = 0;
    OBJ_CONSTRUCT(&seg->reg_list, ompi_list_t);
    OBJ_CONSTRUCT(&seg->keytable, ompi_list_t);
    OBJ_CONSTRUCT(&seg->freekeys, ompi_list_t);
}

/* destructor - used to free any resources held by instance */
static void ompi_registry_segment_destructor(ompi_registry_segment_t* seg)
{
    OBJ_DESTRUCT(&seg->reg_list);
    OBJ_DESTRUCT(&seg->keytable);
    OBJ_DESTRUCT(&seg->freekeys);
}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
		   ompi_registry_segment_t,  /* type name */
		   ompi_list_item_t, /* parent "class" name */
		   ompi_registry_segment_construct, /* constructor */
		   ompi_registry_segment_destructor); /* destructor */


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
    /* If we're the seed, then we want to be selected, so do all the
       setup and return the module */

    if (ompi_process_info.seed) {


	/* Return a module (choose an arbitrary, positive priority --
	   it's only relevant compared to other ns components).  If
	   we're not the seed, then we don't want to be selected, so
	   return NULL. */

	*priority = 50;

	/* We allow multi user threads but don't have any hidden threads */

	*allow_multi_user_threads = true;
	*have_hidden_threads = false;

	/* initialize the registry head */
	OBJ_CONSTRUCT(&mca_gpr_head.registry, ompi_list_t);

	/* initialize the global dictionary for segment id's */
	OBJ_CONSTRUCT(&mca_gpr_head.segment_dict, ompi_list_t);
	OBJ_CONSTRUCT(&mca_gpr_head.freekeys, ompi_list_t);
	mca_gpr_head.lastkey = 0;

	/* define the "universe" segment key */
	if (0 == gpr_replica_definekey("universe", NULL)) {
	    ompi_output(0, "registry_init(error): could not create universe dictionary entry\n");
	    exit(OMPI_ERROR);
	}

	/* initialize the "universe" segment */
	seg = OBJ_NEW(ompi_registry_segment_t);  /* allocate a new segment */
	seg->segment = gpr_replica_getkey("universe", NULL);
	ompi_list_append(&mca_gpr_head.registry, &seg->item);  /* add to the global registry */

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
	initialized = false;
    }

    /* All done */

    return OMPI_SUCCESS;
}
