/*
 * $HEADER$
 */
/** @file:
 *
 * The Open MPI general purpose registry - implementation.
 *
 */

/*
 * includes
 */

#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include <string.h>
#include <unistd.h>
#include <libgen.h>

#include "ompi_config.h"
#include "include/constants.h"
#include "rte/universe/registry/registry.h"

/*
 * globals
 */

struct ompi_registry_t {
    ompi_list_t registry;
    ompi_list_t segment_dict;
    unsigned long int lastkey;
    ompi_list_t freekeys;
};
typedef struct ompi_registry_t ompi_registry_t;

ompi_registry_t ompi_registry;


/*
 * local type definitions
 */

/** Dictionary of token-key pairs.
 * This structure is used to create a linked list of token-key pairs. All calls to
 * registry functions pass character string tokens for programming clarity - the ompi_keytable
 * structure is used to translate those strings into an integer key value, thus allowing
 * for faster searches of the registry. This structure is also used to return token-key pairs
 * from the dictionary in response to an ompi_registry_index() call.
 */
struct ompi_keytable_t {
    ompi_list_item_t item;  /**< Allows this item to be placed on a list */
    char *token;  /**< Char string that defines the key */
    unsigned long int key;      /**< Numerical value assigned by registry to represent token string */
};
typedef struct ompi_keytable_t ompi_keytable_t;

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

/** List of keys that describe a stored object.
 * Each object stored in the registry may have as many keys describing it as the
 * creator desires. This structure is used to create a linked list of keys
 * associated with each object.
 */
struct ompi_keylist_t {
    ompi_list_item_t item;   /**< Allows this item to be placed on a list */
    unsigned long int key;     /**< Numerical key that defines stored object */
};
typedef struct ompi_keylist_t ompi_keylist_t;

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


/** List of subscribers to a stored object.
 * Each object can have an arbitrary number of subscribers desiring notification
 * upon specified actions being performed against the object. This structure is
 * used to create a linked list of subscribers for objects.
 */
struct ompi_subscribe_list_t {
    ompi_list_item_t item;    /**< Allows this item to be placed on a list */
    unsigned long int id;   /**< ID of the subscriber */
    uint8_t action;  /**< Bit-mask of actions that trigger notification */
};
typedef struct ompi_subscribe_list_t ompi_subscribe_list_t;

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


/** List of replicas that hold a stored object.
 * Each object can have an arbitrary number of replicas that hold a copy
 * of the object. The GPR requires that each object be replicated in at least
 * two locations. This structure is used to create a linked list of
 * replicas for the object.
 */
struct ompi_replica_list_t {
    ompi_list_item_t item;    /**< Allows this item to be placed on a list */
    char *name;               /**< Name of the replica */
};
typedef struct ompi_subscribe_list_t ompi_subscribe_list_t;

/* constructor - used to initialize state of keytable instance */
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

/** Write invalidate structure.
 * The structure used to indicate that an object has been updated somewhere else in the GPR.
 * The structure contains a flag indicating that the locally stored copy of the object
 * is no longer valid, a time tag indicating the time of the last known modification
 * of the object within the global registry, and the replica holding the last known
 * up-to-date version of the object.
 */
struct ompi_write_invalidate_t {
    bool invalidate;
    time_t last_mod;
    char *replica;
};
typedef struct ompi_write_invalidate_t ompi_write_invalidate_t;


/** The core registry structure.
 * Each segment of the registry contains a linked list of registry entries. This structure
 * represents a link in that list. The structure contains a linked list of the keys that
 * define this particular object, the size of the object, a pointer to the object, and a linked
 * list of subscribers to this object. Objects are stored as unsigned bytes - knowledge of any
 * structure within the objects is the responsibility of the calling functions. The repository
 * has no knowledge of what is in the structure, nor any way of determining such structure.
 *
 * At this time, no security is provided on an object-level basis. Thus, all requests for an
 * object are automatically granted. This may be changed at some future time by adding an
 * "authorization" linked list of ID's and their access rights to this structure.
 */
struct ompi_registry_core_t {
    ompi_list_item_t item;   /**< Allows this item to be placed on a list */
    ompi_list_t keys;   /**< Linked list of keys that define stored object */
    int object_size;      /**< Size of stored object, in bytes */
    uint8_t *object;      /**< Pointer to stored object */
    ompi_list_t subscriber;  /**< Linked list of subscribers to this object */
    ompi_list_t replicas;   /**< Linked list of replicas that also contain this object */
    ompi_write_invalidate_t write_invalidate;  /**< Structure containing write invalidate info */
};
typedef struct ompi_registry_core_t ompi_registry_core_t;

/* constructor - used to initialize state of keytable instance */
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


/** Registry segment definition.
 * The registry is subdivided into segments, each defining a unique domain. The "universe" segment
 * is automatically created to allow the exchange of information supporting universe-level functions.
 * Similarly, a segment is automatically created for each MPI CommWorld within the universe - the
 * name for that segment is stored in each CommWorld's ompi_system_info structure so program
 * elements within that CommWorld can access it. The segment structure serves as the "head" of a linked
 * list of registry elements for that segment. Each segment also holds its own token-key dictionary
 * to avoid naming conflicts between tokens from CommWorlds sharing a given universe.
 */
struct ompi_registry_segment_t {
    ompi_list_item_t item;   /**< Allows this item to be placed on a list */
    unsigned long int segment;    /**< ID of registry segment */
    unsigned long int lastkey;    /**< Highest key value used */
    ompi_list_t reg_list;   /**< Linked list of stored objects within this segment */
    ompi_list_t keytable;   /**< Token-key dictionary for this segment */
    ompi_list_t freekeys;  /**< List of keys that have been made available */
};
typedef struct ompi_registry_segment_t ompi_registry_segment_t;

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
 * internal function prototypes
 */

/** Retrieve a registry key value for a given token string.
 * The ompi_registry_getkey() function is used to translate a token string for a particular
 * segment of the registry into its associated (integer) key value.
 *
 * @param segment Pointer to a character string defining the segment of the registry being queried.
 * @param token Pointer to a character string containing the token to be translated. If token=NULL,
 * the function returns the key value corresponding to the specified segment itself.
 *
 * @retval key Unsigned long integer value corresponding to the specified token within the specified segment.
 * @retval -1 Indicates that the segment and/or token could not be found.
 */
unsigned long int ompi_registry_getkey(char *segment, char *token);

/** Add a token to a segment's dictionary.
 * The ompi_registry_definekey() function allows the addition of a new definition to
 * the registry's token-key dictionaries. The specified token is assigned an integer
 * value within the specified segment, and the entry is added to the segment's token-key
 * dictionary.
 *
 * @param segment Pointer to a character string defining the segment of the registry being queried.
 * @param token Pointer to a character string containing the token to be defined. If token=NULL,
 * the function adds the token to the segment dictionary, thus defining a new segment name.
 *
 * @retval key Unsigned long integer value corresponding to the specified token within the specified segment.
 * @retval -1 Indicates that the entry could not be created.
 */
unsigned long int ompi_registry_definekey(char *segment, char *token);

/** Delete a token from a segment's dictionary.
 * The ompi_registry_deletekey() function allows the removal of a definition from the
 * registry's token-key dictionaries. This should be used with caution! Deletion of
 * a token-key pair causes the registry to search through all entries within that segment
 * for objects that include the specified token-key pair in their description. The reference
 * is subsequently removed, and any object for which this was the SOLE key will also
 * be removed from the registry!
 *
 * @param segment Pointer to a character string defining the segment of the registry.
 * @param token Pointer to a character string containing the token to be deleted. If token=NULL,
 * the function deletes the specified segment name from the segment dictionary.
 *
 * @retval OMPI_SUCCESS Indicating that the operation was successful.
 * @retval OMPI_ERROR Indicates that the operation failed - most likely caused by specifying
 * a token that did not exist within the specified segment, or a non-existent segment.
 */
int ompi_registry_deletekey(char *segment, char *token);

ompi_registry_segment_t *ompi_registry_findseg(char *segment);

ompi_keytable_t *ompi_registry_finddictentry(char *segment, char *token);


int main(int argc, char **argv)
{
    /*    ompi_keytable_t *keytable;
    ompi_registry_core_t *reg;
    */
    ompi_registry_segment_t *seg;
    uint8_t *object;

    /* initialize the global registry list */
    OBJ_CONSTRUCT(&ompi_registry.registry, ompi_list_t);

    /* initialize the global dictionary for segment id's */
    OBJ_CONSTRUCT(&ompi_registry.segment_dict, ompi_list_t);
    OBJ_CONSTRUCT(&ompi_registry.freekeys, ompi_list_t);
    ompi_registry.lastkey = 0;

    /* define the "universe" segment key */
    if (0 == ompi_registry_definekey("universe", NULL)) {
	fprintf(stderr, "registry_init(error): could not create universe dictionary entry\n");
	exit(OMPI_ERROR);
    }

    /* initialize the "universe" segment */
    seg = OBJ_NEW(ompi_registry_segment_t);  /* allocate a new segment */
    seg->segment = ompi_registry_getkey("universe", NULL);
    ompi_list_append(&ompi_registry.registry, &seg->item);  /* add to the global registry */

    printf("initialized universe seg %ld\n", seg->segment);

    if (0 == ompi_registry_definekey("universe", "ipsy-doodles")) {
	fprintf(stderr, "well, ipsy-doodles define didn't work\n");
	exit(OMPI_ERROR);
    }

    printf("ipsy-doodles ok: %ld\n", ompi_registry_getkey("universe", "ipsy-doodles"));

    if (0 == ompi_registry_definekey("commworld-1", NULL)) {
	fprintf(stderr, "commworld 1 failed\n");
	exit(OMPI_ERROR);
    }

    printf("commworld ok: %ld\n", ompi_registry_getkey("commworld-1", NULL));

    object = (uint8_t *)malloc(200);

    ompi_registry_put(false, object, sizeof(object), "commworld-1", "tok1", "tok2", "tok3", "tok4", NULL);

    /*    keytable = (ompi_keytable *)malloc(sizeof(ompi_keytable));
    keytable->token = strdup("universe");
    keytable->key = 1;
    keytable->next = NULL;

    nextkey = (ompi_keytable *)malloc(sizeof(ompi_keytable));
    nextkey->token = strdup("commworld-1");
    nextkey->key = 1;
    keytable->next = nextkey;
    nextkey->next = NULL;

    registry = (ompi_registry_core *)malloc(sizeof(ompi_registry_core));
    registry->primary_key = ompi_getkey("universe");
    registry->keys = (ompi_keylist *)malloc(sizeof(ompi_keylist));
    registry->keys->key = ompi_getkey("commworld-1");
    registry->keys->next = NULL;
    registry->object_size = 100;
    registry->object = (uint8_t *)malloc(100);
    registry->subscriber = (ompi_subscribe_list *)malloc(sizeof(ompi_subscribe_list));
    registry->subscriber->id = 1;
    registry->subscriber->action = OMPI_REGISTRY_NOTIFY_MODIFICATION | OMPI_REGISTRY_NOTIFY_DELETE;

    printf("universe key = %d\n", registry->primary_key);
    */
}

ompi_registry_segment_t *ompi_registry_findseg(char *segment)
{
    ompi_keytable_t *ptr_seg;
    ompi_registry_segment_t *seg;

    /* search the registry segments to find which one is being referenced */
    for (ptr_seg = (ompi_keytable_t*)ompi_list_get_first(&ompi_registry.segment_dict);
	 ptr_seg != (ompi_keytable_t*)ompi_list_get_end(&ompi_registry.segment_dict);
	 ptr_seg = (ompi_keytable_t*)ompi_list_get_next(ptr_seg)) {
	if (0 == strcmp(segment, ptr_seg->token)) {
	    fprintf(stderr, "findseg: found segment token %s key %ld\n", ptr_seg->token, ptr_seg->key);
	    /* search ompi_registry to find segment */
	    for (seg=(ompi_registry_segment_t*)ompi_list_get_first(&ompi_registry.registry);
		 seg != (ompi_registry_segment_t*)ompi_list_get_end(&ompi_registry.registry);
		 seg = (ompi_registry_segment_t*)ompi_list_get_next(seg)) {
		fprintf(stderr, "findseg: checking seg\n");
		if(seg->segment == ptr_seg->key) {
		    fprintf(stderr, "findseg: found segment key %ld\n", seg->segment);
		    return(seg);
		}
	    }
	}
    }
    return(NULL); /* couldn't find the specified segment */
}

ompi_keytable_t *ompi_registry_finddictentry(char *segment, char *token)
{
    ompi_keytable_t *ptr_seg;
    ompi_keytable_t *ptr_key;
    ompi_registry_segment_t *seg;

    /* search the registry segments to find which one is being referenced */
    for (ptr_seg = (ompi_keytable_t*)ompi_list_get_first(&ompi_registry.segment_dict);
	 ptr_seg != (ompi_keytable_t*)ompi_list_get_end(&ompi_registry.segment_dict);
	 ptr_seg = (ompi_keytable_t*)ompi_list_get_next(ptr_seg)) {
	if (0 == strcmp(segment, ptr_seg->token)) {
	    if (NULL == token) { /* just want segment token-key pair */
		return(ptr_seg);
	    }
	    /* search ompi_registry to find segment */
	    for (seg=(ompi_registry_segment_t*)ompi_list_get_first(&ompi_registry.registry);
		 seg != (ompi_registry_segment_t*)ompi_list_get_end(&ompi_registry.registry);
		 seg = (ompi_registry_segment_t*)ompi_list_get_next(seg)) {
		if(seg->segment == ptr_seg->key) {
		    /* got segment - now find specified token-key pair in that dictionary */
		    for (ptr_key = (ompi_keytable_t*)ompi_list_get_first(&seg->keytable);
			 ptr_key != (ompi_keytable_t*)ompi_list_get_end(&seg->keytable);
			 ptr_key = (ompi_keytable_t*)ompi_list_get_next(ptr_key)) {
			if (0 == strcmp(token, ptr_key->token)) {
			    return(ptr_key);
			}
		    }
		    return(NULL); /* couldn't find the specified entry */
		}
	    }
	    return(NULL); /* couldn't find segment, even though we found entry in registry dict */
	}
    }
    return(NULL); /* couldn't find segment token-key pair */
}


unsigned long int ompi_registry_getkey(char *segment, char *token)
{
    ompi_keytable_t *ptr_key;

    /* find registry segment */
    ptr_key = ompi_registry_finddictentry(segment, NULL);
    if (NULL != ptr_key) {
	if (NULL == token) { /* only want segment key */
	    return(ptr_key->key);
	}
	/* if token specified, find the dictionary entry that matches token */
	ptr_key = ompi_registry_finddictentry(segment, token);
	if (NULL != ptr_key) {
	    return(ptr_key->key);
	}
	return(0); /* couldn't find dictionary entry */
    }
    return(0); /* couldn't find segment */
}


unsigned long int ompi_registry_definekey(char *segment, char *token)
{
    ompi_registry_segment_t *seg;
    ompi_keytable_t *ptr_seg, *ptr_key, *new;

    /* protect against errors */
    if (NULL == segment) {
	return(0);
    }

    /* if token is NULL, then this is defining a segment name. Check dictionary to ensure uniqueness */
    if (NULL == token) {
	for (ptr_seg = (ompi_keytable_t*)ompi_list_get_first(&ompi_registry.segment_dict);
	     ptr_seg != (ompi_keytable_t*)ompi_list_get_end(&ompi_registry.segment_dict);
	     ptr_seg = (ompi_keytable_t*)ompi_list_get_next(ptr_seg)) {
	    if (0 == strcmp(segment, ptr_seg->token)) {
		return(0);
	    }
	}

	/* okay, name is not previously taken. Define a key value for it and return */
	new = OBJ_NEW(ompi_keytable_t);
	new->token = strdup(segment);
	if (0 == ompi_list_get_size(&ompi_registry.freekeys)) { /* no keys waiting for reuse */
	    ompi_registry.lastkey++;
	    new->key = ompi_registry.lastkey;
	} else {
	    ptr_key = (ompi_keytable_t*)ompi_list_remove_first(&ompi_registry.freekeys);
	    new->key = ptr_key->key;
	}
	ompi_list_append(&ompi_registry.segment_dict, &new->item);
	return(new->key);
    }

    /* okay, token is specified */
    /* search the registry segments to find which one is being referenced */
    seg = ompi_registry_findseg(segment);
    if (NULL != seg) {
	/* using that segment, check dictionary to ensure uniqueness */
	for (ptr_key = (ompi_keytable_t*)ompi_list_get_first(&seg->keytable);
	     ptr_key != (ompi_keytable_t*)ompi_list_get_end(&seg->keytable);
	     ptr_key = (ompi_keytable_t*)ompi_list_get_next(ptr_key)) {
	    if (0 == strcmp(token, ptr_key->token)) {
		return(0); /* already taken, report error */
	    }
	}
	/* okay, token is unique - create dictionary entry */
	new = OBJ_NEW(ompi_keytable_t);
	new->token = strdup(token);
	if (0 == ompi_list_get_size(&seg->freekeys)) { /* no keys waiting for reuse */
	    seg->lastkey++;
	    new->key = seg->lastkey;
	} else {
	    ptr_key = (ompi_keytable_t*)ompi_list_remove_first(&seg->freekeys);
	    new->key = ptr_key->key;
	}
	ompi_list_append(&seg->keytable, &new->item);
	return(new->key);
    }
    /* couldn't find segment */
    return(0);
}

int ompi_registry_deletekey(char *segment, char *token)
{
    ompi_registry_segment_t *seg;
    ompi_registry_core_t *reg, *prev;
    ompi_keytable_t *ptr_seg, *ptr_key, *new, *regkey;
    ompi_subscribe_list_t *subscriber;

    /* protect ourselves against errors */
    if (NULL == segment) {
	return(OMPI_ERROR);
    }

    /* find the segment */
    seg = ompi_registry_findseg(segment);
    if (NULL != seg) {
	/* if specified token is NULL, then this is deleting a segment name.*/
	if (NULL == token) {
	    /* empty the segment's registry */
	    while (0 < ompi_list_get_size(&seg->reg_list)) {
		ompi_list_remove_last(&seg->reg_list);
	    }
	    /* empty the segment's dictionary */
	    while (0 < ompi_list_get_size(&seg->keytable)) {
		ompi_list_remove_last(&seg->keytable);
	    }
	    /* empty the list of free keys */
	    while (0 < ompi_list_get_size(&seg->freekeys)) {
		ompi_list_remove_last(&seg->freekeys);
	    }
	    /* now remove segment from global registry */
	    ompi_list_remove_item(&ompi_registry.registry, &seg->item);
	    /* add key to global registry's freekey list */
	    new = OBJ_NEW(ompi_keytable_t);
	    new->token = NULL;
	    new->key = ptr_seg->key;
	    ompi_list_append(&ompi_registry.freekeys, &new->item);
	    /* NEED TO RE-FIND PTR_SEG */
	    /* now remove the dictionary entry from the global registry dictionary*/
	    ompi_list_remove_item(&ompi_registry.segment_dict, &ptr_seg->item);
	    return(OMPI_SUCCESS);
	} else {  /* token not null, so need to find dictionary element to delete */
	    ptr_key = ompi_registry_finddictentry(segment, token);
	    if (NULL != ptr_key) {
		/* found key in dictionary */
		/* need to search this segment's registry to find all instances of key - then delete them */
		for (reg = (ompi_registry_core_t*)ompi_list_get_first(&seg->reg_list);
		     reg != (ompi_registry_core_t*)ompi_list_get_end(&seg->reg_list);
		     reg = (ompi_registry_core_t*)ompi_list_get_next(reg)) {
		    /* check the subscriber list */
		    for (subscriber = (ompi_subscribe_list_t*)ompi_list_get_first(&reg->subscriber);
			 (subscriber != (ompi_subscribe_list_t*)ompi_list_get_end(&reg->subscriber)
			  && (subscriber->id != ptr_key->key));
			 subscriber = (ompi_subscribe_list_t*)ompi_list_get_next(subscriber));
		    if (subscriber != (ompi_subscribe_list_t*)ompi_list_get_end(&reg->subscriber)) {
			ompi_list_remove_item(&reg->subscriber, &subscriber->item);
		    }
		    /* check the key list */
		    for (regkey = (ompi_keytable_t*)ompi_list_get_first(&reg->keys);
			 (regkey != (ompi_keytable_t*)ompi_list_get_end(&reg->keys))
			     && (regkey->key != ptr_key->key);
			 regkey = (ompi_keytable_t*)ompi_list_get_next(regkey));
		    if (regkey != (ompi_keytable_t*)ompi_list_get_end(&reg->keys)) {
			ompi_list_remove_item(&reg->keys, &regkey->item);
		    }
		    /* if this was the last key, then remove the registry entry itself */
		    if (0 == ompi_list_get_size(&reg->keys)) {
			while (0 < ompi_list_get_size(&reg->subscriber)) {
			    ompi_list_remove_last(&reg->subscriber);
			}
			prev = (ompi_registry_core_t*)ompi_list_get_prev(reg);
			ompi_list_remove_item(&seg->reg_list, &reg->item);
			reg = prev;
		    }
		}
		/* add key to this segment's freekey list */
		new = OBJ_NEW(ompi_keytable_t);
		new->token = NULL;
		new->key = ptr_key->key;
		ompi_list_append(&seg->freekeys, &new->item);
		/* now remove the dictionary entry from the segment's dictionary */
		ompi_list_remove_item(&seg->keytable, &ptr_key->item);
		return(OMPI_SUCCESS);
	    }
	    return(OMPI_ERROR); /* if we get here, then we couldn't find token in dictionary */
	}
    }
    return(OMPI_ERROR); /* if we get here, then we couldn't find segment */
}


int ompi_registry_definesegment(char *segment)
{
     if (0 == ompi_registry_definekey(segment, NULL)) {
 	return(OMPI_ERROR);
     }
     return(OMPI_SUCCESS);
}


int ompi_registry_put(bool overwrite, uint8_t *object, int size, char *segment, char *token, ...)
{
    va_list ap, ap1;
    int num_tokens=1, i;
    char *tokenel, **tokenlist, **tok_ptr;
    ompi_keytable_t *ptr_seg;
    ompi_registry_segment_t *seg;

#if __STDC__
    va_start(ap, token);
    va_start(ap1, token);
#else
    va_start(ap);
    va_start(ap1);
#endif

    /* protect ourselves against errors */
    if (NULL == segment || NULL == object || 0 == size || NULL == token) {
	return(OMPI_ERROR);
    }

    /* get list of tokens */
    while (NULL != (tokenel=va_arg(ap, char*))) {
	num_tokens++;
    }

    tokenlist = (char **)malloc(num_tokens);
    tok_ptr = tokenlist;
    *tok_ptr = strdup(token);
    tok_ptr++;
    while (NULL != (tokenel=va_arg(ap1, char*))) {
	*tok_ptr = strdup(tokenel);
	tok_ptr++;
    }
    va_end(ap);
    va_end(ap1);

    for (i=0, tok_ptr=tokenlist; i<num_tokens; i++) {
	fprintf(stderr, "put: token %s\n", *tok_ptr);
	tok_ptr++;
    }
    return(1);

    /* find the segment */
    seg = ompi_registry_findseg(segment);
    if (NULL != seg) {
    }
    return(OMPI_SUCCESS);
}


ompi_registry_value_t *ompi_registry_get(char *segment, char *token, ...)
{
}


int ompi_registry_del(char *segment, char *token, ...)
{
}


ompi_keytable_t *ompi_registry_index(char *segment, ...)
{
}


ompi_keytable_t *ompi_registry_segment_index(void)
{
}


int ompi_registry_subscribe(int caller, uint8_t action, char *segment, char *token, ...)
{
}


int ompi_registry_unsubscribe(int caller, uint8_t action, char *segment, char *token, ...)
{
}
