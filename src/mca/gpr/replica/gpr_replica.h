/* -*- C -*-
 * 
 * $HEADER$
 *
 */
#ifndef GPR_REPLICA_H
#define GPR_REPLICA_H


#include "ompi_config.h"
#include "include/types.h"
#include "include/constants.h"
#include "class/ompi_list.h"
#include "mca/gpr/base/base.h"

/*
 * typedefs needed in replica component
 */
typedef uint32_t mca_gpr_replica_key_t;

struct ompi_registry_t {
    ompi_list_t registry;
    ompi_list_t segment_dict;
    mca_gpr_replica_key_t lastkey;
    ompi_list_t freekeys;
};
typedef struct ompi_registry_t ompi_registry_t;

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

OBJ_CLASS_DECLARATION(ompi_keytable_t);

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

OBJ_CLASS_DECLARATION(ompi_keylist_t);

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

OBJ_CLASS_DECLARATION(ompi_subscribe_list_t);

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
typedef struct ompi_replica_list_t ompi_replica_list_t;

OBJ_CLASS_DECLARATION(ompi_replica_list_t);

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

OBJ_CLASS_DECLARATION(ompi_registry_core_t);

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

OBJ_CLASS_DECLARATION(ompi_registry_segment_t);


/*
 * globals needed within component
 */
extern ompi_registry_t mca_gpr_head;

/*
 * Module open / close
 */
int mca_gpr_replica_open(void);
int mca_gpr_replica_close(void);


/*
 * Startup / Shutdown
 */
mca_gpr_base_module_t *mca_gpr_replica_init(bool *allow_multi_user_threads, bool *have_hidden_threads, int *priority);
int mca_gpr_replica_finalize(void);

/*
 * Implemented registry functions
 */

int gpr_replica_define_segment(char *segment);
int gpr_replica_delete_segment(char *segment);
int gpr_replica_put(ompi_registry_mode_t mode, char *segment,
		    char **tokens, ompi_registry_object_t *object,
		    ompi_registry_object_size_t size);
int gpr_replica_delete(ompi_registry_mode_t mode,
		       char *segment, char **tokens);
ompi_registry_index_t* gpr_replica_index(char *segment);
int gpr_replica_subscribe(ompi_process_name_t *caller, ompi_registry_mode_t mode,
			  ompi_registry_notify_action_t action,
			  char *segment, char **tokens);
int gpr_replica_unsubscribe(ompi_process_name_t *caller, ompi_registry_mode_t mode,
			    char *segment, char **tokens);
ompi_registry_value_t* gpr_replica_get(ompi_registry_mode_t mode,
				       char *segment, char **tokens);

#endif
