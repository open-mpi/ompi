/*
 * $HEADER$
 */
/** @file:
 *
 * The Open MPI general purpose registry.
 *
 * The Open MPI system contains a general purpose registry for use by both
 * applications and internal systems to dynamically share information. For
 * speed purposes, the registry is divided into "segments", each labelled
 * with an appropriate "token" string that describes its contents. Segments
 * are automatically provided for the "universe" and for each MPI CommWorld.
 * At this time, all segments may be accessed by any application within the universe, thus
 * providing a mechanism for cross-CommWorld communications (with the requirement
 * that all participating CommWorlds must reside within the same universe). In the future,
 * some form of security may be provided to limit access privileges between
 * segments.
 *
 * Within each registry segment, there exists a list of objects that have
 * been "put" onto the registry. Each object must be tagged with at least
 * one token, but may be tagged with as many tokens as the creator desires.
 * Retrieval requests must specify the segment and at least one token, but
 * can specify an arbitrary number of tokens to describe the search. The registry
 * will return a list of all objects that meet the search criteria.
 *
 * Tokens are defined as character strings, thus allowing for clarity in
 * the program. However, for speed purposes, tokens are translated into
 * integer keys prior to storing an object. A table of token-key pairs
 * is independently maintained for each registry segment. Users can obtain
 * an index of tokens within a dictionary by requesting it through the ompi_registry_index()
 * function.
 *
 * The registry also provides a subscription capability whereby a caller
 * can subscribe to a stored object and receive notification when various actions
 * are performed on that object. Currently supported actions include modification,
 * the addition of another subscriber, and deletion. Notifications are sent via
 * the OOB communication channel.
 *
 * 
 */

/*
 * includes
 */
#include <sys/types.h>
#include <sys/inttypes.h>
#include <limits.h>

#include "ompi_config.h"
#include "include/constants.h"
#include "class/ompi_list.h"

/** Define the notification actions for the subscription system
 */
/** Notifies subscriber when object is modified */
#define OMPI_REGISTRY_NOTIFY_MODIFICATION     0x0001
/** Notifies subscriber when another subscriber is added */
#define OMPI_REGISTRY_NOTIFY_ADD_SUBSCRIBER   0x0002
/** Notifies subscriber when object is removed from registry */
#define OMPI_REGISTRY_NOTIFY_DELETE           0x0004
/** Notifies subscriber upon any action - effectively an OR of all other flags */
#define OMPI_REGISTRY_NOTIFY_ALL              0xffff


/** Define the action bit-masks for registry put and gets.
 */
/** Overwrite permission */
#define OMPI_REGISTRY_OVERWRITE       0x0001
/** AND tokens together for search results */
#define OMPI_REGISTRY_AND             0x0002
/** OR tokens for search results */
#define OMPI_REGISTRY_OR              0x0004


/*
 * structures
 */


/** Return value structure for registry requests.
 * A request for information stored within the registry returns a linked list of values that
 * correspond to the provided tokens. The linked list is terminated by a "next" value of NULL.
 * Each link in the list contains a pointer to a copy of the registry object, and the size
 * of that object in bytes. Note that the pointer is to a \em copy of the object, and not
 * to the registry object itself. This prevents inadvertent modification of the registry, but
 * may require the recipient to release the structure's memory when done.
 */
struct ompi_registry_value_t {
    ompi_list_item_t item;    /**< Allows this item to be placed on a list */
    char *segment;            /**< Name of segment this object came from */
    ompi_key_table_t keylist;  /**< List of keys describing the object */
    uint8_t *object;   /**< Pointer to object being returned */
    int object_size;   /**< Size of returned object, in bytes */
};
typedef struct ompi_registry_value_t ompi_registry_value_t;

/* constructor - used to initialize state of keytable instance */
static void ompi_registry_value_construct(ompi_registry_value_t* reg_val)
{
    reg_val->object = NULL;
    reg_val->object_size = -1;
}

/* destructor - used to free any resources held by instance */
static void ompi_registry_value_destructor(ompi_registry_value_t* reg_val)
{
    if (NULL != reg_val->object) {
	free(reg_val->object);
    }
}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
		   ompi_registry_value_t,  /* type name */
		   ompi_list_item_t, /* parent "class" name */
		   ompi_registry_value_construct, /* constructor */
		   ompi_registry_value_destructor); /* destructor */



/*
 * external function prototypes
 */

/** Define a new registry segment.
 * The ompi_registry_definesegment() function allows the caller to create a new registry
 * segment with the specified name. Each segment is given its own token-key dictionary and
 * object storage list. There is no limit nor restrictions on the number of segments
 * that can be created and who can create them, or for what they can be used. Attempts to
 * define a segment with a name that already exists will return an error.
 *
 * @param segment A pointer to a character string containing the name of the segment
 * to be created.
 *
 * @retval OMPI_SUCCESS Indicates that the operation was successfully completed.
 * @retval OMPI_ERROR Indicates that the operation failed - most likely due to the
 * prior existence of a segment with an identical name.
 */
int ompi_registry_definesegment(char *segment);

/** Define multiple new registry segments.
 * Same functionality as the single segment request ompi_registry_definesegment().
 *
 * @param segments A pointer to an array of character strings containing the names of
 * the segments to be created. The last entry in the array must be a string of length
 * zero ("\0").
 *
 * @retval OMPI_SUCCESS Indicates that the operation was successfully completed.
 * @retval OMPI_ERROR Indicates that the operation failed - most likely due to the
 * prior existence of a segment with an identical name.
 */
int ompi_registry_definesegmentv(char **segments);

/** Place an object on the registry.
 * The ompi_registry_put() function places an object on the registry within the specified
 * registry segment. At least one token describing the object must be provided - an unlimited
 * number of additional tokens may also be provided. Note that placing an object on the
 * registry where another object with all tokens identical already exists will cause the
 * prior object to either: (a) be replaced with the new object, if the action bit-mask
 * includes overwrite permission, or (b) generate an error, if the action bit-mask does
 * not include overwrite permission.
 *
 * @param action A bit-mask constructed from the defined action values that controls
 * the behaviour of the function.
 *
 * @param segment A pointer to a character string stating the registry segment to be used.
 *
 * @param tokens An array of one or more pointers to characters string containing a token
 * that defines the object
 * being stored.
 *
 * CAUTION: The array of tokens MUST end with a string of zero length ("\0")! Failure to correctly
 * terminate the array will result
 * in segmentation violations or bus errors, thus causing the program to catastrophically fail.
 *
 * @param object A pointer to pre-packed buffer to be stored on the registry. The registry will create
 * a copy of the object. Since the registry has no knowledge of the object's internal structure,
 * the object must be pre-packed by the caller to ensure accurate communication to other
 * requestors on systems of different architecture.
 *
 * @param size Integer value of the size of the object being passed, in bytes.
 *
 * @retval OMPI_SUCCESS Indicates that the operation was successful.
 * @retval OMPI_ERROR Indicates that the registry was unable to store the object - most
 * likely due to specifying a non-existent segment or lack of available memory.
 */
int ompi_registry_put(uint16_t action, char *segment, char **tokens, uint8_t *object, int size);

/** Place multiple objects on the registry.
 * Same functionality as the single object ompi_registry_put() function.
 *
 * @param actions A pointer to an array of bit-masks constructed from the defined action value
 * that control the behaviour of the function. The number of bit-mask values must equal the
 * number of segments provided.
 *
 * @param segments A pointer to an array of character strings stating the registry segment to
 * be used for each object. The last entry in the array must point to a string of zero length.
 *
 * CAUTION: Failure to correctly
 * terminate the array will result
 * in segmentation violations or bus errors, thus causing the program to catastrophically fail.
 *
 * @param tokens A two-dimensional array of one or more pointers to character strings, each row
 * containing at least one token that defines the corresponding object being stored. The number
 * of rows in the array must match the number of segments provided.
 *
 * CAUTION: Each row in the array of tokens MUST end with a string of zero length ("\0")!
 * Failure to correctly
 * terminate the array will result
 * in segmentation violations or bus errors, thus causing the program to catastrophically fail.
 *
 * @param object A pointer to an array of pre-packed buffers to be stored on the registry.
 * The registry will create
 * a copy of each object. Since the registry has no knowledge of the object's internal structure,
 * each object must be pre-packed by the caller to ensure accurate communication to other
 * requestors on systems of different architecture. The number of entries in the array must
 * match the number of segments provided.
 *
 * @param size Pointer to an array of integer values of the size of the objects being passed,
 * in bytes. The number of entries in the array must match the number of segments provided.
 *
 * @retval OMPI_SUCCESS Indicates that the operation was successful.
 * @retval OMPI_ERROR Indicates that the registry was unable to store the objects - most
 * likely due to specifying a non-existent segment or lack of available memory.
 */
int ompi_registry_putv(uint16_t *actions, char **segments, char ***tokens, uint8_t **objects, int *sizes);

/** Retrieve an object from the registry.
 * The ompi_registry_get() function retrieves one or more packed buffers, each containing a copy of an
 * object previously stored on
 * the registry. The caller must provide the registry segment containing the object, and at
 * least one token that describes it. An unlimited number of additional tokens describing the
 * object may be provided. The function will return a linked list of all objects whose description
 * contains: (a) the entire specified collection of tokens, if the action bit-mask includes
 * the AND bit, or (b) any one of the specified tokens, if the action bit-mask includes
 * the OR bit. Failure to provide either of these two bits defaults to the AND condition.
 *
 * @param action A bit-mask constructed from the defined registry action flags that controls
 * the behaviour of the function, as previously described.
 *
 * @param segment Pointer to a character string defining the segment of the registry.
 *
 * @param tokens Pointer to an array of pointers to character strings containing one or
 * more tokens describing the object to be retrieved.
 *
 * CAUTION: The array must end with a string of zero length ("\0")! Failure to correctly
 * terminate the array will result
 * in segmentation violations or bus errors, thus causing the program to catastrophically fail.
 *
 * @retval object Pointer to a linked list of ompi_registry_value_t structures, each containing
 * the name of the segment,
 * a linked list of the tokens that describe the object,
 * a pointer to a packed buffer containing a copy of the object retrieved from the registry,
 * and the size of the object in bytes. The caller must unpack the buffer to access
 * the information in the object.
 * @retval NULL Indicates that no object meeting the provided specifications could not be found.
 */
ompi_registry_value_t *ompi_registry_get(uint16_t action, char *segment, char **tokens);

/** Perform multiple retrieves from the registry.
 * The ompi_registry_getv() function allows the caller to execute multiple ompi_registry_get()
 * calls in a single call, thus saving message overhead for faster response.
 *
 * @param action An array of bit-masks constructed from the defined registry action flags that control
 * the behaviour of the function for each requested retrieval, as previously described.
 *
 * @param segment An array of pointers to character strings defining the segment of the registry
 * for each retrieval. The last value in the array must be a string of zero length.
 *
 * CAUTION: Failure to correctly
 * terminate the array will result
 * in segmentation violations or bus errors, thus causing the program to catastrophically fail.
 *
 * @param tokens A two-dimensional array of one or more pointers to character strings, each row
 * containing at least one token that defines the corresponding object being stored. The number
 * of rows in the array must match the number of segments provided.
 *
 * CAUTION: Each row in the array of tokens MUST end with a string of zero length ("\0")!
 * Failure to correctly
 * terminate the array will result
 * in segmentation violations or bus errors, thus causing the program to catastrophically fail.
 *
 * @retval object Pointer to a linked list of ompi_registry_value_t structures, each containing
 * the name of the segment, a linked list of the tokens that describe the object,
 * a pointer to a packed buffer containing a copy of the object retrieved from the registry,
 * and the size of the object in bytes. The caller must unpack the buffer to access
 * the information in the object.
 * @retval NULL Indicates that no object meeting the provided specifications could not be found
 * for any of the requested retrievals.
 */
ompi_registry_value_t *ompi_registry_getv(uint16_t *action, char **segment, char ***tokens);


/** Delete an object from the registry.
 * The ompi_registry_del() function removes an object that was previously stored on the registry.
 * The caller must provide the registry segment containing the object, and at
 * least one token that describes it. An unlimited number of additional tokens describing the
 * object may be provided.
 *
 * CAUTION: The function will delete ALL objects that match the search criteria.
 *
 * CAUTION: The ompi_registry_del() function call MUST end with a NULL parameter! The C variable
 * argument system does not provide for a mechanism by which we can determine the number of
 * arguments that were passed. Failure to terminate the argument list with a NULL will result
 * in segmentation violations or bus errors, thus causing the program to catastrophically fail.
 *
 * @param segment Pointer to a character string defining the segment of the registry.
 * @param token Pointer to a character string containing the token to be deleted.
 * @param tokens... Additional tokens (provided as pointers to character strings) can be
 * provided to further identify the object being deleted.
 * @param NULL The last parameter in the function call MUST be a NULL to terminate the
 * variable list of arguments.
 * @retval OMPI_SUCCESS Indicates that the operation was successful.
 * @retval OMPI_ERROR Indicates that the registry was unable to delete the object - most
 * likely due to specifying a non-existent segment or object.
 */
int ompi_registry_delete(char *segment, char **tokens);

/** Obtain an index of the registry token-key dictionary.
 * The ompi_registry_index() function provides a list of the token-key pairs within
 * a specified dictionary. The caller must provide the name of the segment being
 * queried - this will return a linked list of all token-key pairs within that segment's
 * dictionary. Alternatively, the caller may also provide an unlimited number of tokens
 * which the caller would like to have translated - the function will then return a
 * linked list of token-key pairs. Any tokens not found will be ignored.
 *
 * CAUTION: The ompi_registry_index() function call MUST end with a NULL parameter! The C variable
 * argument system does not provide for a mechanism by which we can determine the number of
 * arguments that were passed. Failure to terminate the argument list with a NULL will result
 * in segmentation violations or bus errors, thus causing the program to catastrophically fail.
 *
 * @param segment Pointer to a character string defining the segment of the registry.
 * @param tokens... Additional tokens (provided as pointers to character strings) can be
 * provided that the caller would like to have translated.
 * @param NULL If segment parameter is NULL, then segment index returned.
 *
 * @retval keyvalues A pointer to a linked list of token-key pairs. Any tokens not found
 * will be ignored and will, therefore, not be included in the returned list.
 * @retval NULL Indicates that the operation failed - most likely caused by failing to specify
 * any token that exists within the specified segment, or a non-existent segment.
 */
ompi_keytable_t *ompi_registry_index(char *segment);

/** Subscribe to a registry object.
 * The ompi_registry_subscribe() function allows the caller to be notified when specific actions
 * are taken on the specified object. Notification will be sent via the OOB communication channel.
 * The caller must provide an ID that allows the OOB to properly route the notification message.
 *
 * CAUTION: The ompi_registry_subscribe() function call MUST end with a NULL parameter! The C variable
 * argument system does not provide for a mechanism by which we can determine the number of
 * arguments that were passed. Failure to terminate the argument list with a NULL will result
 * in segmentation violations or bus errors, thus causing the program to catastrophically fail.
 *
 * @param caller The ID of the caller - used to route any subsequent notifications.
 * @param action A bit-mask value formed using the OMPI_REGISTRY_NOTIFY flags that indicates
 * the action that shall trigger notification of the caller.
 * @param segment Pointer to a character string defining the segment of the registry.
 * @param token Pointer to a character string containing the token of the object to which
 * the caller is subscribing.
 * @param tokens... Additional tokens (provided as pointers to character strings) can be
 * provided to further identify the object to which the caller is subscribing.
 * @param NULL The last parameter in the function call MUST be a NULL to terminate the
 * variable list of arguments.
 *
 * @retval OMPI_SUCCESS Indicates that the operation was successful.
 * @retval OMPI_ERROR Indicates that the operation failed - most likely caused by specifying
 * an object that did not exist within the specified segment, or a non-existent segment.
 */
int ompi_registry_subscribe(int caller, uint8_t action, char *segment, char **tokens);

/** Unsubscribe from a registry object.
 *
 * CAUTION: The ompi_registry_put() function call MUST end with a NULL parameter! The C variable
 * argument system does not provide for a mechanism by which we can determine the number of
 * arguments that were passed. Failure to terminate the argument list with a NULL will result
 * in segmentation violations or bus errors, thus causing the program to catastrophically fail.
 *
 * @param caller The ID of the caller wishing to remove its subscription to the object.
 * @param segment Pointer to a character string defining the segment of the registry.
 * @param token Pointer to a character string containing the token of the object to which
 * the caller is subscribed.
 * @param tokens... Additional tokens (provided as pointers to character strings) can be
 * provided to further identify the object to which the caller is subscribed.
 * @param NULL The last parameter in the function call MUST be a NULL to terminate the
 * variable list of arguments.
 *
 * @retval OMPI_SUCCESS Indicates that the operation was successful. Note that this value will
 * also be returned if the caller was not previously subscribed to the specified object since an
 * unsubscribe request would have resulted in the same end condition.
 * @retval OMPI_ERROR Indicates that the operation failed - most likely caused by specifying
 * an object that did not exist within the specified segment, or a non-existent segment.
 */
int ompi_registry_unsubscribe(int caller, uint8_t action, char *segment, char **tokens);
