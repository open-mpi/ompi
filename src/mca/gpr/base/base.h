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
#include <stdint.h>

#include "ompi_config.h"
#include "class/ompi_list.h"
#include "mca/gpr/gpr.h"


/*
 * globals
 */

extern mca_gpr_t ompi_registry;

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
    ompi_registry_object_t *object;   /**< Pointer to object being returned */
    int object_size;   /**< Size of returned object, in bytes */
};
typedef struct ompi_registry_value_t ompi_registry_value_t;

OBJ_CLASS_DECLARATION(ompi_registry_value_t);

/*
 * external functions - here for purely documentation purposes
 */

/** @fn int ompi_registry.definesegment(char *segment)
 *
 * Define a new registry segment.
 * The ompi_registry.definesegment() function allows the caller to create a new registry
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

/** @fn int ompi_registry.deletesegment(char *segment)
 *
 * Delete a registry segment.
 * The ompi_registry.deletesegment() function removes a segment - and ALL of its stored
 * objects - from the registry.
 *
 * @param segment A pointer to a character string containing the name of the segment
 * to be created.
 *
 * @retval OMPI_SUCCESS Indicates that the operation was successfully completed.
 * @retval OMPI_ERROR Indicates that the operation failed - most likely due to the
 * non-existence of the segment.
 */

/** @fn int ompi_registry.put(ompi_registry_mode_t mode, char *segment, char **tokens, uint8_t *object, int size)
 *
 * Place an object on the registry.
 * The ompi_registry.put() function places an object on the registry within the specified
 * registry segment. At least one token describing the object must be provided - an unlimited
 * number of additional tokens may also be provided. Note that placing an object on the
 * registry where another object with all tokens identical already exists will cause the
 * prior object to either: (a) be replaced with the new object, if the action bit-mask
 * includes overwrite permission, or (b) generate an error, if the action bit-mask does
 * not include overwrite permission.
 *
 * @param mode A bit-mask constructed from the defined mode values that controls
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

/** @fn ompi_registry_value_t* ompi_registry.get(ompi_registry_mode_t mode, char *segment, char **tokens)
 *
 * Retrieve an object from the registry.
 * The ompi_registry.get() function retrieves one or more packed buffers, each containing a copy of an
 * object previously stored on
 * the registry. The caller must provide the registry segment containing the object, and at
 * least one token that describes it. An unlimited number of additional tokens describing the
 * object may be provided. The function will return a linked list of all objects whose description
 * contains the specified tokens, with the tokens used as defined by the specified mode.
 *
 * @param mode A bit-mask constructed from the defined registry mode flags that controls
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

/** @fn int ompi_registry.delete(ompi_registry_mode_t mode, char *segment, char **tokens)
 *
 * Delete an object from the registry.
 * The ompi_registry.delete() function removes an object that was previously stored on the registry.
 * The caller must provide the registry segment containing the object, and at
 * least one token that describes it. An unlimited number of additional tokens describing the
 * object may be provided.
 *
 * @param mode A bit-mask constructed from the defined registry mode flags that controls
 * the behaviour of the function, as previously described.
 *
 * CAUTION: The function will delete ALL objects that match the search criteria.
 *
 * @param segment Pointer to a character string defining the segment of the registry.
 * @param tokens Pointer to an array of pointers to character strings containing one or
 * more tokens describing the object to be retrieved.
 *
 * CAUTION: The array must end with a string of zero length ("\0")! Failure to correctly
 * terminate the array will result
 * in segmentation violations or bus errors, thus causing the program to catastrophically fail.
 *
 * @retval OMPI_SUCCESS Indicates that the operation was successful.
 * @retval OMPI_ERROR Indicates that the registry was unable to delete the object - most
 * likely due to specifying a non-existent segment or object.
 */

/** @fn ompi_keytable_t* ompi_registry.index(char *segment)
 *
 * Obtain an index of the registry dictionary.
 * The ompi_registry.index() function provides a list of the tokens within
 * a specified dictionary. The caller provides the name of the segment being
 * queried - this will return a linked list of all tokens within that segment's
 * dictionary - or a NULL to return the list of tokens in the universe directory.
 *
 * @param segment Pointer to a character string defining the segment of the registry.
 * @param NULL If segment parameter is NULL, then index of universe dictionary is returned.
 *
 * @retval keyvalues A pointer to a linked list of tokens.
 * @retval NULL Indicates that the operation failed - most likely caused by specifying a non-existent segment.
 */

/** @fn int ompi_registry.subscribe(ompi_registry_mode_t mode, ompi_registry_action_t action, char *segment, char **tokens)
 *
 * Subscribe to a registry object.
 * The ompi_registry.subscribe() function allows the caller to be notified when specific actions
 * are taken on the specified object. Notification will be sent via the OOB communication channel.
 *
 * @param mode A bit-mask constructed from the defined registry mode flags that controls
 * the behaviour of the function, as previously described.
 * @param action A bit-mask value formed using the OMPI_REGISTRY_NOTIFY flags that indicates
 * the action that shall trigger notification of the caller.
 * @param segment Pointer to a character string defining the segment of the registry.
 * @param tokens Pointer to an array of pointers to character strings containing one or
 * more tokens describing the object to be retrieved.
 *
 * CAUTION: The array must end with a string of zero length ("\0")! Failure to correctly
 * terminate the array will result
 * in segmentation violations or bus errors, thus causing the program to catastrophically fail.
 *
 * @retval OMPI_SUCCESS Indicates that the operation was successful.
 * @retval OMPI_ERROR Indicates that the operation failed - most likely caused by specifying
 * an object that did not exist within the specified segment, or a non-existent segment.
 */

/** @fn int ompi_registry.unsubscribe(ompi_registry_mode_t mode, char *segment, char **tokens)
 *
 * Unsubscribe from a registry object.
 * The ompi_registry.unsubscribe() function allows the caller to cancel a subscription to
 * one or more objects on the registry.
 *
 * @param mode A bit-mask constructed from the defined registry mode flags that controls
 * the behaviour of the function, as previously described.
 * @param segment Pointer to a character string defining the segment of the registry.
 * @param tokens Pointer to an array of pointers to character strings containing one or
 * more tokens describing the object to be retrieved.
 *
 * CAUTION: The array must end with a string of zero length ("\0")! Failure to correctly
 * terminate the array will result
 * in segmentation violations or bus errors, thus causing the program to catastrophically fail.
 *
 * @retval OMPI_SUCCESS Indicates that the operation was successful. Note that this value will
 * also be returned if the caller was not previously subscribed to the specified object since an
 * unsubscribe request would have resulted in the same end condition.
 * @retval OMPI_ERROR Indicates that the operation failed - most likely caused by specifying
 * an object that did not exist within the specified segment, or a non-existent segment.
 */

/** @fn ompi_registry_buf_t *ompi_registry.getbuf(size_t size)
 *
 * Get a buffer for packing a multi-command request.
 *
 * The ompi_registry.getbuf() function creates a buffer for packing multiple commands
 * into a single registry request. Packing multiple commands saves on messaging overhead,
 * thus potentially providing faster response. Each command packed into the buffer will
 * be processed in sequence, and the result of the command packed into a "response" buffer
 * that will subsequently be returned. The results must then be unpacked by the caller
 * in the reverse order of the original commands.
 *
 * Note: any command that returns an error condition will automatically terminate processing
 * of the packed command sequence. In such cases, the returned "response" buffer will
 * contain the results of all commands up to that point, with the error conditions
 * from the last command being the first item in the buffer.
 *
 * @param size The size of the requested buffer in bytes. If a value of zero is provided,
 * the function will allocate an unlimited buffer.
 *
 * @retval buffer A pointer to the requested buffer.
 * @retval NULL Indicates that a buffer could not be obtained - most likely due to lack
 * of sufficient available memory or a problem in the OOB subsystem.
 */

/** @fn int ompi_registry.packbuf(ompi_registry_buf_t *buf, void *ptr, size_t num_items, ompi_registry_bufdata_t datatype)
 *
 * Pack non-string data types into a buffer.
 * Pack arbitrary bytes and previously packed buffers for recursive
 * packing commands.
 *
 * @param buf A pointer to the buffer to which the packed info is to be added. A NULL value will
 * cause the function to return OMPI_ERROR, but not fail.
 *
 * @param ptr A pointer (cast as void) to the data to be packed into the buffer.
 *
 * @param num_items The number of items in the data to be packed into the buffer. The items must all
 * be of the same type.
 *
 * @param datatype One of the defined datatype flags indicating the type of the data to be packed
 * into the buffer.
 *
 * @retval OMPI_SUCCESS Indicates that the operation was successful.
 * @retval OMPI_ERROR Indicates that the operation failed.
 */

/** @fn int ompi_registry.pack_string(ompi_registry_buf_t *buf, char *string)
 *
 * Pack a string into a buffer.
 * Due to their use as tokens in the registry, the packing of strings into the buffer is a fairly
 * common operation. This function packs strings - and only strings - into the specified buffer.
 *
 * @param buf A pointer to the buffer to which the string is being added. A NULL value will cause
 * the function to return OMPI_ERROR, but not fail.
 * @param string A pointer to the string to be added to the buffer.
 *
 * @retval OMPI_SUCCESS Indicates that the operation was successful.
 * @retval OMPI_ERROR Indicates that the operation failed.
 */

/** @fn int ompi_registry.unpack_string(ompi_registry_buf_t *buf, char *string, size_t maxlen)
 *
 * Unpack a string from buffer.
 * Due to their use as tokens in the registry, the packing of strings into the buffer is a fairly
 * common operation. This function unpacks strings - and only strings - from the specified buffer.
 *
 * @param buf A pointer to the buffer from which the string is to be extracted. A NULL value will cause
 * the function to return OMPI_ERROR, but not fail.
 * @param string A pointer to where the string is to be placed.
 * @param maxlen The maximum length of the string that can be placed into the provided storage location.
 * Strings longer than the maximum length will be truncated, but the function will still return
 * "success".
 *
 * @retval OMPI_SUCCESS Indicates that the operation was successful.
 * @retval OMPI_ERROR Indicates that the operation failed.
 */

/** @fn int ompi_registry.unpack_buf(ompi_registry_buf_t *buf, void *ptr, size_t num_items, ompi_registry_bufdata_t datatype)
 *
 * Unpack non-string data types from a buffer.
 * Unpack arbitrary bytes and previously packed buffers for recursive
 * packing commands.
 *
 * @param buf A pointer to the buffer from which the packed info is to be extracted. A NULL value will
 * cause the function to return OMPI_ERROR, but not fail.
 *
 * @param ptr A pointer (cast as void) to the location where the packed data is to be placed.
 *
 * @param num_items The number of items in the data to be extracted from the buffer. The items must all
 * be of the same type.
 *
 * @param datatype One of the defined datatype flags indicating the type of the data to be extracted
 * from the buffer.
 *
 * @retval OMPI_SUCCESS Indicates that the operation was successful.
 * @retval OMPI_ERROR Indicates that the operation failed.
 */

/** @fn int ompi_registry.sendbuf(ompi_process_name_t *target, ompi_registry_buf_t *buf, bool freebuf)
 *
 * Send a previously packed buffer to a target process.
 * Once a buffer has been packed with commands, this function will send it to the target process.
 *
 * @param target The name of the process to which the buffer is to be sent.
 * @param buf A pointer to the buffer to be sent. A NULL value will cause the function to return
 * OMPI_ERROR, but not fail.
 * @param freebuf A boolean value indicating if the buffer is to be released (true) or not (false)
 * once the send has been completed.
 *
 * @retval OMPI_SUCCESS Indicates that the operation was successful.
 * @retval OMPI_ERROR Indicates that the operation failed.
 */
