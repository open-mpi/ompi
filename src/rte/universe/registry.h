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
 * is independetly maintained for each registry segment. Users can obtain
 * an index of token-key values by requesting it through the ompi_registry_index()
 * function, and can define/delete token-key values using the ompi_registry_definekey()
 * and ompi_registry_deletekey() functions, respectively.
 *
 * The registry also provides a subscription capability whereby a caller
 * can subscribe to a stored object and receive notification when various actions
 * are performed on that object. Currently supported actions include modification,
 * the addition of another subscriber, and deletion. Notifications are sent via
 * the OOB communication channel.
 *
 * 
 */

/** Define the notification actions for the subscription system
 */
/** Notifies subscriber when object is modified */
#define OMPI_REGISTRY_NOTIFY_MODIFICATION     0x01
/** Notifies subscriber when another subscriber is added */
#define OMPI_REGISTRY_NOTIFY_ADD_SUBSCRIBER   0x02
/** Notifies subscriber when object is removed from registry */
#define OMPI_REGISTRY_NOTIFY_DELETE           0x04
/** Notifies subscriber upon any action - effectively an OR of all other flags */
#define OMPI_REGISTRY_NOTIFY_ALL              0xff

/** Dictionary of token-key pairs.
 * This structure is used to create a linked list of token-key pairs. All calls to
 * registry functions pass char string tokens for programming clarity - the ompi_keytable
 * structure is used to translate those strings into an integer key value, thus allowing
 * for faster searches of the registry.
 */
struct ompi_keytable_t {
    char *token; /**< Char string that defines the key */
    int key;     /**< Numerical value assigned by registry to represent token string */
    struct ompi_keytable_t *next;
};
typedef struct ompi_keytable_t ompi_keytable_t;

/** List of keys that describe a stored object.
 * Each object stored in the registry may have as many keys describing it as the
 * creator desires. This structure is used to create a linked list of keys
 * associated with each object.
 */
struct ompi_keylist_t {
    int key;     /**< Numerical key that defines stored object */
    struct ompi_keylist_t *next;
};
typedef struct ompi_keylist_t ompi_keylist_t;

/** List of subscribers to a stored object.
 * Each object can have an arbitrary number of subscribers desiring notification
 * upon specified actions being performed against the object. This structure is
 * used to create a linked list of subscribers for objects.
 */
struct ompi_subscribe_list_t {
    int id;   /**< ID of the subscriber */
    uint8_t action;  /**< Bit-mask of actions that trigger notification */
    struct ompi_subscribe_list_t *next;
};
typedef struct ompi_subscribe_list_t ompi_subscribe_list_t;

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
    ompi_keylist_t *keys;   /**< Linked list of keys that define stored object */
    int object_size;      /**< Size of stored object, in bytes */
    uint8_t *object;      /**< Pointer to stored object */
    ompi_subscribe_list_t *subscriber;  /**< Linked list of subscribers to this object */
    struct ompi_registry_core_t *next;
};
typedef struct ompi_registry_core_t ompi_registry_core_t;

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
    int segment;    /**< ID of registry segment */
    ompi_registry_core_t *reg_list;   /**< Linked list of stored objects within this segment */
    ompi_keytable_t *keytable;   /**< Token-key dictionary for this segment */
    struct ompi_registry_segment_t *next;
};
typedef struct ompi_registry_segment_t ompi_registry_segment_t;

/** Return value structure for registry requests.
 * A request for information stored within the registry returns a linked list of values that
 * correspond to the provided tokens. The linked list is terminated by a "next" value of NULL.
 * Each link in the list contains a pointer to a copy of the registry object, and the size
 * of that object in bytes. Note that the pointer is to a \em copy of the object, and not
 * to the registry object itself. This prevents inadvertent modification of the registry, but
 * may require the recipient to release the structure's memory when done.
 */
struct ompi_registry_value_t {
    uint8_t *object;   /**< Pointer to object being returned */
    int object_size;   /**< Size of returned object, in bytes */
    struct ompi_registry_value_t *next;
};
typedef struct ompi_registry_value_t ompi_registry_value_t;

/** Retrieve a registry key value for a given token string.
 * The ompi_registry_getkey() function is used to translate a token string for a particular
 * segment of the registry into its associated (integer) key value.
 *
 * @param segment Pointer to a character string defining the segment of the registry being queried.
 * @param token Pointer to a character string containing the token to be translated.
 *
 * @retval key Integer value corresponding to the specified token within the specified segment.
 * @retval -1 Indicates that the segment and/or token could not be found.
 */
int ompi_registry_getkey(char *segment, char *token);

/** Add a token to a segment's dictionary.
 * The ompi_registry_definekey() function allows the addition of a new definition to
 * the registry's token-key dictionaries. The specified token is assigned an integer
 * value within the specified segment, and the entry is added to the segment's token-key
 * dictionary.
 *
 * @param segment Pointer to a character string defining the segment of the registry being queried.
 * @param token Pointer to a character string containing the token to be translated.
 *
 * @retval key Integer value corresponding to the specified token within the specified segment.
 * @retval -1 Indicates that the entry could not be created.
 */
int ompi_registry_definekey(char *segment, char *token);

/** Delete a token from a segment's dictionary.
 * The ompi_registry_deletekey() function allows the removal of a definition from the
 * registry's token-key dictionaries. This should be used with caution! Deletion of
 * a token-key pair causes the registry to search through all entries within that segment
 * for objects that include the specified token-key pair in their description. The reference
 * is subsequently removed, and any object for which this was the SOLE key will also
 * be removed from the registry!
 *
 * @param segment Pointer to a character string defining the segment of the registry.
 * @param token Pointer to a character string containing the token to be deleted.
 *
 * @retval OMPI_SUCCESS Indicating that the operation was successful.
 * @retval OMPI_ERROR Indicates that the operation failed - most likely caused by specifying
 * a token that did not exist within the specified segment, or a non-existent segment.
 */
int ompi_registry_deletekey(char *segment, char *token);

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

/** Place an object on the registry.
 * The ompi_registry_put() function places an object on the registry within the specified
 * registry segment. At least one token describing the object must be provided - an unlimited
 * number of additional tokens may also be provided. Note that placing an object on the
 * registry where another object with all tokens identical already exists will cause the
 * prior object to be replaced with the new object.
 *
 * CAUTION: The ompi_registry_put() function call MUST end with a NULL parameter! The C variable
 * argument system does not provide for a mechanism by which we can determine the number of
 * arguments that were passed. Failure to terminate the argument list with a NULL will result
 * in segmentation violations or bus errors, thus causing the program to catastrophically fail.
 *
 * @param object A pointer to the object to be stored on the registry. The registry will create
 * a copy of the object. Since the registry has no knowledge of the object's internal structure,
 * the object is stored as an object of uint8_t (unsigned 8-bit int) type. The pointer must be cast
 * into the uint8_t type when passed.
 * @param size Integer value of the size of the object being passed, in bytes.
 * @param segment A pointer to a character string stating the registry segment to be used.
 * @param token A pointer to a character string containing a token that defines the object
 * being stored. This token can later be used to retrieve the object. The registry uses the
 * ompi_registry_getkey() function to translate the token to an integer key prior to storing
 * the object. If the token is not currently defined in the segment's dictionary, a new
 * dictionary entry will automatically be created.
 * @param tokens... Additional tokens (provided as pointers to character strings) can be
 * provided to further identify the object being stored.
 * @param NULL The last parameter in the function call MUST be a NULL to terminate the
 * variable list of arguments.
 *
 * @retval OMPI_SUCCESS Indicates that the operation was successful.
 * @retval OMPI_ERROR Indicates that the registry was unable to store the object - most
 * likely due to specifying a non-existent segment or lack of available memory.
 */
int ompi_registry_put(uint8_t *object, int size, char *segment, char *token, ...);

/** Retrieve an object from the registry.
 * The ompi_registry_get() function retrieves a copy of an object previously stored on
 * the registry. The caller must provide the registry segment containing the object, and at
 * least one token that describes it. An unlimited number of additional tokens describing the
 * object may be provided. The function will return a linked list of all objects that match
 * the search criteria.
 *
 * CAUTION: The ompi_registry_get() function call MUST end with a NULL parameter! The C variable
 * argument system does not provide for a mechanism by which we can determine the number of
 * arguments that were passed. Failure to terminate the argument list with a NULL will result
 * in segmentation violations or bus errors, thus causing the program to catastrophically fail.
 *
 * @param segment Pointer to a character string defining the segment of the registry.
 * @param token Pointer to a character string containing the token to be retrieved.
 * @param tokens... Additional tokens (provided as pointers to character strings) can be
 * provided to further identify the object being retrieved.
 * @param NULL The last parameter in the function call MUST be a NULL to terminate the
 * variable list of arguments.
 *
 * @retval object Pointer to a linked list of ompi_registry_value structures, each containing
 * a pointer to a copy of the object retrieved from the registry, the size of the object in bytes,
 * and a pointer to the next link in the list (which is NULL for the last link). The object
 * is returned as a *uint8_t since the object's internal structure is unknown to the registry.
 * @retval NULL Indicates that the specified object could not be found.
 */
ompi_registry_value_t *ompi_registry_get(char *segment, char *token, ...);

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
int ompi_registry_del(char *segment, char *token, ...);

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
 * @param NULL The last parameter in the function call MUST be a NULL to terminate the
 * variable list of arguments.
 *
 * @retval keyvalues A pointer to a linked list of token-key pairs. Any tokens not found
 * will be ignored and will, therefore, not be included in the returned list.
 * @retval NULL Indicates that the operation failed - most likely caused by failing to specify
 * any token that exists within the specified segment, or a non-existent segment.
 */
ompi_keytable_t *ompi_registry_index(char *segment, ...);

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
 * @retval OMPI_SUCCESS Indicating that the operation was successful.
 * @retval OMPI_ERROR Indicates that the operation failed - most likely caused by specifying
 * an object that did not exist within the specified segment, or a non-existent segment.
 */
int ompi_registry_subscribe(int caller, uint8_t action, char *segment, char *token, ...);

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
 * @retval OMPI_SUCCESS Indicating that the operation was successful. Note that this value will
 * also be returned if the caller was not previously subscribed to the specified object since an
 * unsubscribe request would have resulted in the same end condition.
 * @retval OMPI_ERROR Indicates that the operation failed - most likely caused by specifying
 * an object that did not exist within the specified segment, or a non-existent segment.
 */
int ompi_registry_unsubscribe(int caller, uint8_t action, char *segment, char *token, ...);
