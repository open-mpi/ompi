/*
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

#include "ompi_config.h"

#include "attribute/attribute.h"
#include "threads/mutex.h"
#include "include/constants.h"
#include "datatype/datatype.h"
#include "communicator/communicator.h"
#include "win/win.h"

/*
 * Macros
 */

#define ATTR_TABLE_SIZE 10

/* This is done so that I can have a consistent interface to my macros
   here */

#define MPI_DATATYPE_NULL_COPY_FN MPI_TYPE_NULL_COPY_FN
#define attr_communicator_f c_f_to_c_index
#define attr_datatype_f d_f_to_c_index
#define attr_win_f w_f_to_c_index

#define CREATE_KEY(key) ompi_bitmap_find_and_set_first_unset_bit(key_bitmap, (key))

#define FREE_KEY(key) ompi_bitmap_clear_bit(key_bitmap, (key))


/* Not checking for NULL_DELETE_FN here, since according to the
   MPI-standard it should be a valid function that returns
   MPI_SUCCESS */

#define DELETE_ATTR_OBJECT(type, attribute) \
    if((key_item->attr_flag & OMPI_KEYVAL_F77)) { \
        (*((key_item->delete_attr_fn).attr_F_delete_fn)) \
                            (&(((ompi_##type##_t *)object)->attr_##type##_f), \
			    &key, (int *) &attribute, \
			    (int *) &key_item->extra_state, &err); \
    } else { \
        if ((err = (*((key_item->delete_attr_fn).attr_##type##_delete_fn)) \
                            ((ompi_##type##_t *)object, \
			    key, attribute, \
			    key_item->extra_state)) != MPI_SUCCESS) {\
            if (need_lock) { \
                OMPI_THREAD_UNLOCK(&alock); \
            } \
	    return err;\
        } \
    }

#define COPY_ATTR_OBJECT(type, old_object, hash_value) \
    if((hash_value->attr_flag & OMPI_KEYVAL_F77)) { \
        (*((hash_value->copy_attr_fn).attr_F_copy_fn)) \
                   (&(((ompi_##type##_t *)old_object)->attr_##type##_f),\
		    (int *) &key, (int *) &hash_value->extra_state, \
		    (int *) &old_attr, \
		    (int *) &new_attr, &flag, &err); \
    } else { \
        if ((err = (*((hash_value->copy_attr_fn).attr_##type##_copy_fn)) \
              ((ompi_##type##_t *)old_object, key, hash_value->extra_state, \
               old_attr, &new_attr, &flag)) != MPI_SUCCESS) { \
            OMPI_THREAD_UNLOCK(&alock); \
           return err; \
        }\
    }


/* 
 * Static
 */
static void ompi_attrkey_item_construct(ompi_attrkey_item_t *item);
static void ompi_attrkey_item_destruct(ompi_attrkey_item_t *item);


/*
 * ompi_attribute_entry_t classes
 */

struct ompi_attrkey_item_t_class;
static OBJ_CLASS_INSTANCE(ompi_attrkey_item_t, 
                          ompi_object_t,
                          ompi_attrkey_item_construct,
                          ompi_attrkey_item_destruct);


/* 
 * Static variables 
 */

static ompi_hash_table_t *keyval_hash;
static ompi_bitmap_t *key_bitmap;

/*
 * Have one lock protect all access to any attribute stuff (keyval
 * hash, key bitmap, attribute hashes on MPI objects, etc.).
 * Arguably, we would have a finer-grained scheme (e.g., 2 locks) that
 * would allow at least *some* concurrency, but these are attributes
 * -- they're not in the performance-critical portions of the code.
 * So why bother?
 */
static ompi_mutex_t alock;


/*
 * ompi_attrkey_item_t interface functions
 */

static void
ompi_attrkey_item_construct(ompi_attrkey_item_t *item) 
{
    memset(&(item->attr_type), 0, 
	   sizeof(ompi_attrkey_item_t) - sizeof(ompi_object_t));
}


static void 
ompi_attrkey_item_destruct(ompi_attrkey_item_t *item) 
{
    /* Remove the key entry from the hash and free the key */

    OMPI_THREAD_LOCK(&alock);
    ompi_hash_table_remove_value_uint32(keyval_hash, item->key);
    FREE_KEY(item->key);
    OMPI_THREAD_UNLOCK(&alock);
}


/* 
 * This will initialize the main list to store key- attribute
 * items. This will be called one time, mostly during MPI_INIT()
 */

int 
ompi_attr_init(void)
{
    int ret;

    keyval_hash = OBJ_NEW(ompi_hash_table_t);
    if (NULL == keyval_hash) {
        /* show_help */
	return MPI_ERR_SYSRESOURCE;
    }
    key_bitmap = OBJ_NEW(ompi_bitmap_t);
    if (0 != ompi_bitmap_init(key_bitmap, 10)) {
	return MPI_ERR_SYSRESOURCE;
    }
    if (OMPI_SUCCESS != (ret = ompi_hash_table_init(keyval_hash,
                                                    ATTR_TABLE_SIZE))) {
	return ret;
    }
    if (OMPI_SUCCESS != (ret = ompi_attr_create_predefined())) {
        return ret;
    }
  
    return OMPI_SUCCESS;
}


/* 
 * This will destroy the list, mostly during MPI_Finalize()
 */

int
ompi_attr_finalize(void)
{
    OBJ_RELEASE(keyval_hash);
    OBJ_RELEASE(key_bitmap);

    return OMPI_SUCCESS;
}
  

int 
ompi_attr_create_keyval(ompi_attribute_type_t type,
			ompi_attribute_fn_ptr_union_t copy_attr_fn,
			ompi_attribute_fn_ptr_union_t delete_attr_fn,
			int *key, void *extra_state, int flags)
{
    ompi_attrkey_item_t *attr;
    int ret;

    /* Protect against the user calling ompi_attr_destroy and then
       calling any of the functions which use it  */
    if (NULL == keyval_hash) {
	return MPI_ERR_INTERN;
    }

    /* Allocate space for the list item */

    attr = OBJ_NEW(ompi_attrkey_item_t);
    if (NULL == attr) {
	return MPI_ERR_SYSRESOURCE;
    }

    /* Create a new unique key and fill the hash */
  
    OMPI_THREAD_LOCK(&alock);
    ret = CREATE_KEY(key);
    if (OMPI_SUCCESS == ret) {
        ret = ompi_hash_table_set_value_uint32(keyval_hash, *key, attr);
    }
    OMPI_THREAD_UNLOCK(&alock);
    if (OMPI_SUCCESS != ret) {
	return ret;
    }

    /* Fill in the list item */
  
    attr->copy_attr_fn = copy_attr_fn;
    attr->delete_attr_fn = delete_attr_fn;
    attr->extra_state = extra_state;
    attr->attr_type = type;
    attr->key = *key;
    attr->attr_flag = flags;

    /* Bump up the reference count, since we want the object to be
       destroyed only when free_keyval is called and not when all
       attributes associated with it have been deleted */

    OBJ_RETAIN(attr);

    /* Other fields will be filled in by the create_attr function */
  
    return MPI_SUCCESS;
}


int 
ompi_attr_free_keyval(ompi_attribute_type_t type, int *key, bool predefined)
{
    int ret;
    ompi_attrkey_item_t *key_item;

    /* Protect against the user calling ompi_attr_destroy and then
       calling any of the functions which use it  */
    if (NULL == keyval_hash) {
	return MPI_ERR_INTERN;
    }

    /* Find the key-value pair */

    OMPI_THREAD_LOCK(&alock);
    ret = ompi_hash_table_get_value_uint32(keyval_hash, *key, 
					   (void **) &key_item);
    OMPI_THREAD_UNLOCK(&alock);
  
    if ((OMPI_SUCCESS != ret) || (NULL == key_item) || (key_item->attr_type != type) ||
	((!predefined) && (key_item->attr_flag & OMPI_KEYVAL_PREDEFINED))) {
	return OMPI_ERR_BAD_PARAM;
    }

    /* MPI says to set the returned value to MPI_KEYVAL_INVALID */

    *key = MPI_KEYVAL_INVALID;

    /* This will delete the key only when no attributes are associated
       with it, else it will just decrement the reference count, so that when
       the last attribute is deleted, this object gets deleted too */  

    OBJ_RELEASE(key_item);

    return MPI_SUCCESS;
}


int 
ompi_attr_delete(ompi_attribute_type_t type, void *object, 
                 ompi_hash_table_t *keyhash, int key,
                 bool predefined, bool need_lock) 
{
    ompi_attrkey_item_t *key_item;
    int ret = OMPI_SUCCESS, err;
    void *attr;

    /* Protect against the user calling ompi_attr_destroy and then
       calling any of the functions which use it  */
    if (NULL == keyval_hash) {
	return MPI_ERR_INTERN;
    }

    /* Note that this function can be invoked by
       ompi_attr_delete_all() to set attributes on the new object (in
       addition to the top-level MPI_* functions that set attributes).
       In these cases, ompi_attr_delete_all() has already locked the
       keyval_lock, so we should not try to lock it again. */

    if (need_lock) {
        OMPI_THREAD_LOCK(&alock);
    }

    /* Check if the key is valid in the key-attribute hash */

    ret = ompi_hash_table_get_value_uint32(keyval_hash, key, 
					   (void **) &key_item);

    if ( (OMPI_SUCCESS != ret)||(NULL == key_item)||(key_item->attr_type!= type)||
	((!predefined) && (key_item->attr_flag & OMPI_KEYVAL_PREDEFINED))) {
	ret = OMPI_ERR_BAD_PARAM;
	goto exit;
    }

    /* Ensure that we don't have an empty keyhash */

    if (NULL == keyhash) {
        ret = OMPI_ERR_BAD_PARAM;
	goto exit;
    }

    /* Check if the key is valid for the communicator/window/dtype. If
       yes, then delete the attribute and key entry from the CWD hash */

    ret = ompi_hash_table_get_value_uint32(keyhash, key, &attr);

    if ( OMPI_SUCCESS == ret ) {
	switch(type) {
	    case COMM_ATTR:
		DELETE_ATTR_OBJECT(communicator, attr);
		break;
		
#if OMPI_WANT_MPI2_ONE_SIDED
	    case WIN_ATTR:
		DELETE_ATTR_OBJECT(win, attr);
		break;
#endif
		
	    case TYPE_ATTR:
		DELETE_ATTR_OBJECT(datatype, attr);
		break;
		
	    default:
		/* show_help */
		ret = MPI_ERR_INTERN;
		goto exit;
	}
    
	ret = ompi_hash_table_remove_value_uint32(keyhash, key);
	if (OMPI_SUCCESS != ret) {
	    goto exit;
	}
    }


 exit:
    if (need_lock) {
	OMPI_THREAD_UNLOCK(&alock);
    }	

    /* Decrement the ref count for the key, and if ref count is 0,
       remove the key (the destructor deletes the key implicitly for
       this object */
    
    OBJ_RELEASE(key_item);
    return ret;
}


int
ompi_attr_set(ompi_attribute_type_t type, void *object, 
              ompi_hash_table_t **keyhash, int key, void *attribute,
              bool predefined, bool need_lock)
{
    ompi_attrkey_item_t *key_item;
    int ret, err;
    void *oldattr;
    int had_old = 0;

    /* Protect against the user calling ompi_attr_destroy and then
       calling any of the functions which use it  */
    if (NULL == keyval_hash) {
	return MPI_ERR_INTERN;
    }
    if (NULL == keyhash) {
        return MPI_ERR_INTERN;
    }

    /* Note that this function can be invoked by ompi_attr_copy_all()
       to set attributes on the new object (in addition to the
       top-level MPI_* functions that set attributes).  In these
       cases, ompi_attr_copy_all() has already locked the keyval_lock,
       so we should not try to lock it again. */

    if (need_lock) {
        OMPI_THREAD_LOCK(&alock);
    }
    ret = ompi_hash_table_get_value_uint32(keyval_hash, key, 
					   (void **) &key_item);

    /* If key not found */

    if ( (OMPI_SUCCESS != ret )||(NULL == key_item) || (key_item->attr_type != type) ||
	((!predefined) && (key_item->attr_flag & OMPI_KEYVAL_PREDEFINED))) {
        if (need_lock) {
            OMPI_THREAD_UNLOCK(&alock);
        }
	return OMPI_ERR_BAD_PARAM;
    }

    /* Do we need to make a new keyhash? */

    if (NULL == *keyhash) {
        ompi_attr_hash_init(keyhash);
    }

    /* Now see if the key is present in the CWD object. If so, delete
       the old attribute in the key */

    ret = ompi_hash_table_get_value_uint32(*keyhash, key, &oldattr);
    if ( OMPI_SUCCESS == ret )  {
	switch(type) {
	case COMM_ATTR:
	    DELETE_ATTR_OBJECT(communicator, oldattr);
	    break;

#if OMPI_WANT_MPI2_ONE_SIDED
	case WIN_ATTR:
	    DELETE_ATTR_OBJECT(win, oldattr);
	    break;
#endif

	case TYPE_ATTR:	    
	    DELETE_ATTR_OBJECT(datatype, oldattr);
	    break;

	default:
            /* show_help */
            return MPI_ERR_INTERN;
	}
	had_old = 1;
    }

    ret = ompi_hash_table_set_value_uint32(*keyhash, key, attribute); 
    if (need_lock) {
        OMPI_THREAD_UNLOCK(&alock);
    }
    if (OMPI_SUCCESS != ret) {
	return ret; 
    }

    /* Increase the reference count of the object, only if there was no
       old atribute/no old entry in the CWD */

    if (!had_old) {
	OBJ_RETAIN(key_item);
    }
    return MPI_SUCCESS;
}


int
ompi_attr_get(ompi_hash_table_t *keyhash, int key, void *attribute,
              int *flag)
{
    int ret;
    void *attr;
    ompi_attrkey_item_t *key_item;

    /* According to MPI specs, the call is invalid if key is not
       present in the main hash at all. If no attribute is associated
       with the key, then the call is valid and returns FALSE in the
       flag argument */

    *flag = 0;
    OMPI_THREAD_LOCK(&alock);
    ret = ompi_hash_table_get_value_uint32(keyval_hash, key, 
					   (void**) &key_item);

    if ( OMPI_ERR_NOT_FOUND == ret ) {
        OMPI_THREAD_UNLOCK(&alock);
	return MPI_KEYVAL_INVALID;
    }

    /* If we have a null keyhash table, that means that nothing has
       been cached on this object yet.  So just return *flag = 0. */

    if (NULL == keyhash) {
        OMPI_THREAD_UNLOCK(&alock);
        return MPI_SUCCESS;
    }

    ret = ompi_hash_table_get_value_uint32(keyhash, key, &attr);
    OMPI_THREAD_UNLOCK(&alock);
    if ( OMPI_SUCCESS == ret ) {
	*((void **) attribute) = attr;
	*flag = 1;
    }
    return MPI_SUCCESS;
}


/* There is too much of code copy/paste in here, see if some other
   logic could work here  */
int
ompi_attr_copy_all(ompi_attribute_type_t type, void *old_object, 
                   void *new_object, ompi_hash_table_t *oldkeyhash,
                   ompi_hash_table_t *newkeyhash)
{
    int ret;
    int err;
    uint32_t key;
    int flag;
    void *node, *in_node, *old_attr, *new_attr;
    ompi_attrkey_item_t *hash_value;

    /* Protect against the user calling ompi_attr_destroy and then
       calling any of the functions which use it  */
    if (NULL == keyval_hash) {
	return MPI_ERR_INTERN;
    }

    /* If there's nothing to do, just return */

    if (NULL == oldkeyhash) {
        return MPI_SUCCESS;
    }

    /* Lock this whole sequence of events -- don't let any other
       thread modify the structure of the keyval hash or bitmap while
       we're traversing it */

    OMPI_THREAD_LOCK(&alock);

    /* Get the first key-attr in the CWD hash */
    ret = ompi_hash_table_get_first_key_uint32(oldkeyhash, &key, &old_attr,
                                               &node);

    /* While we still have some key-attr pair in the CWD hash */
    while (OMPI_SUCCESS == ret) {
        in_node = node;

        /* Get the attr_item in the main hash - so that we know what
           the copy_attr_fn is */

	err = ompi_hash_table_get_value_uint32(keyval_hash, key, 
					       (void **) &hash_value);

	/* assert (err == OMPI_SUCCESS); */

        switch (type) {
        case COMM_ATTR:
            /* Now call the copy_attr_fn */
            COPY_ATTR_OBJECT(communicator, old_object, hash_value);
            break;
	    
        case TYPE_ATTR:
            /* Now call the copy_attr_fn */
            COPY_ATTR_OBJECT(datatype, old_object, hash_value);
            break;

#if OMPI_WANT_MPI2_ONE_SIDED
        case WIN_ATTR:
            /* Now call the copy_attr_fn */
            COPY_ATTR_OBJECT(win, old_object, hash_value);
            break;
#endif
        }

        /* Hang this off the new CWD object */
	    
        /* The "predefined" parameter to ompi_attr_set() is set to 1,
           so that no comparison is done for prdefined at all and it
           just falls off the error checking loop in attr_set  */

        /* VPS: we pass the address of new_attr in here, I am assuming
           that new_attr should have actually been a double pointer in
           the copy fn, but since its a pointer in that MPI specs, we
           need to pass *new_attr here  */
        if (1 == flag) {
            ompi_attr_set(type, new_object, &newkeyhash, key, 
                          new_attr, true, false);
        }

        ret = ompi_hash_table_get_next_key_uint32(oldkeyhash, &key, 
                                                  &old_attr, in_node, 
                                                  &node);
    }

    /* All done */

    OMPI_THREAD_UNLOCK(&alock);
    return MPI_SUCCESS;
}


int
ompi_attr_delete_all(ompi_attribute_type_t type, void *object, 
                     ompi_hash_table_t *keyhash)
{
    int key_ret, del_ret;
    uint32_t key, oldkey;
    void *node, *in_node, *old_attr;

    /* Protect against the user calling ompi_attr_destroy and then
       calling any of the functions which use it  */
    if (NULL == keyval_hash) {
	return MPI_ERR_INTERN;
    }

    /* Ensure that the table is not empty */

    if (NULL == keyhash) {
        return MPI_SUCCESS;
    }
	
    /* Lock this whole sequence of events -- don't let any other
       thread modify the structure of the keyval hash or bitmap while
       we're traversing it */

    OMPI_THREAD_LOCK(&alock);

    /* Get the first key in local CWD hash  */
    key_ret = ompi_hash_table_get_first_key_uint32(keyhash,
                                               &key, &old_attr,
                                               &node);
    del_ret = OMPI_SUCCESS;
    while (OMPI_SUCCESS == key_ret && OMPI_SUCCESS == del_ret) {

	/* Save this node info for deletion, before we move onto the
	   next node */

	in_node = node;
	oldkey = key;
	
	/* Move to the next node */

	key_ret = ompi_hash_table_get_next_key_uint32(keyhash,
                                                      &key, &old_attr, 
                                                      in_node, &node);
	/* Now delete this attribute */

	del_ret = ompi_attr_delete(type, object, keyhash, oldkey, true, false);
    }

    /* All done */

    OMPI_THREAD_UNLOCK(&alock);
    return del_ret;
}
