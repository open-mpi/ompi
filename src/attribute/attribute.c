/**
 * $HEADER$
 */

#include "attribute/attribute.h"
#include "communicator/communicator.h"
#include "include/constants.h"

/**
 * Macros
 */

#define ATTR_TABLE_SIZE 10

/* This is done so that I can have a consistent interface to my macros
   here */

#define MPI_DATATYPE_NULL_COPY_FN MPI_TYPE_NULL_COPY_FN
#define attr_communicator_f c_f_to_c_index
#define attr_datatype_f d_f_to_c_index
#define attr_win_f w_f_to_c_index

#define CREATE_KEY() ompi_bitmap_find_and_set_first_unset_bit(key_bitmap)

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

OBJ_CLASS_INSTANCE(ompi_attrkey_item_t, 
		   ompi_object_t,
		   ompi_attrkey_item_construct,
		   ompi_attrkey_item_destruct);


/* 
 * Static variables 
 */

static ompi_hash_table_t *keyval_hash;
static ompi_bitmap_t *key_bitmap;


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

    ompi_hash_table_remove_value_uint32(keyval_hash, item->key);
    FREE_KEY(item->key);
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
  
    *key = CREATE_KEY();
    ret = ompi_hash_table_set_value_uint32(keyval_hash, *key, attr);
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
ompi_attr_free_keyval(ompi_attribute_type_t type, int *key, int predefined)
{
    ompi_attrkey_item_t *key_item;

    /* Protect against the user calling ompi_attr_destroy and then
       calling any of the functions which use it  */
    if (NULL == keyval_hash) {
	return MPI_ERR_INTERN;
    }

    /* Find the key-value pair */

    key_item = (ompi_attrkey_item_t*) 
	ompi_hash_table_get_value_uint32(keyval_hash, *key);
  
    if ((NULL == key_item) || (key_item->attr_type != type) ||
	((!predefined) && (key_item->attr_flag & OMPI_KEYVAL_PREDEFINED))) {
	return OMPI_ERR_BAD_PARAM;
    }

    /* Not releasing the object here, it will be done in MPI_*_attr_delete */

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
                 int predefined) 
{
    ompi_attrkey_item_t *key_item;
    int ret, err;
    void *attr;

    /* Protect against the user calling ompi_attr_destroy and then
       calling any of the functions which use it  */
    if (NULL == keyval_hash) {
	return MPI_ERR_INTERN;
    }

    /* Check if the key is valid in the key-attribute hash */

    key_item = (ompi_attrkey_item_t*) 
	ompi_hash_table_get_value_uint32(keyval_hash, key);
  
    if ((NULL == key_item) || (key_item->attr_type!= type) ||
	((!predefined) && (key_item->attr_flag & OMPI_KEYVAL_PREDEFINED))) {
	return OMPI_ERR_BAD_PARAM;
    }

    /* Check if the key is valid for the communicator/window/dtype. If
       yes, then delete the attribute and key entry from the CWD hash */

    attr = ompi_hash_table_get_value_uint32(keyhash, key);

    switch(type) {
    case COMM_ATTR:
	DELETE_ATTR_OBJECT(communicator, attr);
	break;

    case WIN_ATTR:
	DELETE_ATTR_OBJECT(win, attr);
	break;

    case TYPE_ATTR:
	DELETE_ATTR_OBJECT(datatype, attr);
	break;

    default:
        /* show_help */
        return MPI_ERR_INTERN;
    }

    ret = ompi_hash_table_remove_value_uint32(keyhash, key);
    if (OMPI_SUCCESS != ret) {
        return ret; 
    }
    
    /* Decrement the ref count for the key, and if ref count is 0,
       remove the key (the destructor deletes the key implicitly for
       this object */

    OBJ_RELEASE(key_item);
    return MPI_SUCCESS;
}


int
ompi_attr_set(ompi_attribute_type_t type, void *object, 
              ompi_hash_table_t *keyhash, int key, void *attribute,
              int predefined)
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

    key_item = (ompi_attrkey_item_t *) 
	ompi_hash_table_get_value_uint32(keyval_hash, key);

    /* If key not found */

    if ((NULL == key_item) || (key_item->attr_type != type) ||
	((!predefined) && (key_item->attr_flag & OMPI_KEYVAL_PREDEFINED))) {
	return OMPI_ERR_BAD_PARAM;
    }

    /* Now see if the key is present in the CWD object. If so, delete
       the old attribute in the key */
    oldattr = ompi_hash_table_get_value_uint32(keyhash, key);

    if (oldattr != NULL) {
	switch(type) {
	case COMM_ATTR:
	    DELETE_ATTR_OBJECT(communicator, oldattr);
	    break;

	case WIN_ATTR:
	    DELETE_ATTR_OBJECT(win, oldattr);
	    break;

	case TYPE_ATTR:	    
	    DELETE_ATTR_OBJECT(datatype, oldattr);
	    break;

	default:
            /* show_help */
            return MPI_ERR_INTERN;
	}
	had_old = 1;
    }

    ret = ompi_hash_table_set_value_uint32(keyhash, key, attribute); 
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
    void *attr;
    ompi_attrkey_item_t *key_item;

    /* According to MPI specs, the call is invalid if key is not
       present in the main hash at all. If no attribute is associated
       with the key, then the call is valid and returns FALSE in the
       flag argument */

    key_item = (ompi_attrkey_item_t *) 
	ompi_hash_table_get_value_uint32(keyval_hash, key);

    if (NULL == key_item) {
	return MPI_KEYVAL_INVALID;
    }

    attr = ompi_hash_table_get_value_uint32(keyhash, key);

    if (NULL == attr) {
	*flag = 0;
    } else {

	/* VPS: Put Fortran stuff here too */
	
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

    /* Get the first key-attr in the CWD hash */
    ret = ompi_hash_table_get_first_key_uint32(oldkeyhash, &key, &old_attr,
                                               &node);

    /* While we still have some key-attr pair in the CWD hash */
    while (OMPI_SUCCESS == ret) {
        in_node = node;

        /* Get the attr_item in the main hash - so that we know what
           the copy_attr_fn is */

        hash_value = (ompi_attrkey_item_t *)
            ompi_hash_table_get_value_uint32(keyval_hash, key);

        assert (hash_value != NULL);

        switch (type) {
        case COMM_ATTR:
            /* Now call the copy_attr_fn */
            COPY_ATTR_OBJECT(communicator, old_object, hash_value);
            break;
	    
        case TYPE_ATTR:
            /* Now call the copy_attr_fn */
            COPY_ATTR_OBJECT(datatype, old_object, hash_value);
            break;

        case WIN_ATTR:
            /* Now call the copy_attr_fn */
            COPY_ATTR_OBJECT(win, old_object, hash_value);
            break;
        }

        /* Hang this off the new CWD object */
	    
        /* VPS: predefined is set to 1, so that no comparison is done
           for prdefined at all and it just falls off the error
           checking loop in attr_set  */

        /* VPS: we pass the address of new_attr in here, I am assuming
           that new_attr should have actually been a double pointer in
           the copy fn, but since its a pointer in that MPI specs, we
           need to pass *new_attr here  */
        if (flag == 1) {
            ompi_attr_set(type, new_object, newkeyhash, key, 
                          new_attr, 1);
        }

        ret = ompi_hash_table_get_next_key_uint32(oldkeyhash, &key, 
                                                  &old_attr, in_node, 
                                                  &node);
    }
    return MPI_SUCCESS;
}


int
ompi_attr_delete_all(ompi_attribute_type_t type, void *object, 
                     ompi_hash_table_t *keyhash)
{
    int ret;
    uint32_t key, oldkey;
    void *node, *in_node, *old_attr;

    /* Protect against the user calling ompi_attr_destroy and then
       calling any of the functions which use it  */
    if (NULL == keyval_hash) {
	return MPI_ERR_INTERN;
    }
	
    /* Get the first key in local CWD hash  */
    ret = ompi_hash_table_get_first_key_uint32(keyhash,
                                               &key, &old_attr,
                                               &node);
    while (OMPI_SUCCESS == ret) {

	/* Save this node info for deletion, before we move onto the
	   next node */

	in_node = node;
	oldkey = key;
	
	/* Move to the next node */

	ret = ompi_hash_table_get_next_key_uint32(keyhash,
                                                  &key, &old_attr, 
                                                  in_node, &node);
	/* Now delete this attribute */

	ompi_attr_delete(type, object, keyhash, oldkey, 1);
    }
	
    return MPI_SUCCESS;
}
