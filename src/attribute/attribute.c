/**
 * $HEADER$
 */

#include "attribute/attribute.h"
#include "communicator/communicator.h"

/**
 * Macros - Lots of them!
 */


#define ATTR_TABLE_SIZE 10

/* This is done so that I can have a conssitent interface to my macros
   here */

#define MPI_DATATYPE_NULL_COPY_FN MPI_TYPE_NULL_COPY_FN
#define lam_c_t lam_communicator_t *
#define lam_d_t lam_datatype_t *
#define lam_w_t lam_win_t *

#define MPI_c_delete_attr_function MPI_Comm_delete_attr_function
#define MPI_d_delete_attr_function MPI_Type_delete_attr_function
#define MPI_w_delete_attr_function MPI_Win_delete_attr_function

#define MPI_c_copy_attr_function MPI_Comm_copy_attr_function
#define MPI_d_copy_attr_function MPI_Type_copy_attr_function
#define MPI_w_copy_attr_function MPI_Win_copy_attr_function

#define CAST_HANDLE(object, type) (lam_##type##_t) object

#define KEYHASH(type) ((lam_##type##_t)object)->type##_keyhash

#define CREATE_KEY() lam_bitmap_find_and_set_first_unset_bit(key_bitmap)


#define FREE_KEY(key) lam_bitmap_clear_bit(key_bitmap, (key))


/* Not checking for NULL_DELETE_FN here, since according to the
   MPI-standard it should be a valid function that returns
   MPI_SUCCESS */


#define DELETE_ATTR_OBJECT(type, caps_type, attr) \
    if ((err = (*((MPI_##type##_delete_attr_function *) \
	   (key_item->delete_attr_fn)))((lam_##type##_t)object, \
				    key, attr, \
				    key_item->extra_state)) != MPI_SUCCESS) {\
	return err;\
    }

#define COPY_ATTR_OBJECT(type, old_object, hash_value) \
    if ((err = (*(MPI_##type##_copy_attr_function *) (hash_value->copy_attr_fn)) \
          ((lam_##type##_t)old_object, key, hash_value->extra_state, \
            old_attr, new_attr, &flag)) != MPI_SUCCESS) { \
        return err; \
    }


#define GET_ATTR(type) \
    lam_hash_table_get_value_uint32(((lam_##type##_t)object)->type##_keyhash, key);


#define SET_ATTR(type, attribute) \
    ret = lam_hash_table_set_value_uint32(((lam_##type##_t)object)->type##_keyhash,\
                                            key, \
                                            attribute); \
    if (ret != LAM_SUCCESS) \
        return ret; 


#define REMOVE_ATTR_ENTRY(type) \
    ret = lam_hash_table_remove_value_uint32(((lam_##type##_t)object)->type##_keyhash,\
                                               key);\
    if (ret != LAM_SUCCESS) \
        return ret;


/* 
 * Static functions
 */

static void lam_attribute_construct(lam_attrkey_t *attribute);
static void lam_attribute_destruct(lam_attrkey_t *attribute);
static void lam_attrkey_item_construct(lam_attrkey_item_t *item);
static void lam_attrkey_item_destruct(lam_attrkey_item_t *item);


/*
 * lam_attrkey_t classes
 */

OBJ_CLASS_INSTANCE(lam_attrkey_t, 
		   lam_list_t, 
		   lam_attribute_construct,
		   lam_attribute_destruct);
/*
 * lam_attribute_entry_t classes
 */

OBJ_CLASS_INSTANCE(lam_attrkey_item_t, 
		   lam_object_t,
		   lam_attrkey_item_construct,
		   lam_attrkey_item_destruct);


/* 
 * Static variables 
 */

static lam_attrkey_t *attr_hash;
static lam_bitmap_t *key_bitmap;


/*
 * lam_attrkey_t interface functions
 */

static void 
lam_attribute_construct(lam_attrkey_t *attribute) 
{
    attribute->a_fhandle = -1;
}


static void 
lam_attribute_destruct(lam_attrkey_t *attribute) 
{
}

/*
 * lam_attrkey_item_t interface functions
 */

static void
lam_attrkey_item_construct(lam_attrkey_item_t *item) 
{
    memset((void *)item, 0, sizeof(lam_attrkey_item_t));
}


static void 
lam_attrkey_item_destruct(lam_attrkey_item_t *item) 
{
    /* Remove the key entry from the hash and free the key */

    lam_hash_table_remove_value_uint32(&(attr_hash->super), item->key);
    FREE_KEY(item->key);
}


/* 
 * This will initialize the main list to store key- attribute
 * items. This will be called one time, mostly during MPI_INIT()
 */

int 
lam_attr_init()
{
    attr_hash = OBJ_NEW(lam_attrkey_t);
    if (NULL == attr_hash) {
	fprintf(stderr, "Error while creating the main attribute list\n");
	return MPI_ERR_SYSRESOURCE;
    }
    key_bitmap = OBJ_NEW(lam_bitmap_t);
    if (0 != lam_bitmap_init(key_bitmap, 10)) {
	return MPI_ERR_SYSRESOURCE;
    }
    if (lam_hash_table_init(&attr_hash->super,
			    ATTR_TABLE_SIZE) != LAM_SUCCESS)
	return MPI_ERR_SYSRESOURCE;
  
    return MPI_SUCCESS;
}


/* 
 * This will destroy the list, mostly during MPI_Finalize()
 */

void 
lam_attr_destroy()
{
    OBJ_RELEASE(attr_hash);
}
  

int 
lam_attr_create_keyval(lam_attribute_type_t type,
		       void *copy_attr_fn, void *delete_attr_fn,
		       int *key, void *extra_state, int predefined)
{
    lam_attrkey_item_t *attr;
    int ret;

    /* Allocate space for the list item */

    attr = OBJ_NEW(lam_attrkey_item_t);
    if (NULL == attr) {
	fprintf(stderr, "Error during new object creation for attribute \n");
	return MPI_ERR_SYSRESOURCE;
    }

    /* Create a new unique key and fill the hash */
  
    *key = CREATE_KEY();
    ret = lam_hash_table_set_value_uint32(&attr_hash->super, *key, attr);
    if (ret != LAM_SUCCESS)
	return ret;

    /* Fill in the list item */
  
    attr->copy_attr_fn = copy_attr_fn;
    attr->delete_attr_fn = delete_attr_fn;
    attr->extra_state = extra_state;
    attr->attr_type = type;
    attr->key = *key;
    attr->attr_flag = (1 == predefined) ? LAM_PREDEFINED : 0;

    /* Bump up the reference count, since we want the object to be
       destroyed only when free_keyval is called and not when all
       attributes associated with it have been deleted */

    OBJ_RETAIN(attr);

    /* Other fields will be filled in by the create_attr function */
  
    return MPI_SUCCESS;
}


int 
lam_attr_free_keyval(lam_attribute_type_t type, int *key, int predefined)
{
    lam_attrkey_item_t *key_item;

    /* Find the key-value pair */

    key_item = (lam_attrkey_item_t*) 
	lam_hash_table_get_value_uint32(&attr_hash->super, *key);
  
    if ((NULL == key_item) || (key_item->attr_type != type) ||
	((!predefined) && (key_item->attr_flag & LAM_PREDEFINED)))
	return MPI_INVALID_ATTR_KEYVAL;

    /* Not releasing the object here, it will be done in MPI_*_attr_delete */

    *key = MPI_KEYVAL_INVALID;

    /* This will delete the key only when no attributes are associated
       with it, else it will just decrement the reference count, so that when
       the last attribute is deleted, this object gets deleted too */  

    OBJ_RELEASE(key_item);

    return MPI_SUCCESS;
}


int 
lam_attr_delete(lam_attribute_type_t type, void *object, int key,
		int predefined) 
{
    lam_attrkey_item_t *key_item;
    int ret, err;
    void *attr;

    /* Check if the key is valid in the key-attribute hash */

    key_item = (lam_attrkey_item_t*) 
	lam_hash_table_get_value_uint32(&attr_hash->super, key);
  
    if ((NULL == key_item) || (key_item->attr_type!= type) ||
	((!predefined) && (key_item->attr_flag & LAM_PREDEFINED)))
	return MPI_INVALID_ATTR_KEYVAL;

    /* Check if the key is valid for the communicator/window/dtype. If
       yes, then delete the attribute and key entry from the CWD hash */
  
    switch(type) {
  
    case COMM_ATTR:
	attr = GET_ATTR(c);
	DELETE_ATTR_OBJECT(c, COMM, attr);
	REMOVE_ATTR_ENTRY(c);
	break;

    case WIN_ATTR:
	attr = GET_ATTR(w);
	DELETE_ATTR_OBJECT(w, WIN, attr);
	REMOVE_ATTR_ENTRY(w);
	break;

    case TYPE_ATTR:
	attr = GET_ATTR(d);
	DELETE_ATTR_OBJECT(d, TYPE, attr);
	REMOVE_ATTR_ENTRY(d);
	break;

    default:
	fprintf(stderr, "lam_attribute: lam_attr_seet: Invalid type -- "
		" Should be one of COMM/WIN/TYPE \n");
	assert(0);
    }

    /* Decrement the ref count for the key, and if ref count is 0,
       remove the key (the destructor deletes the key implicitly for
       this object */

    OBJ_RELEASE(key_item);
    return MPI_SUCCESS;
}


int
lam_attr_set(lam_attribute_type_t type, void *object, int key, void *attribute,
	     int predefined)

{
    lam_attrkey_item_t *key_item;
    int ret, err;
    void *oldattr;
    int had_old = 0;

    key_item = (lam_attrkey_item_t *) 
	lam_hash_table_get_value_uint32(&(attr_hash->super), key);

    /* If key not found */

    if ((NULL == key_item) || (key_item->attr_type != type) ||
	((!predefined) && (key_item->attr_flag & LAM_PREDEFINED))) {
	fprintf(stderr, "lam_attribute: lam_attr_set: key not found \n");
	return MPI_INVALID_ATTR_KEYVAL;
    }

    /* Now see if the key is present in the CWD object. If so, delete
       the old attribute in the key */
  
    switch(type) {
    
    case COMM_ATTR:
	oldattr = GET_ATTR(c);
	if (oldattr != NULL) {
	    DELETE_ATTR_OBJECT(c, COMM, oldattr);
	    had_old = 1;
	}
	SET_ATTR(c, attribute);
	break;
      
    case WIN_ATTR:
	oldattr = GET_ATTR(w);
	if (oldattr != NULL) {
	    DELETE_ATTR_OBJECT(w, WIN, oldattr);
	    had_old = 1;
	}
	SET_ATTR(w, attribute);
	break;
    
    case TYPE_ATTR:
	oldattr = GET_ATTR(d);
	if (oldattr != NULL) {
	    DELETE_ATTR_OBJECT(d, TYPE, oldattr);
	    had_old = 1;
	}
	SET_ATTR(d, attribute);
	break;
      
    default:
	fprintf(stderr, "lam_attribute: lam_attr_set: Invalid type -- "
		" Should be one of COMM/WIN/TYPE \n");
	assert(0);
    }
    
    /* Increase the reference count of the object, only if there was no
       old atribute/no old entry in the CWD */

    if (!had_old)
	OBJ_RETAIN(key_item);
    return MPI_SUCCESS;
}


int
lam_attr_get(lam_attribute_type_t type, void *object, int key, void *attribute,
	     int *flag)
{
    /* VPS: I dont think you really need to check if the key is present
       in the main hash or not. If it is present in the CWD hash, then
       that means its a valid key. There should be no way that a key is
       existing in CWD and not in the main hash if things are done
       properly in the back-end */

    void *attr;

    switch (type) {

    case COMM_ATTR:
	attr = GET_ATTR(c);
	break;
    
    case WIN_ATTR:
	attr = GET_ATTR(w);
	break;

    case TYPE_ATTR:
	attr = GET_ATTR(d);
	break;

    default:
	fprintf(stderr, "lam_attribute: lam_attr_set: Invalid type -- "
		" Should be one of COMM/WIN/TYPE \n");
	assert(0);
    }
  
    if (NULL == attr)
	*flag = 0;

    /* VPS: Put Fortran stuff here too */

    *((void **) attribute) = attr;
    *flag = 1;
    return MPI_SUCCESS;
}


/* There is too much of code copy/paste in here, see if some other
   logic could work here  */
int
lam_attr_copy_all(lam_attribute_type_t type, void *old_object, 
		  void *new_object)
{
    int ret;
    int err;
    uint32_t key;
    int flag;
    void *node, *in_node, *old_attr, *new_attr;
    lam_attrkey_item_t *hash_value;
    void *object = old_object; /* For consistent interface to
				  KEYHASH. I know whis is a bad way,
				  but was being lazy to change all
				  calls to KEYHASH with an ardditional
				  argument */

    switch (type) {

    case COMM_ATTR:
	/* Get the first key-attr in the CWD hash */
	ret = lam_hash_table_get_first_key_uint32(KEYHASH(c), &key, old_attr,
						  node);

	/* While we still have some key-attr pair in the CWD hash */
	while (ret != LAM_ERROR) {
	    in_node = node;

	    /* Get the attr_item in the main hash - so that we know
	       what the copy_attr_fn is */

	    hash_value = (lam_attrkey_item_t *)
		lam_hash_table_get_value_uint32(&(attr_hash->super), key);

	    assert (hash_value != NULL);
	    
	    /* Now call the copy_attr_fn */
	    COPY_ATTR_OBJECT(c, CAST_HANDLE(old_object, c), 
			     hash_value);

	    /* Hang this off the new CWD object */
	    lam_attr_set(COMM_ATTR, new_object, key, new_attr, 1);

	    ret = lam_hash_table_get_next_key_uint32(KEYHASH(c), &key, 
						     old_attr, in_node, node);
	}
	break;

    case TYPE_ATTR:
	ret = lam_hash_table_get_first_key_uint32(KEYHASH(d), &key, old_attr,
						  node);
	while (ret != LAM_ERROR) {
	    in_node = node;
	    hash_value = (lam_attrkey_item_t *)
		lam_hash_table_get_value_uint32(&(attr_hash->super), key);

	    assert (hash_value != NULL);
	    
	    /* Now call the copy_attr_fn */
	    COPY_ATTR_OBJECT(d, CAST_HANDLE(old_object, d), hash_value);

	    /* Hang this off the new CWD object */
	    lam_attr_set(TYPE_ATTR, new_object, key, new_attr, 1);

	    ret = lam_hash_table_get_next_key_uint32(KEYHASH(d), &key, 
						     old_attr, in_node, node);
	}
	break;

    case WIN_ATTR:
	ret = lam_hash_table_get_first_key_uint32(KEYHASH(w), &key, old_attr,
						  node);
	while (ret != LAM_ERROR) {
	    in_node = node;
	    hash_value = (lam_attrkey_item_t *)
		lam_hash_table_get_value_uint32(&(attr_hash->super), key);

	    assert (hash_value != NULL);
	    
	    /* Now call the copy_attr_fn */
	    COPY_ATTR_OBJECT(w, CAST_HANDLE(old_object, w), hash_value);

	    /* Hang this off the new CWD object */
	    lam_attr_set(WIN_ATTR, new_object, key, new_attr, 1);

	    ret = lam_hash_table_get_next_key_uint32(KEYHASH(w), &key, 
						     old_attr, in_node, node);
	}
	break;
    }
    return MPI_SUCCESS;
}


int
lam_attr_delete_all(lam_attribute_type_t type, void *object)
{
    int ret;
    uint32_t key;
    void *node, *in_node, *old_attr;
    lam_attrkey_item_t *hash_value;


    switch (type) {
    case COMM_ATTR:
	
	/* Get the first key in local CWD hash  */
	ret = lam_hash_table_get_first_key_uint32(KEYHASH(c), &key, old_attr,
						  node);
	while (ret != LAM_ERROR) {
	    in_node = node;
	    hash_value = (lam_attrkey_item_t *)
		lam_hash_table_get_value_uint32(&(attr_hash->super), key);

	    assert (hash_value != NULL);
	    
	    /* Now delete this attribute */

	    lam_attr_delete(type, object, key, 1);

	    ret = lam_hash_table_get_next_key_uint32(KEYHASH(c), &key, 
						     old_attr, in_node, node);
	}

	break;

    case TYPE_ATTR:

	/* Get the first key in local CWD hash  */
	ret = lam_hash_table_get_first_key_uint32(KEYHASH(d), &key, old_attr,
						  node);
	while (ret != LAM_ERROR) {
	    in_node = node;
	    hash_value = (lam_attrkey_item_t *)
		lam_hash_table_get_value_uint32(&(attr_hash->super), key);

	    assert (hash_value != NULL);
	    
	    /* Now delete this attribute */

	    lam_attr_delete(type, object, key, 1);

	    ret = lam_hash_table_get_next_key_uint32(KEYHASH(d), &key, 
						     old_attr, in_node, node);
	}
	break;

    case WIN_ATTR:

	/* Get the first key in local CWD hash  */
	ret = lam_hash_table_get_first_key_uint32(KEYHASH(w), &key, old_attr,
						  node);
	while (ret != LAM_ERROR) {
	    in_node = node;
	    hash_value = (lam_attrkey_item_t *)
		lam_hash_table_get_value_uint32(&(attr_hash->super), key);

	    assert (hash_value != NULL);
	    
	    /* Now delete this attribute */

	    lam_attr_delete(type, object, key, 1);

	    ret = lam_hash_table_get_next_key_uint32(KEYHASH(w), &key, 
						     old_attr, in_node, node);
	}
	break;
    }
    return MPI_SUCCESS;
}
