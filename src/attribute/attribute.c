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
   here, because Datatype and Type - both are used for Datatype
   related stuff */

#define MPI_DATATYPE_NULL_COPY_FN MPI_TYPE_NULL_COPY_FN
#define lam_Comm_t lam_communicator_t *
#define lam_Type_t lam_datatype_t *
#define lam_Win_t lam_win_t *

#define CREATE_KEY() lam_bitmap_find_and_set_first_unset_bit(key_bitmap)


#define FREE_KEY(key) lam_bitmap_clear_bit(key_bitmap, (key))


#define DELETE_ATTR_OBJECT(type, caps_type, attr) \
    if ((MPI_##caps_type##_NULL_DELETE_FN == \
        (MPI_##type##_delete_attr_function *) (key_item->delete_attr_fn)) || \
	((*((MPI_##type##_delete_attr_function *) \
	   (key_item->delete_attr_fn)))((lam_##type##_t)object, \
				    key, attr, \
				    key_item->extra_state) != MPI_SUCCESS)) {\
	return LAM_ERROR;\
    }


#define GET_ATTR(type) \
    lam_hash_table_get_value_uint32(((lam_##type##_t)object)->keyhash, key);


#define SET_ATTR(type, attribute) \
    ret = lam_hash_table_set_value_uint32(((lam_##type##_t)object)->keyhash,\
                                            key, \
                                            attribute); \
    if (ret != LAM_SUCCESS) \
        return ret; 


#define REMOVE_ATTR_ENTRY(type) \
    ret = lam_hash_table_remove_value_uint32(((lam_##type##_t)object)->keyhash,\
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
    int ret;
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
	attr = GET_ATTR(Comm);
	DELETE_ATTR_OBJECT(Comm, COMM, attr);
	REMOVE_ATTR_ENTRY(Comm);
	break;

    case WIN_ATTR:
	attr = GET_ATTR(Win);
	DELETE_ATTR_OBJECT(Win, WIN, attr);
	REMOVE_ATTR_ENTRY(Win);
	break;

    case TYPE_ATTR:
	attr = GET_ATTR(Type);
	DELETE_ATTR_OBJECT(Type, TYPE, attr);
	REMOVE_ATTR_ENTRY(Type);
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
    int ret;
    void *oldattr;
    int had_old = 0;

    key_item = (lam_attrkey_item_t *) 
	lam_hash_table_get_value_uint32(&(attr_hash->super), key);

    /* If key not found */

    if ((NULL == key_item) || (key_item->attr_type != type) ||
	((predefined) && (key_item->attr_flag & LAM_PREDEFINED))) {
	fprintf(stderr, "lam_attribute: lam_attr_set: key not found \n");
	return MPI_INVALID_ATTR_KEYVAL;
    }

    /* Now see if the key is present in the CWD object. If so, delete
       the old attribute in the key */
  
    switch(type) {
    
    case COMM_ATTR:
	oldattr = GET_ATTR(Comm);
	if (oldattr != NULL) {
	    DELETE_ATTR_OBJECT(Comm, COMM, oldattr);
	    had_old = 1;
	}
	SET_ATTR(Comm, attribute);
	break;
      
    case WIN_ATTR:
	oldattr = GET_ATTR(Win);
	if (oldattr != NULL) {
	    DELETE_ATTR_OBJECT(Win, WIN, oldattr);
	    had_old = 1;
	}
	SET_ATTR(Win, attribute);
	break;
    
    case TYPE_ATTR:
	oldattr = GET_ATTR(Type);
	if (oldattr != NULL) {
	    DELETE_ATTR_OBJECT(Type, TYPE, oldattr);
	    had_old = 1;
	}
	SET_ATTR(Type, attribute);
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
	attr = GET_ATTR(Comm);
	break;
    
    case WIN_ATTR:
	attr = GET_ATTR(Win);
	break;

    case TYPE_ATTR:
	attr = GET_ATTR(Type);
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
