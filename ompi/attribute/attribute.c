/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 *
 * Back-end MPI attribute engine.
 *
 * This is complicated enough that it deserves a lengthy discussion of
 * what is happening.  This is extremely complicated stuff, paired
 * with the fact that it is not described well in the MPI standard.
 * There are several places in the standard that should be read about
 * attributes:
 *
 * MPI-1: Section 5.7 (pp 167-173)
 * MPI-1: Section 7.1 (pp 191-192) predefined attributes in MPI-1
 * MPI-2: Section 4.12.7 (pp 57-59) interlanguage attribute
 *        clarifications
 * MPI-2: Section 6.2.2 (pp 112) window predefined attributes
 * MPI-2: Section 8.8 (pp 198-208) new attribute caching functions
 *
 * After reading all of this, note the following:
 *
 * - C MPI-1 and MPI-2 attribute functions and functionality are
 *   identical except for their function names.
 * - Fortran MPI-1 and MPI-2 attribute functions and functionality are
 *   different (namely: the parameters are different sizes, both in the
 *   functions and the user callbacks, and the assignments to the
 *   different sized types occur differently [e.g., truncation and sign
 *   extension])
 * - C functions store values by reference (i.e., writing an attribute
 *   means writing a pointer to an instance of something; changing the
 *   value of that instance will make it visible to anyone who reads
 *   that attribute value).
 * - Fortran functions store values by value (i.e., writing an
 *   attribute value means that anyone who reads that attribute value
 *   will not be able to affect the value read by anyone else).
 * - The predefined attribute MPI_WIN_BASE seems to flaunt the rules
 *   designated by the rest of the standard; it is handled
 *   specifically in the MPI_WIN_GET_ATTR binding functions (see the 
 *   comments in there for an explanation).
 * - MPI-2 4.12.7:Example 4.13 (p58) is wrong.  The C->Fortran example
 *   should have the Fortran "val" variable equal to &I.
 *
 * By the first two of these, there are 9 possible use cases -- 3
 * possibilities for writing an attribute value, each of which has 3
 * possibilities for reading that value back.  The following lists
 * each of the 9 cases, and what happens in each.
 *
 * Cases where C writes an attribute value:
 * ----------------------------------------
 *
 * In all of these cases, a pointer was written by C (e.g., a pointer
 * to an int -- but it could have been a pointer to anything, such as
 * a struct).  These scenarios each have 2 examples:
 *
 * Example A: int foo = 3; 
 *            MPI_Attr_put(..., &foo);
 * Example B: struct foo bar; 
 *            MPI_Attr_put(..., &bar);
 * 
 * 1. C reads the attribute value.  Clearly, this is a "unity" case,
 * and no translation occurs.  A pointer is written, and that same
 * pointer is returned.
 *
 * Example A: int *ret; 
 *            MPI_Attr_get(..., &ret); 
 *            --> *ret will equal 3
 * Example B: struct foo *ret; 
 *            MPI_Attr_get(..., &ret);
 *            --> *ret will point to the instance bar that was written
 *
 * 2. Fortran MPI-1 reads the attribute value.  The C pointer is cast
 * to a fortran INTEGER (i.e., MPI_Fint) -- potentially being
 * truncated if sizeof(void*) > sizeof(INTEGER).
 *
 * Example A: INTEGER ret
 *            CALL MPI_ATTR_GET(..., ret, ierr)
 *            --> ret will equal &foo, possibly truncaed
 * Example B: INTEGER ret
 *            CALL MPI_ATTR_GET(..., ret, ierr)
 *            --> ret will equal &bar, possibly truncaed
 *
 * 3. Fortran MPI-2 reads the attribute value.  The C pointer is cast
 * to a fortran INTEGER(KIND=MPI_ADDRESS_KIND) (i.e., a (MPI_Aint)).
 *
 * Example A: INTEGER(KIND=MPI_ADDRESS_KIND) ret
 *            CALL MPI_COMM_GET_ATTR(..., ret, ierr)
 *            --> ret will equal &foo
 * Example B: INTEGER(KIND=MPI_ADDRESS_KIND) ret
 *            CALL MPI_COMM_GET_ATTR(..., ret, ierr) 
 *            --> ret will equal &bar
 *
 * Cases where Fortran MPI-1 writes an attribute value:
 * ----------------------------------------------------
 *
 * In all of these cases, an INTEGER is written by Fortran.
 *
 * Example: INTEGER FOO = 7
 *          CALL MPI_ATTR_PUT(..., foo, ierr)
 *
 * 4. C reads the attribute value.  The value returned is a pointer
 *    that points to an INTEGER (i.e., an MPI_Fint) that has a value
 *    of 7.
 *    --> NOTE: The external MPI interface does not distinguish between
 *        this case and case 7.  It is the programer's responsibility
 *        to code accordingly.
 *
 * Example: MPI_Fint *ret; 
 *          MPI_Attr_get(..., &ret);
 *          -> *ret will equal 7.
 *
 * 5. Fortran MPI-1 reads the attribute value.  This is the unity
 *    case; the same value is returned.
 *
 * Example: INTEGER ret
 *          CALL MPI_ATTR_GET(..., ret, ierr)
 *          --> ret will equal 7
 *
 * 6. Fortran MPI-2 reads the attribute value.  The same value is
 *    returned, but potentially sign-extended if sizeof(INTEGER) <
 *    sizeof(INTEGER(KIND=MPI_ADDRESS_KIND)).
 *
 * Example: INTEGER(KIND=MPI_ADDRESS_KIND) ret
 *          CALL MPI_COMM_GET_ATTR(..., ret, ierr)
 *          --> ret will equal 7
 *
 * Cases where Fortran MPI-2 writes an attribute value:
 * ----------------------------------------------------
 *
 * In all of these cases, an INTEGER(KIND=MPI_ADDRESS_KIND) is written
 * by Fortran.
 *
 * Example A: INTEGER(KIND=MPI_ADDRESS_KIND) FOO = 12
 *            CALL MPI_COMM_PUT_ATTR(..., foo, ierr)
 * Example B: // Assume a platform where sizeof(void*) = 8 and
 *            // sizeof(INTEGER) = 4.
 *            INTEGER(KIND=MPI_ADDRESS_KIND) FOO = pow(2, 40)
 *            CALL MPI_COMM_PUT_ATTR(..., foo, ierr)
 *
 * 7. C reads the attribute value.  The value returned is a pointer
 *    that points to an INTEGER(KIND=MPI_ADDRESS_KIND) (i.e., a void*)
 *    that has a value of 12.
 *    --> NOTE: The external MPI interface does not distinguish between
 *        this case and case 4.  It is the programer's responsibility
 *        to code accordingly.
 *
 * Example A: MPI_Aint *ret; 
 *            MPI_Attr_get(..., &ret);
 *            -> *ret will equal 12
 * Example B: MPI_Aint *ret; 
 *            MPI_Attr_get(..., &ret);
 *            -> *ret will equal 2^40
 *
 * 8. Fortran MPI-1 reads the attribute value.  The same value is
 *    returned, but potentially truncated if sizeof(INTEGER) <
 *    sizeof(INTEGER(KIND=MPI_ADDRESS_KIND)).
 *
 * Example A: INTEGER ret
 *            CALL MPI_ATTR_GET(..., ret, ierr)
 *            --> ret will equal 12
 * Example B: INTEGER ret
 *            CALL MPI_ATTR_GET(..., ret, ierr)
 *            --> ret will equal 0
 *
 * 9. Fortran MPI-2 reads the attribute value.  This is the unity
 *    case; the same value is returned.
 *
 * Example A: INTEGER(KIND=MPI_ADDRESS_KIND) ret
 *            CALL MPI_COMM_GET_ATTR(..., ret, ierr)
 *            --> ret will equal 7
 * Example B: INTEGER(KIND=MPI_ADDRESS_KIND) ret
 *            CALL MPI_COMM_GET_ATTR(..., ret, ierr)
 *            --> ret will equal 2^40
 */

#include "ompi_config.h"

#include "attribute/attribute.h"
#include "threads/mutex.h"
#include "include/constants.h"
#include "datatype/datatype.h"
#include "communicator/communicator.h"
#include "win/win.h"
#include "mpi/f77/fint_2_int.h"

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
   MPI_SUCCESS. 

   This macro exists because we have to replicate the same code for
   MPI_Comm, MPI_Datatype, and MPI_Win.  Ick.

   There are 3 possible sets of callbacks:

   1. MPI-1 Fortran-style: attribute and extra state arguments are of
      type (INTEGER).  This is used if both the OMPI_KEYVAL_F77 and
      OMPI_KEYVAL_F77_MPI1 flags are set.
   2. MPI-2 Fortran-style: attribute and extra state arguments are of
      type (INTEGER(KIND=MPI_ADDRESS_KIND)).  This is used if the
      OMPI_KEYVAL_F77 flag is set and the OMPI_KEYVAL_F77_MPI1 flag is
      *not* set.
   3. C-style: attribute arguments are of type (void*).  This is used
      if OMPI_KEYVAL_F77 is not set.
   
   Ick.
 */

#define DELETE_ATTR_CALLBACKS(type, attribute, keyval_obj) \
    if (0 != (keyval_obj->attr_flag & OMPI_KEYVAL_F77)) { \
        MPI_Fint f_key = OMPI_INT_2_FINT(key); \
        MPI_Fint f_err; \
        /* MPI-1 Fortran-style */ \
        if (0 != (keyval_obj->attr_flag & OMPI_KEYVAL_F77_MPI1)) { \
            MPI_Fint attr_val = translate_to_fortran_mpi1(attribute); \
            (*((keyval_obj->delete_attr_fn).attr_mpi1_fortran_delete_fn)) \
                (&(((ompi_##type##_t *)object)->attr_##type##_f), \
                 &f_key, &attr_val, keyval_obj->extra_state, &f_err); \
            if (MPI_SUCCESS != OMPI_FINT_2_INT(f_err)) { \
                if (need_lock) { \
                    OMPI_THREAD_UNLOCK(&alock); \
                } \
                return OMPI_FINT_2_INT(f_err); \
            } \
        } \
        /* MPI-2 Fortran-style */ \
        else { \
            MPI_Aint attr_val = translate_to_fortran_mpi2(attribute); \
            (*((keyval_obj->delete_attr_fn).attr_mpi2_fortran_delete_fn)) \
                (&(((ompi_##type##_t *)object)->attr_##type##_f), \
                 &f_key, &attr_val, keyval_obj->extra_state, &f_err); \
            if (MPI_SUCCESS != OMPI_FINT_2_INT(f_err)) { \
                if (need_lock) { \
                    OMPI_THREAD_UNLOCK(&alock); \
                } \
                return OMPI_FINT_2_INT(f_err); \
            } \
        } \
    } \
    /* C style */ \
    else { \
        void *attr_val = translate_to_c(attribute); \
        if ((err = (*((keyval_obj->delete_attr_fn).attr_##type##_delete_fn)) \
                            ((ompi_##type##_t *)object, \
			    key, attr_val, \
			    keyval_obj->extra_state)) != MPI_SUCCESS) {\
            if (need_lock) { \
                OMPI_THREAD_UNLOCK(&alock); \
            } \
	    return err;\
        } \
    }

/* See the big, long comment above from DELETE_ATTR_CALLBACKS -- most of
   that text applies here, too. */

#define COPY_ATTR_CALLBACKS(type, old_object, keyval_obj, in_attr, out_attr) \
    if (0 != (keyval_obj->attr_flag & OMPI_KEYVAL_F77)) { \
        MPI_Fint f_key = OMPI_INT_2_FINT(key); \
        MPI_Fint f_err; \
        ompi_fortran_logical_t f_flag; \
        /* MPI-1 Fortran-style */ \
        if (0 != (keyval_obj->attr_flag & OMPI_KEYVAL_F77_MPI1)) { \
            MPI_Fint in, out; \
            in = translate_to_fortran_mpi1(in_attr); \
            (*((keyval_obj->copy_attr_fn).attr_mpi1_fortran_copy_fn)) \
                (&(((ompi_##type##_t *)old_object)->attr_##type##_f), \
                 &f_key, keyval_obj->extra_state, \
                 &in, &out, &f_flag, &f_err); \
            if (MPI_SUCCESS != OMPI_FINT_2_INT(f_err)) { \
                OMPI_THREAD_UNLOCK(&alock); \
                return OMPI_FINT_2_INT(f_err); \
            } \
            out_attr->av_value = (void*) 0; \
            *out_attr->av_integer_pointer = out; \
            flag = OMPI_FINT_2_INT(f_flag); \
        } \
        /* MPI-2 Fortran-style */ \
        else { \
            MPI_Aint in, out; \
            in = translate_to_fortran_mpi2(in_attr); \
            (*((keyval_obj->copy_attr_fn).attr_mpi2_fortran_copy_fn)) \
                (&(((ompi_##type##_t *)old_object)->attr_##type##_f), \
                 &f_key, keyval_obj->extra_state, &in, &out, \
                 &f_flag, &f_err); \
            if (MPI_SUCCESS != OMPI_FINT_2_INT(f_err)) { \
                OMPI_THREAD_UNLOCK(&alock); \
                return OMPI_FINT_2_INT(f_err); \
            } \
            out_attr->av_value = (void *) out; \
            flag = OMPI_FINT_2_INT(f_flag); \
        } \
    } \
    /* C style */ \
    else { \
        void *in, *out; \
        in = translate_to_c(in_attr); \
        if ((err = (*((keyval_obj->copy_attr_fn).attr_##type##_copy_fn)) \
              ((ompi_##type##_t *)old_object, key, keyval_obj->extra_state, \
               in, &out, &flag)) != MPI_SUCCESS) { \
            OMPI_THREAD_UNLOCK(&alock); \
            return err; \
        } \
        out_attr->av_value = out; \
    }


/* 
 * Cases for attribute values
 */
typedef enum ompi_attribute_translate_t {
    OMPI_ATTRIBUTE_C,
    OMPI_ATTRIBUTE_FORTRAN_MPI1,
    OMPI_ATTRIBUTE_FORTRAN_MPI2
} ompi_attribute_translate_t;


/*
 * struct to hold attribute values on each MPI object
 */
typedef struct attribute_value_t {
    opal_object_t super;
    void *av_value;
    MPI_Aint *av_address_kind_pointer;
    MPI_Fint *av_integer_pointer;
    int av_set_from;
} attribute_value_t;


/* 
 * Local functions
 */
static void attribute_value_construct(attribute_value_t *item);
static void ompi_attrkey_item_construct(ompi_attrkey_item_t *item);
static void ompi_attrkey_item_destruct(ompi_attrkey_item_t *item);
static int set_value(ompi_attribute_type_t type, void *object, 
                     ompi_hash_table_t **keyhash, int key, 
                     attribute_value_t *new_attr,
                     bool predefined, bool need_lock);
static int get_value(ompi_hash_table_t *keyhash, int key, 
                     attribute_value_t **attribute, int *flag);
static void *translate_to_c(attribute_value_t *val);
static MPI_Fint translate_to_fortran_mpi1(attribute_value_t *val);
static MPI_Aint translate_to_fortran_mpi2(attribute_value_t *val);


/*
 * attribute_value_t class
 */
static OBJ_CLASS_INSTANCE(attribute_value_t,
                          opal_object_t,
                          attribute_value_construct,
                          NULL);


/*
 * ompi_attribute_entry_t classes
 */
static OBJ_CLASS_INSTANCE(ompi_attrkey_item_t, 
                          opal_object_t,
                          ompi_attrkey_item_construct,
                          ompi_attrkey_item_destruct);


/* 
 * Static variables 
 */

static ompi_hash_table_t *keyval_hash;
static ompi_bitmap_t *key_bitmap;
static unsigned int int_pos = 12345;

#if OMPI_HAVE_THREAD_SUPPORT
/*
 * Have one lock protect all access to any attribute stuff (keyval
 * hash, key bitmap, attribute hashes on MPI objects, etc.).
 * Arguably, we would have a finer-grained scheme (e.g., 2 locks) that
 * would allow at least *some* concurrency, but these are attributes
 * -- they're not in the performance-critical portions of the code.
 * So why bother?
 */
static ompi_mutex_t alock;
#endif  /* OMPI_HAVE_THREAD_SUPPORT */


/*
 * attribute_value_t constructor function
 */
static void attribute_value_construct(attribute_value_t *item)
{
    item->av_address_kind_pointer = (MPI_Aint*) &item->av_value;
    item->av_integer_pointer = &(((MPI_Fint*) &item->av_value)[int_pos]);
    item->av_set_from = 0;
}


/*
 * ompi_attrkey_item_t constructor / destructor
 */
static void
ompi_attrkey_item_construct(ompi_attrkey_item_t *item) 
{
    memset(&(item->attr_type), 0, 
	   sizeof(ompi_attrkey_item_t) - sizeof(opal_object_t));
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

int ompi_attr_init(void)
{
    int ret;
    void *bogus = (void*) 1;
    MPI_Fint *p = (MPI_Fint*) &bogus;

    keyval_hash = OBJ_NEW(ompi_hash_table_t);
    if (NULL == keyval_hash) {
	return MPI_ERR_SYSRESOURCE;
    }
    key_bitmap = OBJ_NEW(ompi_bitmap_t);
    if (0 != ompi_bitmap_init(key_bitmap, 10)) {
	return MPI_ERR_SYSRESOURCE;
    }

    for (int_pos = 0; int_pos < (sizeof(void*) / sizeof(MPI_Fint)); 
         ++int_pos) {
        if (p[int_pos] == 1) {
            break;
        }
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

int ompi_attr_finalize(void)
{
    OBJ_RELEASE(keyval_hash);
    OBJ_RELEASE(key_bitmap);

    return OMPI_SUCCESS;
}
  

int ompi_attr_create_keyval(ompi_attribute_type_t type,
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

    /* Other fields will be filled in by the create_attr function */
  
    return MPI_SUCCESS;
}


int ompi_attr_free_keyval(ompi_attribute_type_t type, int *key, 
                          bool predefined)
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
  
    if ((OMPI_SUCCESS != ret) || (NULL == key_item) || 
        (key_item->attr_type != type) ||
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


int ompi_attr_delete(ompi_attribute_type_t type, void *object, 
                     ompi_hash_table_t *keyhash, int key,
                     bool predefined, bool need_lock) 
{
    ompi_attrkey_item_t *key_item;
    int ret = OMPI_SUCCESS, err;
    attribute_value_t *attr;

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

    /* Check if the key is valid in the master keyval hash */

    ret = ompi_hash_table_get_value_uint32(keyval_hash, key, 
					   (void **) &key_item);

    if ((OMPI_SUCCESS != ret) || (NULL == key_item) ||
        (key_item->attr_type!= type) ||
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
       yes, then delete the attribute and key entry from the object's key
       hash */

    ret = ompi_hash_table_get_value_uint32(keyhash, key, (void**) &attr);
    if (OMPI_SUCCESS == ret) {
	switch (type) {
        case COMM_ATTR:
            DELETE_ATTR_CALLBACKS(communicator, attr, key_item);
            break;
		
#if OMPI_WANT_MPI2_ONE_SIDED
        case WIN_ATTR:
            DELETE_ATTR_CALLBACKS(win, attre, key_item);
            break;
#endif
		
        case TYPE_ATTR:
            DELETE_ATTR_CALLBACKS(datatype, attr, key_item);
            break;
		
        default:
            ret = MPI_ERR_INTERN;
            goto exit;
	}
        OBJ_RELEASE(attr);
    
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


/*
 * Front-end function called by the C MPI API functions to set an
 * attribute.
 */
int ompi_attr_set_c(ompi_attribute_type_t type, void *object, 
                    ompi_hash_table_t **keyhash,
                    int key, void *attribute, bool predefined, bool need_lock)
{
    attribute_value_t *new_attr = OBJ_NEW(attribute_value_t);
    if (NULL == new_attr) {
        return MPI_ERR_SYSRESOURCE;
    }

    new_attr->av_value = attribute;
    new_attr->av_set_from = OMPI_ATTRIBUTE_C;
    return set_value(type, object, keyhash, key, new_attr,
                     predefined, need_lock);
}


/*
 * Front-end function called by the Fortran MPI-2 API functions to set
 * an attribute.
 */
int ompi_attr_set_fortran_mpi1(ompi_attribute_type_t type, void *object, 
                               ompi_hash_table_t **keyhash,
                               int key, MPI_Fint attribute, 
                               bool predefined, bool need_lock)
{
    attribute_value_t *new_attr = OBJ_NEW(attribute_value_t);
    if (NULL == new_attr) {
        return MPI_ERR_SYSRESOURCE;
    }

    new_attr->av_value = (void *) 0;
    *new_attr->av_integer_pointer = attribute;
    new_attr->av_set_from = OMPI_ATTRIBUTE_FORTRAN_MPI1;
    return set_value(type, object, keyhash, key, new_attr,
                     predefined, need_lock);
}


/*
 * Front-end function called by the Fortran MPI-2 API functions to set
 * an attribute.
 */
int ompi_attr_set_fortran_mpi2(ompi_attribute_type_t type, void *object, 
                               ompi_hash_table_t **keyhash,
                               int key, MPI_Aint attribute, 
                               bool predefined, bool need_lock)
{
    attribute_value_t *new_attr = OBJ_NEW(attribute_value_t);
    if (NULL == new_attr) {
        return MPI_ERR_SYSRESOURCE;
    }

    new_attr->av_value = (void *) attribute;
    new_attr->av_set_from = OMPI_ATTRIBUTE_FORTRAN_MPI2;
    return set_value(type, object, keyhash, key, new_attr,
                     predefined, need_lock);
}


/*
 * Front-end function called by the C MPI API functions to get
 * attributes.
 */
int ompi_attr_get_c(ompi_hash_table_t *keyhash, int key, 
                    void **attribute, int *flag)
{
    attribute_value_t *val;
    int ret;

    ret = get_value(keyhash, key, &val, flag);
    if (MPI_SUCCESS == ret && 1 == *flag) {
        *attribute = translate_to_c(val);
    }

    return ret;
}


/*
 * Front-end function called by the Fortran MPI-1 API functions to get
 * attributes.
 */
int ompi_attr_get_fortran_mpi1(ompi_hash_table_t *keyhash, int key, 
                               MPI_Fint *attribute, int *flag)
{
    attribute_value_t *val;
    int ret;

    ret = get_value(keyhash, key, &val, flag);
    if (MPI_SUCCESS == ret && 1 == *flag) {
        *attribute = translate_to_fortran_mpi1(val);
    }

    return ret;
}


/*
 * Front-end function called by the Fortran MPI-2 API functions to get
 * attributes.
 */
int ompi_attr_get_fortran_mpi2(ompi_hash_table_t *keyhash, int key, 
                               MPI_Aint *attribute, int *flag)
{
    attribute_value_t *val;
    int ret;

    ret = get_value(keyhash, key, &val, flag);
    if (MPI_SUCCESS == ret && 1 == *flag) {
        *attribute = translate_to_fortran_mpi2(val);
    }

    return ret;
}


/*
 * Copy all the attributes from one MPI object to another
 */
int ompi_attr_copy_all(ompi_attribute_type_t type, void *old_object, 
                       void *new_object, ompi_hash_table_t *oldkeyhash,
                       ompi_hash_table_t *newkeyhash)
{
    int ret;
    int err;
    uint32_t key;
    int flag;
    void *node, *in_node;
    attribute_value_t *old_attr, *new_attr;
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

    /* Get the first key-attr in the object's key hash */
    ret = ompi_hash_table_get_first_key_uint32(oldkeyhash, &key, 
                                               (void **) &old_attr,
                                               &node);

    /* While we still have some key-attr pair in the object's key
       hash */
    while (OMPI_SUCCESS == ret) {
        in_node = node;

        /* Get the attr_item in the main keyval hash - so that we know
           what the copy_attr_fn is */

	err = ompi_hash_table_get_value_uint32(keyval_hash, key, 
					       (void **) &hash_value);

        new_attr = OBJ_NEW(attribute_value_t);
        switch (type) {
        case COMM_ATTR:
            /* Now call the copy_attr_fn */
            COPY_ATTR_CALLBACKS(communicator, old_object, hash_value, 
                                old_attr, new_attr);
            break;
	    
        case TYPE_ATTR:
            /* Now call the copy_attr_fn */
            COPY_ATTR_CALLBACKS(datatype, old_object, hash_value, 
                                old_attr, new_attr);
            break;

#if OMPI_WANT_MPI2_ONE_SIDED
        case WIN_ATTR:
            /* Now call the copy_attr_fn */
            COPY_ATTR_CALLBACKS(win, old_object, hash_value, 
                                old_attr, new_attr);
            break;
#endif
        }

        /* Hang this off the object's key hash */
	    
        /* The "predefined" parameter to ompi_attr_set() is set to 1,
           so that no comparison is done for prdefined at all and it
           just falls off the error checking loop in attr_set  */

        if (1 == flag) {
            if (0 != (hash_value->attr_flag & OMPI_KEYVAL_F77)) {
                if (0 != (hash_value->attr_flag & OMPI_KEYVAL_F77_MPI1)) {
                    new_attr->av_set_from = OMPI_ATTRIBUTE_FORTRAN_MPI1;
                } else {
                    new_attr->av_set_from = OMPI_ATTRIBUTE_FORTRAN_MPI2;
                }
            } else {
                new_attr->av_set_from = OMPI_ATTRIBUTE_C;
            }
            set_value(type, new_object, &newkeyhash, key, 
                      new_attr, true, false);
        } else {
            OBJ_RELEASE(new_attr);
        }

        ret = ompi_hash_table_get_next_key_uint32(oldkeyhash, &key, 
                                                  (void **) &old_attr, 
                                                  in_node, &node);
    }

    /* All done */

    OMPI_THREAD_UNLOCK(&alock);
    return MPI_SUCCESS;
}


/*
 * Delete all the attributes on an MPI object
 */
int ompi_attr_delete_all(ompi_attribute_type_t type, void *object, 
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

    /* Get the first key in local object's key hash  */
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

/*************************************************************************/

/*
 * Back-end function to set an attribute on an MPI object
 */
static int set_value(ompi_attribute_type_t type, void *object, 
                     ompi_hash_table_t **keyhash, int key, 
                     attribute_value_t *new_attr,
                     bool predefined, bool need_lock)
{
    ompi_attrkey_item_t *key_item;
    int ret, err;
    attribute_value_t *old_attr;
    bool had_old = false;

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

    if ((OMPI_SUCCESS != ret ) || (NULL == key_item) || 
        (key_item->attr_type != type) ||
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

    /* Now see if the key is present in the object's key hash. If so,
       delete the old attribute value. */

    ret = ompi_hash_table_get_value_uint32(*keyhash, key, (void**) &old_attr);
    if (OMPI_SUCCESS == ret)  {
	switch (type) {
	case COMM_ATTR:
	    DELETE_ATTR_CALLBACKS(communicator, old_attr, key_item);
	    break;

#if OMPI_WANT_MPI2_ONE_SIDED
	case WIN_ATTR:
	    DELETE_ATTR_CALLBACKS(win, old_attr, key_item);
	    break;
#endif

	case TYPE_ATTR:	    
	    DELETE_ATTR_CALLBACKS(datatype, old_attr, key_item);
	    break;

	default:
            return MPI_ERR_INTERN;
	}
	had_old = true;
        OBJ_RELEASE(old_attr);
    }

    ret = ompi_hash_table_set_value_uint32(*keyhash, key, new_attr); 

    if (need_lock) {
        OMPI_THREAD_UNLOCK(&alock);
    }
    if (OMPI_SUCCESS != ret) {
	return ret; 
    }

    /* Increase the reference count of the object, only if there was no
       old atribute/no old entry in the object's key hash */

    if (!had_old) {
	OBJ_RETAIN(key_item);
    }
    return MPI_SUCCESS;
}


/*
 * Back-end function to get an attribute from the hash map and return
 * it to the caller.  Translation services are not provided -- they're
 * in small, standalone functions that are called from several
 * different places.
 */
static int get_value(ompi_hash_table_t *keyhash, int key, 
                     attribute_value_t **attribute, int *flag)
{
    int ret;
    void *attr;
    ompi_attrkey_item_t *key_item;

    /* According to MPI specs, the call is invalid if key is not
       present in the main keyval hash at all.  If no attribute is
       associated with the key, then the call is valid and returns
       FALSE in the flag argument */

    *flag = 0;
    OMPI_THREAD_LOCK(&alock);
    ret = ompi_hash_table_get_value_uint32(keyval_hash, key, 
					   (void**) &key_item);

    if (OMPI_ERR_NOT_FOUND == ret) {
        OMPI_THREAD_UNLOCK(&alock);
	return MPI_KEYVAL_INVALID;
    }

    /* If we have a null keyhash table, that means that nothing has
       been cached on this object yet.  So just return *flag = 0. */

    if (NULL == keyhash) {
        OMPI_THREAD_UNLOCK(&alock);
        return OMPI_SUCCESS;
    }

    ret = ompi_hash_table_get_value_uint32(keyhash, key, &attr);
    OMPI_THREAD_UNLOCK(&alock);
    if (OMPI_SUCCESS == ret) {
        *attribute = attr;
	*flag = 1;
    }
    return OMPI_SUCCESS;
}


/*
 * Take an attribute and translate it according to the cases listed in
 * the comments at the top of this file.
 *
 * This function does not fail -- it is only invoked in "safe"
 * situations.
 */
static void *translate_to_c(attribute_value_t *val)
{
    switch (val->av_set_from) {
    case OMPI_ATTRIBUTE_C:
        /* Case 1: written in C, read in C (unity) */
        return val->av_value;
        break;

    case OMPI_ATTRIBUTE_FORTRAN_MPI1:
        /* Case 4: written in Fortran MPI-1, read in C */
        return (void *) val->av_integer_pointer;
        break;

    case OMPI_ATTRIBUTE_FORTRAN_MPI2:
        /* Case 7: written in Fortran MPI-2, read in C */
        return (void *) val->av_address_kind_pointer;
        break;

    default:
        /* Should never reach here */
        return NULL;
    }
}


/*
 * Take an attribute and translate it according to the cases listed in
 * the comments at the top of this file.
 *
 * This function does not fail -- it is only invoked in "safe"
 * situations.
 */
static MPI_Fint translate_to_fortran_mpi1(attribute_value_t *val)
{
    switch (val->av_set_from) {
    case OMPI_ATTRIBUTE_C:
        /* Case 2: written in C, read in Fortran MPI-1 */
        return *val->av_integer_pointer;
        break;

    case OMPI_ATTRIBUTE_FORTRAN_MPI1:
        /* Case 5: written in Fortran MPI-1, read in Fortran MPI-1
           (unity) */
        return *val->av_integer_pointer;
        break;

    case OMPI_ATTRIBUTE_FORTRAN_MPI2:
        /* Case 8: written in Fortran MPI-2, read in Fortran MPI-1 */
        return *val->av_integer_pointer;
        break;

    default:
        /* Should never reach here */
        return 0;
    }
}


/*
 * Take an attribute and translate it according to the cases listed in
 * the comments at the top of this file.
 *
 * This function does not fail -- it is only invoked in "safe"
 * situations.
 */
static MPI_Aint translate_to_fortran_mpi2(attribute_value_t *val)
{
    switch (val->av_set_from) {
    case OMPI_ATTRIBUTE_C:
        /* Case 3: written in C, read in Fortran MPI-2 */
        return (MPI_Aint) val->av_value;
        break;

    case OMPI_ATTRIBUTE_FORTRAN_MPI1:
        /* Case 6: written in Fortran MPI-1, read in Fortran MPI-2 */
        return (MPI_Aint) *val->av_integer_pointer;
        break;

    case OMPI_ATTRIBUTE_FORTRAN_MPI2:
        /* Case 9: written in Fortran MPI-2, read in Fortran MPI-2
           (unity) */
        return (MPI_Aint) val->av_value;
        break;

    default:
        /* Should never reach here */
        return 0;
    }
}
