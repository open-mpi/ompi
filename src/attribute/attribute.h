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

/** @file
 *
 * Implementation for taking care of the attribute that can hang off a comm, 
 * win or datatype.
 */

#ifndef OMPI_ATTRIBUTE_H
#define OMPI_ATTRIBUTE_H

#include <string.h>
#include "mpi.h"

#include "ompi_config.h"
#include "include/constants.h"
#include "class/ompi_object.h"
#include "class/ompi_bitmap.h"
#include "class/ompi_hash_table.h"
#include "mca/gpr/gpr_types.h"

#define ATTR_HASH_SIZE 10

/* Flags for attribute will contain these */

#define OMPI_KEYVAL_PREDEFINED 1
#define OMPI_KEYVAL_F77 2

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
enum ompi_attribute_type_t{
    COMM_ATTR = 1, /**< The attribute belongs to a comm object. Starts
		      with 1 so that we can have it initialized to 0
		      using memset in the constructor */
#if OMPI_WANT_MPI2_ONE_SIDED
    WIN_ATTR, /**< The attribute belongs to a win object */
#endif
    TYPE_ATTR /**< The attribute belongs to datatype object */
};
typedef enum ompi_attribute_type_t ompi_attribute_type_t;


/* Fortran function pointer declarations for copy and delete. These
   will only be used here and not in the front end functions */

typedef void (MPI_F_copy_function)(int *, int *, int *, int *, int *,
				   int *, int *);
typedef void (MPI_F_delete_function)(int *, int *, int *, int *, int *);


/* Union to take care of proper casting of the function pointers
   passed from the front end functions depending on the type. This
   will avoid casting function pointers to void*  */

union ompi_attribute_fn_ptr_union_t {
    MPI_Comm_delete_attr_function *attr_communicator_delete_fn;
    MPI_Type_delete_attr_function *attr_datatype_delete_fn;
#if OMPI_WANT_MPI2_ONE_SIDED
    MPI_Win_delete_attr_function *attr_win_delete_fn;
#endif

    MPI_Comm_copy_attr_function *attr_communicator_copy_fn;
    MPI_Type_copy_attr_function *attr_datatype_copy_fn;
#if OMPI_WANT_MPI2_ONE_SIDED
    MPI_Win_copy_attr_function *attr_win_copy_fn;
#endif

    /* For Fortran functions */
    
    MPI_F_delete_function *attr_F_delete_fn;
    MPI_F_copy_function *attr_F_copy_fn;
};

typedef union ompi_attribute_fn_ptr_union_t ompi_attribute_fn_ptr_union_t;


struct ompi_attrkey_item_t {
    ompi_object_t super;
    ompi_attribute_type_t attr_type; /**< One of COMM/WIN/DTYPE. This
				       will be used to cast the
				       copy/delete attribute functions
				       properly and error checking */
    int attr_flag; /**< flag field: contains "OMPI_KEYVAL_PREDEFINED",
		      "OMPI_KEYVAL_F77"  */
    ompi_attribute_fn_ptr_union_t copy_attr_fn; /**< Copy function for the 
					     attribute */
    ompi_attribute_fn_ptr_union_t delete_attr_fn; /**< Delete function for the
					       attribute */
    void *extra_state; /**< Extra state of the attribute */
    int key; /**< Keep a track of which key this item belongs to, so that
		the key can be deleted when this object is destroyed */
};

typedef struct ompi_attrkey_item_t ompi_attrkey_item_t;
  

/* Functions */



/** 
 * Convenient way to initialize the attribute hash table per MPI-Object 
 */

static inline
int ompi_attr_hash_init(ompi_hash_table_t **keyhash)
{
   *keyhash = OBJ_NEW(ompi_hash_table_t);
    if (NULL == keyhash) {
	fprintf(stderr, "Error while creating the local attribute list\n");
	return MPI_ERR_SYSRESOURCE;
    }
    if (ompi_hash_table_init(*keyhash, ATTR_HASH_SIZE) != OMPI_SUCCESS)
	return MPI_ERR_SYSRESOURCE;
  
    return MPI_SUCCESS;
}

/**
 * Initialize the main attribute hash that stores the key and meta data
 *
 * @return OMPI return code
 */

int ompi_attr_init(void);

/**
 * Destroy the main attribute hash that stores the key and meta data
 */

int ompi_attr_finalize(void);


/**
 * Create a new key for use by attribute of Comm/Win/Datatype
 *
 * @param type           Type of attribute (COMM/WIN/DTYPE) (IN)
 * @param copy_attr_fn   Union variable containing the function pointer
 *                       to be used in order to copy the attribute (IN)
 * @param delete_attr_fn Function pointer to be used for deleting the 
 *                       attribute (IN)
 * @param key            The newly created key is returned here (OUT)
 * @param extra_state    Extra state to hang off/do some special things (IN)
 * @param flags          Flags for the key -- flags contain OMPI_KEYVAL_F77,
 *                       OMPI_KEYVAL_PREDEFINED
 * NOTE: I have taken the assumption that user cannot modify/delete
 * any predefined keys or the attributes attached. To accomplish this,
 * all MPI* calls will have OMPI_KEYVAL_PREDEFINED set as 0. MPI
 * implementors who will need to play with the predefined keys and
 * attributes would call the ompi* functions here and not the MPI*
 * functions, with OMPI_KEYVAL_PREDEFINED set to 1. 
 * END OF NOTE
 *
 * NOTE: For the function pointers, you need to create a variable of the 
 * union type "ompi_attribute_fn_ptr_union_t" and assign the proper field.
 * to be passed into this function
 * END OF NOTE
 *
 * @return OMPI return code

 *
 */

int ompi_attr_create_keyval(ompi_attribute_type_t type, 
			   ompi_attribute_fn_ptr_union_t copy_attr_fn, 
			   ompi_attribute_fn_ptr_union_t delete_attr_fn,
			   int *key, void *extra_state, int flags);

/**
 * Free an attribute keyval
 * @param type           Type of attribute (COMM/WIN/DTYPE) (IN)
 * @param key            key, which is set to MPI_KEY_INVALID (IN/OUT)
 * @return OMPI error code
 */

int ompi_attr_free_keyval(ompi_attribute_type_t type, int *key, 
                          bool predefined);

/**
 * Set an attribute on the comm/win/datatype
 * @param type           Type of attribute (COMM/WIN/DTYPE) (IN)
 * @param object         The actual Comm/Win/Datatype object (IN)
 * @param keyhash        The attribute hash table hanging on the object(IN/OUT)
 * @param key            Key val for the attribute (IN)
 * @param attribute      The actual attribute pointer (IN)
 * @param predefined     Whether the key is predefined or not 0/1 (IN)
 * @param need_lock      Whether we need to need to lock the keyval_lock or not
 * @return OMPI error code
 *
 * If (*keyhash) == NULL, a new keyhash will be created and
 * initialized.
 *
 * Note that need_lock should *always* be true when this function is
 * invoked from an top-level MPI function.  It is only false when this
 * function is invoked internally (i.e., when we already hold the
 * relevant locks, and we don't want to try to lock them again,
 * recursively).
 */

int ompi_attr_set(ompi_attribute_type_t type, void *object, 
                  ompi_hash_table_t **keyhash,
                  int key, void *attribute, bool predefined, bool need_lock);

/**
 * Get an attribute on the comm/win/datatype
 * @param keyhash        The attribute hash table hanging on the object(IN)
 * @param key            Key val for the attribute (IN)
 * @param attribute      The actual attribute pointer (OUT)
 * @param flag           Flag whether an attribute is associated 
 *                       with the key (OUT)
 * @return OMPI error code
 *
 */

int ompi_attr_get(ompi_hash_table_t *keyhash, int key, 
		 void *attribute, int *flag);


/**
 * Delete an attribute on the comm/win/datatype
 * @param type           Type of attribute (COMM/WIN/DTYPE) (IN)
 * @param object         The actual Comm/Win/Datatype object (IN)
 * @param keyhash        The attribute hash table hanging on the object(IN)
 * @param key            Key val for the attribute (IN)
 * @param predefined     Whether the key is predefined or not 0/1 (IN)
 * @param need_lock      Whether we need to need to lock the keyval_lock or not
 * @return OMPI error code
 *
 * Note that need_lock should *always* be true when this function is
 * invoked from an top-level MPI function.  It is only false when this
 * function is invoked internally (i.e., when we already hold the
 * relevant locks, and we don't want to try to lock them again,
 * recursively).
 */

int ompi_attr_delete(ompi_attribute_type_t type, void *object, 
                     ompi_hash_table_t *keyhash , int key,
                     bool predefined, bool need_lock);


/** 
 * This to be used from functions like MPI_*_DUP inorder to copy all
 * the attributes from the old Comm/Win/Dtype object to a new
 * object. 
 * @param type         Type of attribute (COMM/WIN/DTYPE) (IN)
 * @param old_object   The old COMM/WIN/DTYPE object (IN)
 * @param new_object   The new COMM/WIN/DTYPE object (IN)
 * @param keyhash      The attribute hash table hanging on old object(IN)
 * @param newkeyhash   The attribute hash table hanging on new object(IN)
 * @return OMPI error code
 *
 */

int ompi_attr_copy_all(ompi_attribute_type_t type, void *old_object, 
		      void *new_object, ompi_hash_table_t *oldkeyhash,
		      ompi_hash_table_t *newkeyhash);


/** 
 * This to be used to delete all the attributes from the Comm/Win/Dtype
 * object in one shot
 * @param type         Type of attribute (COMM/WIN/DTYPE) (IN)
 * @param object       The COMM/WIN/DTYPE object (IN)
 * @param keyhash        The attribute hash table hanging on the object(IN)
 * @return OMPI error code
 *
 */

int ompi_attr_delete_all(ompi_attribute_type_t type, void *object, 
			ompi_hash_table_t *keyhash);


/**
 * \internal
 *
 * Create all the predefined attributes
 *
 * @returns OMPI_SUCCESS
 */
int ompi_attr_create_predefined(void);


/**
 * \internal
 * Callback function to get data from registry and create predefined attributes
 *
 * @returns Nothing
 */
void ompi_attr_create_predefined_callback(
	orte_gpr_notify_data_t *data,
	void *cbdata);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


#endif /* OMPI_ATTRIBUTE_H */
