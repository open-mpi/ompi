/*
 * $HEADER$
 */

/** @file
 *
 * Implementation for taking care of the attribute that can hang off a comm, 
 * win or datatype.
 */

#ifndef LAM_ATTRIBUTE_H
#define LAM_ATTRIBUTE_H

#include <string.h>
#include "mpi.h"

#include "lam_config.h"
#include "lfc/lam_object.h"
#include "lfc/lam_bitmap.h"
#include "lfc/lam_hash_table.h"
#include "communicator/communicator.h"
#include "datatype/datatype.h"
#include "win/win.h"


/* *******************************************************************  */
/* VPS: These to be moved into mpi.h or mpisys.h later on. This is
   just to make things compile now, should take other value in a
   different enum later */

enum lam_consts{
    LAM_PREDEFINED = 1,
    MPI_ERROR, 
    MPI_INVALID_ATTR_KEYVAL,
};

/* *******************************************************************  */

enum lam_attribute_type_t{
    COMM_ATTR = 1, /**< The attribute belongs to a comm object. Starts
		      with 1 so that we can have it initialized to 0
		      using memset in the constructor */
    WIN_ATTR, /**< The attribute belongs to a win object */
    TYPE_ATTR /**< The attribute belongs to datatype object */
};

typedef enum lam_attribute_type_t lam_attribute_type_t;


struct lam_attrkey_t {
    lam_hash_table_t super; /**< hash table pointer which will contain
			       <key,attr_meta_data> pair */
    int a_fhandle; /**<Fortran handle for language interoperability */
};

typedef struct lam_attrkey_t lam_attrkey_t;


struct lam_attrkey_item_t {
    lam_object_t super;
    lam_attribute_type_t attr_type; /**< One of COMM/WIN/DTYPE. This
				       will be used to cast the
				       copy/delete attribute functions
				       properly and error checking */
    int attr_flag; /**< flag field to denote if its predefined  */
    void *copy_attr_fn; /**< Copy function for the attribute */
    void *delete_attr_fn; /**< Delete function for the attribute */
    void *extra_state; /**< Extra state of the attribute */
    int key; /**< Keep a track of which key this item belongs to, so that
		the key can be deleted when this object is destroyed */
};

typedef struct lam_attrkey_item_t lam_attrkey_item_t;
  

/* Functions */


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * Initialize the main attribute hash that stores the key and meta data
 *
 * @return LAM return code
 */

int lam_attr_init(void);

/**
 * Destroy the main attribute hash that stores the key and meta data
 */

void lam_attr_destroy(void);


/**
 * Create a new key for use by attribute of Comm/Win/Datatype
 *
 * @param type           Type of attribute (COMM/WIN/DTYPE) (IN)
 * @param copy_attr_fn   Function pointer to be used in order to copy the
 *                       attribute (IN)
 * @param delete_attr_fn Function pointer to be used for deleting the 
 *                       attribute (IN)
 * @param key            The newly created key is returned here (OUT)
 * @param extra_state    Extra state to hang off/do some special things (IN)
 * @param predefined     Whether this key is should be treated as 
 *                       predefined (IN)

 * NOTE: I have taken the assumption that user cannot modify/delete
 * any predefined keys or the attributes attached. To accomplish this,
 * all MPI* calls will have predefined argument set as 0. MPI
 * implementors who will need to play with the predefined keys and
 * attributes would call the lam* functions here and not the MPI*
 * functions, with predefined argument set to 1. 
 * END OF NOTE
 *
 * @return LAM return code

 *
 */

int lam_attr_create_keyval(lam_attribute_type_t type, 
			   void *copy_attr_fn, void *delete_attr_fn,
			   int *key, void *extra_state, int predefined);

/**
 * Free an attribute keyval
 * @param type           Type of attribute (COMM/WIN/DTYPE) (IN)
 * @param key            key, which is set to MPI_KEY_INVALID (IN/OUT)
 * @return LAM error code
 */

int lam_attr_free_keyval(lam_attribute_type_t type, int *key, int predefined);

/**
 * Set an attribute on the comm/win/datatype
 * @param type           Type of attribute (COMM/WIN/DTYPE) (IN)
 * @param object         The actual Comm/Win/Datatype object (IN)
 * @param key            Key val for the attribute (IN)
 * @param attribute      The actual attribute pointer (IN)
 * @param predefined     Whether the key is predefined or not 0/1 (IN)
 * @return LAM error code
 *
 */

int lam_attr_set(lam_attribute_type_t type, void *object, 
		 int key, void *attribute, int predefined);

/**
 * Get an attribute on the comm/win/datatype
 * @param type           Type of attribute (COMM/WIN/DTYPE) (IN)
 * @param object         The actual Comm/Win/Datatype object (IN)
 * @param key            Key val for the attribute (IN)
 * @param attribute      The actual attribute pointer (OUTxb)
 * @param predefined     Whether the key is predefined or not 0/1 (IN)
 * @return LAM error code
 *
 */

int lam_attr_get(lam_attribute_type_t type, void *object, int key, 
		 void *attribute, int *flag);


/**
 * Delete an attribute on the comm/win/datatype
 * @param type           Type of attribute (COMM/WIN/DTYPE) (IN)
 * @param object         The actual Comm/Win/Datatype object (IN)
 * @param key            Key val for the attribute (IN)
 * @param flag           Whether the attribute was found or not (OUT)
 * @return LAM error code
 *
 */

int lam_attr_delete(lam_attribute_type_t type, void *object, int key,
		    int predefined);


/** 
 * This to be used from functions like MPI_*_DUP inorder to copy all
 * the attributes from the old Comm/Win/Dtype object to a new
 * object. 
 * @param type         Type of attribute (COMM/WIN/DTYPE) (IN)
 * @param old_object   The old COMM/WIN/DTYPE object (IN)
 * @param new_object   The new COMM/WIN/DTYPE object (IN)
 * @return LAM error code
 *
 */

int lam_attr_copy_all(lam_attribute_type_t type, void *old_object, 
		      void *new_object);


/** 
 * This to be used to delete all the attributes from the Comm/Win/Dtype
 * object in one shot
 * @param type         Type of attribute (COMM/WIN/DTYPE) (IN)
 * @param object       The COMM/WIN/DTYPE object (IN)
 * @return LAM error code
 *
 */

int lam_attr_delete_all(lam_attribute_type_t type, void *object);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


#endif /* LAM_ATTRIBUTE_H */
