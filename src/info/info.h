/*
 * $HEADER$
 */

#ifndef LAM_INFO_H
#define LAM_INFO_H

#include <string.h>

#include "mpi.h"
#include "util/strncpy.h"
#include "lfc/lam_list.h"
#include "include/lam.h"


/**
 * lam_info_t structure. MPI_Info is a pointer to this structure
 */
struct lam_info_t {
    lam_list_t super; /**< generic list pointer which is
                        * the container for (key,value) pairs */
    int i_fhandle; /**< fortran handle for info. This is needed
                     * for translation from fortran to C and vice versa */
};
typedef struct lam_info_t lam_info_t;

/**
 * lam_info_entry_t object. Each item in lam_info_list is of this
 * type. It contains (key,value) pairs
 */
struct lam_info_entry_t {
    lam_list_item_t super; /**< required for lam_list_t type */
    char *ie_value; /**< value part of the (key, value) pair.
                  * Maximum length is MPI_MAX_INFO_VAL */
    char ie_key[MPI_MAX_INFO_KEY + 1]; /**< "key" part of the (key, value)
                                     * pair */ 
};
typedef struct lam_info_entry_t lam_info_entry_t;

/**
 * Some declarations needed to use OBJ_NEW and OBJ_DESTRUCT macros
 */
extern lam_class_t lam_info_t_class;
extern lam_class_t lam_info_entry_t_class;

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
void lam_info_construct(lam_info_t *info);
void lam_info_destruct(lam_info_t *info);

void lam_info_entry_construct(lam_info_entry_t *entry);
void lam_info_entry_destruct(lam_info_entry_t *entry);
int lam_info_dup (lam_info_t *info, lam_info_t **newinfo);
int lam_info_set (lam_info_t *info, char *key, char *value);
int lam_info_free (lam_info_t **info);
int lam_info_get (lam_info_t *info, char *key, int valuelen,
                  char *value, int *flag);
int lam_info_delete (lam_info_t *info, char *key);
int lam_info_get_valuelen (lam_info_t *info, char *key, int *valuelen,
                           int *flag);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

/**
 * Iterate through the list and search for "key"
 *
 * @param info pointer to lam_info_t handle
 * @param key Key value to search for
 *
 * @retval (lam_info_entry_t *) to the object having the same key
 *         NULL if no matching item is found
 *
 * We need a function to go through the entries and find whether 
 * a given key is already present in the list. If so, this function
 * returns the item which matches this key. It was required 
 * often enough to make it into a function. No error checking on 
 * info is performed. 
 * Assumptions:
 * "info" is a valid key
 */
static inline lam_info_entry_t *lam_info_find_key (lam_info_t *info, 
                                                   char *key) {
    lam_info_entry_t *iterator;
    int nkeys;

    /* Iterate over all the entries. If the key is found, then 
     * return immediately. Else, the loop will fall of the edge
     * and NULL is returned
     */
    nkeys = lam_list_get_size(&(info->super));
    for (iterator = (lam_info_entry_t *)lam_list_get_first(&(info->super));
         nkeys > 0;
         nkeys--) {
        if (0 == strcmp(key, iterator->ie_key)) {
            return iterator;
        }
        iterator = (lam_info_entry_t *)iterator->super.lam_list_next;
    }
    return (lam_info_entry_t *)0;
}

/**
 * Get the number of keys defined on on an MPI_Info object
 * @param info Pointer to lam_info_t object.
 * @param nkeys Pointer to nkeys, which needs to be filled up.
 *
 * @retval The number of keys defined on info
 */
static inline int 
lam_info_get_nkeys(lam_info_t *info, int *nkeys) {
    *nkeys = (int) lam_list_get_size(&(info->super));
    return MPI_SUCCESS;
}

/**
 *   lam_info_get_nthkey - Get a key indexed by integer from an 'MPI_Info' o
 *
 *   @param info Pointer to lam_info_t object
 *   @param n index of key to retrieve (integer)
 *   @param key character string of at least 'MPI_MAX_INFO_KEY' characters
 *
 *   @retval MPI_SUCCESS
 *   @retval MPI_ERR_ARG
 */
static inline int 
lam_info_get_nthkey (lam_info_t *info, int n, char *key) {
    lam_info_entry_t *iterator;
    /*
     * Iterate over and over till we get to the nth key
     */
    iterator = (lam_info_entry_t *)lam_list_get_first(&(info->super));
    for ( ; n > 0; n--) {
        iterator = (lam_info_entry_t *)
        iterator->super.lam_list_next;
    }
    /*
     * iterator is of the type lam_list_item_t. We have to
     * cast it to lam_info_entry_t before we can use it to
     * access the value
     */
    strcpy(key, iterator->ie_key);
    return MPI_SUCCESS;
}

#endif /* LAM_INFO_H */
