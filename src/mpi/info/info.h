/*
 * $HEADER$
 */

#ifndef LAM_INFO_H
#define LAM_INFO_H

#include <string.h>

#include "mpi.h"
#include "lam/lfc/lam_list.h"
#include "lam/lam.h"


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
extern lam_class_info_t lam_info_t_class_info;
extern lam_class_info_t lam_info_entry_t_class_info;

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
void lam_info_construct(lam_info_t *info);
void lam_info_destruct(lam_info_t *info);

void lam_info_entry_construct(lam_info_entry_t *entry);
void lam_info_entry_destruct(lam_info_entry_t *entry);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

/**
 * Iterate through the list and search for "key"
 *
 * @param info MPI_Info handle
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
static inline lam_info_entry_t *lam_info_find_key (MPI_Info info, 
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

#endif /* LAM_INFO_H */
