/*
 * $HEADER$
 */

#ifndef OMPI_INFO_H
#define OMPI_INFO_H

#include <string.h>

#include "mpi.h"
#include "util/strncpy.h"
#include "class/ompi_list.h"
#include "class/ompi_pointer_array.h"
#include "include/ompi.h"


/**
 * ompi_info_t structure. MPI_Info is a pointer to this structure
 */
struct ompi_info_t {
    ompi_list_t super; /**< generic list pointer which is
                        * the container for (key,value) pairs */
    int i_fhandle; /**< fortran handle for info. This is needed
                     * for translation from fortran to C and vice versa */
};
typedef struct ompi_info_t ompi_info_t;

/**
 * ompi_info_entry_t object. Each item in ompi_info_list is of this
 * type. It contains (key,value) pairs
 */
struct ompi_info_entry_t {
    ompi_list_item_t super; /**< required for ompi_list_t type */
    char *ie_value; /**< value part of the (key, value) pair.
                  * Maximum length is MPI_MAX_INFO_VAL */
    char ie_key[MPI_MAX_INFO_KEY + 1]; /**< "key" part of the (key, value)
                                     * pair */ 
};
typedef struct ompi_info_entry_t ompi_info_entry_t;

/**
 * Table for Fortran <-> C translation table
 */ 
extern ompi_pointer_array_t *ompi_info_f_to_c_table;

/**
 * Some declarations needed to use OBJ_NEW and OBJ_DESTRUCT macros
 */
extern ompi_class_t ompi_info_t_class;
extern ompi_class_t ompi_info_entry_t_class;

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
int ompi_info_init(void);
int ompi_info_finalize(void);
void ompi_info_construct(ompi_info_t *info);
void ompi_info_destruct(ompi_info_t *info);

void ompi_info_entry_construct(ompi_info_entry_t *entry);
void ompi_info_entry_destruct(ompi_info_entry_t *entry);
int ompi_info_dup (ompi_info_t *info, ompi_info_t **newinfo);
int ompi_info_set (ompi_info_t *info, char *key, char *value);
int ompi_info_free (ompi_info_t **info);
int ompi_info_get (ompi_info_t *info, char *key, int valuelen,
                  char *value, int *flag);
int ompi_info_delete (ompi_info_t *info, char *key);
int ompi_info_get_valuelen (ompi_info_t *info, char *key, int *valuelen,
                           int *flag);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

/**
 * Iterate through the list and search for "key"
 *
 * @param info pointer to ompi_info_t handle
 * @param key Key value to search for
 *
 * @retval (ompi_info_entry_t *) to the object having the same key
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
static inline ompi_info_entry_t *ompi_info_find_key (ompi_info_t *info, 
                                                   char *key) {
    ompi_info_entry_t *iterator;

    /* Iterate over all the entries. If the key is found, then 
     * return immediately. Else, the loop will fall of the edge
     * and NULL is returned
     */
    for (iterator = (ompi_info_entry_t *)ompi_list_get_first(&(info->super));
         NULL != iterator;
         iterator = (ompi_info_entry_t *)ompi_list_get_next(iterator)) {
        if (0 == strcmp(key, iterator->ie_key)) {
            return iterator;
        }
    }
    return (ompi_info_entry_t *)0;
}

/**
 * Get the number of keys defined on on an MPI_Info object
 * @param info Pointer to ompi_info_t object.
 * @param nkeys Pointer to nkeys, which needs to be filled up.
 *
 * @retval The number of keys defined on info
 */
static inline int 
ompi_info_get_nkeys(ompi_info_t *info, int *nkeys) {
    *nkeys = (int) ompi_list_get_size(&(info->super));
    return MPI_SUCCESS;
}

/**
 *   ompi_info_get_nthkey - Get a key indexed by integer from an 'MPI_Info' o
 *
 *   @param info Pointer to ompi_info_t object
 *   @param n index of key to retrieve (integer)
 *   @param key character string of at least 'MPI_MAX_INFO_KEY' characters
 *
 *   @retval MPI_SUCCESS
 *   @retval MPI_ERR_ARG
 */
static inline int 
ompi_info_get_nthkey (ompi_info_t *info, int n, char *key) {
    ompi_info_entry_t *iterator;
    /*
     * Iterate over and over till we get to the nth key
     */
    for (iterator = (ompi_info_entry_t *)ompi_list_get_first(&(info->super));
         n > 0;
         n--) {
         iterator = (ompi_info_entry_t *)ompi_list_get_next(iterator);
         if ( NULL == iterator) {
             return MPI_ERR_ARG;
         }
    }
    /*
     * iterator is of the type ompi_list_item_t. We have to
     * cast it to ompi_info_entry_t before we can use it to
     * access the value
     */
    strcpy(key, iterator->ie_key);
    return MPI_SUCCESS;
}

#endif /* OMPI_INFO_H */
