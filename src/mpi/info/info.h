/*
 * $HEADER$
 */

#ifndef LAM_INFO_H
#define LAM_INFO_H

#include "mpi.h"
#include "lam/lfc/list.h"

/**
 * lam_info_t structure. MPI_Info is a pointer to this structure
 */
struct lam_info_t {
    char i_name[MPI_MAX_OBJECT_NAME]; /**< name of the info object
                                       * being instantiated */
    lam_list_t *lam_info_list; /**< generic list pointer which is
                                * the container for (key,value)
                                * pairs */
   /* Anju:
    * Should add appropriate member/s to support 
    * fortran translation.
    */
};
typedef struct lam_info_t lam_info_t;

/**
 * lam_info_entry_t object. Each item in lam_info_list is of this
 * type. It contains (key,value) pairs
 */
struct lam_info_entry_t {
    lam_list_item_t super; /**< required for lam_list_t type */
    char *value; /**< value part of the (key, value) pair.
                  * Maximum length is MPI_MAX_INFO_VAL */
    char key[MPI_MAX_INFO_KEY + 1]; /**< "key" part of the (key, value)
                                     * pair */ 
};
typedef struct lam_info_entry_t lam_info_entry_t;

#endif /* LAM_INFO_H */
