/*
 * $HEADER$
 */

#ifndef _MPI_F2C_TABLE
#define _MPI_F2C_TABLE


#include "lam/threads/mutex.h"

/*
 * typedefs
 */
typedef struct lam_pointer_array lam_pointer_array_t;

/*
 * dynamic pointer table (used for MPI requests, dataytypes and ops)
 */
struct lam_pointer_array {
    lam_mutex_t lock;
    size_t lowest_free;
    size_t number_free;
    size_t size;
    void **addr;
};

int lam_pointer_array_add(lam_pointer_array_t *table, void *ptr);
int lam_pointer_array_set_item(lam_pointer_array_t *table, 
        size_t index, void *value);
void *lam_pointer_array_get_item(lam_pointer_array_t *table, int index);

#endif /* _MPI_F2C_TABLE */
