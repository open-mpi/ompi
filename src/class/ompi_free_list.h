/*
 * $HEADER$
 */

#ifndef OMPI_FREE_LIST_H
#define OMPI_FREE_LIST_H

#include "ompi_config.h"
#include "class/ompi_list.h"
#include "include/constants.h"
#include "mca/mpool/mpool.h"

extern ompi_class_t ompi_free_list_t_class;
struct mca_mem_pool_t;


struct ompi_free_list_t
{
    ompi_list_t super;
    int fl_max_to_alloc;
    int fl_num_allocated;
    int fl_num_per_alloc;
    size_t fl_elem_size;
    ompi_class_t* fl_elem_class;
    mca_mpool_t* fl_mpool;
    ompi_mutex_t fl_lock;
};
typedef struct ompi_free_list_t ompi_free_list_t;


int ompi_free_list_init(
    ompi_free_list_t *flist, 
    size_t element_size,
    ompi_class_t* element_class,
    int num_elements_to_alloc,
    int max_elements_to_alloc,
    int num_elements_per_alloc,
    mca_mpool_t*);

int ompi_free_list_grow(ompi_free_list_t* flist, size_t num_elements);
    

#define OMPI_FREE_LIST_GET(fl, item, rc) \
{ \
    if(ompi_using_threads()) { \
        ompi_mutex_lock(&((fl)->fl_lock)); \
        item = ompi_list_remove_first(&((fl)->super)); \
        if(NULL == item) { \
            ompi_free_list_grow((fl), (fl)->fl_num_per_alloc); \
            item = ompi_list_remove_first(&((fl)->super)); \
        } \
        ompi_mutex_unlock(&((fl)->fl_lock)); \
    } else { \
        item = ompi_list_remove_first(&((fl)->super)); \
        if(NULL == item) { \
            ompi_free_list_grow((fl), (fl)->fl_num_per_alloc); \
            item = ompi_list_remove_first(&((fl)->super)); \
        } \
    }  \
    rc = (NULL == item) ?  OMPI_ERR_TEMP_OUT_OF_RESOURCE : OMPI_SUCCESS; \
} 


#define OMPI_FREE_LIST_RETURN(fl, item) \
    THREAD_SCOPED_LOCK(&((fl)->fl_lock), ompi_list_append(&((fl)->super), (item))); 

#endif 

