/*
 * $HEADER$
 */

#include "mem/seg_list.h"
#include "class/ompi_list.h"

/*
 * Public variable
 */
ompi_class_t ompi_seg_list_t_class = {
    "ompi_seg_list_t",
    OBJ_CLASS(ompi_object_t), 
    (ompi_construct_t) ompi_sgl_construct,
    (ompi_destruct_t) ompi_sgl_destruct
};

void ompi_sgl_construct(ompi_seg_list_t *slist)
{
    OBJ_CONSTRUCT(&slist->sgl_list, ompi_list_t);
    OBJ_CONSTRUCT(&slist->sgl_lock, ompi_mutex_t);
    slist->sgl_min_bytes_pushed = 0;
    slist->sgl_max_bytes_pushed = 0;
    slist->sgl_bytes_pushed = 0;
    slist->sgl_max_consec_fail = 0;
    slist->sgl_consec_fail = 0;
}

void ompi_sgl_destruct(ompi_seg_list_t *slist)
{
    OBJ_DESTRUCT(&slist->sgl_list);
    OBJ_DESTRUCT(&slist->sgl_lock);
}


void ompi_sgl_append_elt_chunk(
    ompi_seg_list_t *slist, 
    void *chunk, 
    size_t chunk_size,
    int n_elts, 
    size_t elt_size)
{
    /* Since this function could be called frequently, we do not
        want to compute the number of elements each time, so we
        require it to be passed as an arg.
    */
    int     i;
    char    *ptr;
    
    ptr = (char *)chunk;
    slist->sgl_bytes_pushed += chunk_size;
    for ( i = 0; i < n_elts; i++ )
    {
        ompi_list_append(&(slist->sgl_list), (ompi_list_item_t *)ptr);
        ptr += elt_size;
    }
}
