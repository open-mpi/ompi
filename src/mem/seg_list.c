/*
 * $HEADER$
 */

#include "mem/seg_list.h"
#include "lfc/lam_list.h"

/*
 * Public variable
 */
lam_class_t lam_seg_list_t_class = {
    "lam_seg_list_t",
    OBJ_CLASS(lam_object_t), 
    (lam_construct_t) lam_sgl_construct,
    (lam_destruct_t) lam_sgl_destruct
};

void lam_sgl_construct(lam_seg_list_t *slist)
{
    OBJ_CONSTRUCT(&slist->sgl_list, lam_list_t);
    OBJ_CONSTRUCT(&slist->sgl_lock, lam_mutex_t);
    slist->sgl_min_bytes_pushed = 0;
    slist->sgl_max_bytes_pushed = 0;
    slist->sgl_bytes_pushed = 0;
    slist->sgl_max_consec_fail = 0;
    slist->sgl_consec_fail = 0;
}

void lam_sgl_destruct(lam_seg_list_t *slist)
{
    OBJ_DESTRUCT(&slist->sgl_list);
    OBJ_DESTRUCT(&slist->sgl_lock);
}


void lam_sgl_append_elt_chunk(
    lam_seg_list_t *slist, 
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
        lam_list_append(&(slist->sgl_list), (lam_list_item_t *)ptr);
        ptr += elt_size;
    }
}
