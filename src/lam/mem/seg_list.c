/*
 * $HEADER$
 */

#include "lam/mem/seg_list.h"
#include "lam/lfc/list.h"

/*
 * Public variable
 */
lam_class_info_t lam_seg_list_t_class_info = {
    "lam_seg_list_t",
    CLASS_INFO(lam_object_t), 
    (lam_construct_t) lam_sgl_construct,
    (lam_destruct_t) lam_sgl_destruct
};

void lam_sgl_construct(lam_seg_list_t *slist)
{
    OBJ_CONSTRUCT_SUPER(slist, lam_object_t);
    OBJ_CONSTRUCT(&slist->sgl_list, lam_list_t);
    lam_mutex_construct(&slist->sgl_lock);
    slist->sgl_min_bytes_pushed = 0;
    slist->sgl_max_bytes_pushed = 0;
    slist->sgl_bytes_pushed = 0;
    slist->sgl_max_consec_fail = 0;
    slist->sgl_consec_fail = 0;
}

void lam_sgl_destruct(lam_seg_list_t *slist)
{
    lam_list_destruct(&(slist->sgl_list));
    OBJ_DESTRUCT_SUPER(slist, lam_object_t);
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
