/*
 * $HEADER$
 */

#include "lam/mem/seg_list.h"

lam_class_info_t seg_list_cls = {"lam_seg_list_t", &lam_object_cls, 
    (class_init_t)lam_sgl_init, (class_destroy_t)lam_sgl_destroy};

void lam_sgl_init(lam_seg_list_t *slist)
{
    SUPER_INIT(slist, seg_list_cls.cls_parent);
    STATIC_INIT(slist->sgl_list, &lam_dbl_list_cls);
    lam_mtx_init(&slist->sgl_lock);
    slist->sgl_min_bytes_pushed = 0;
    slist->sgl_max_bytes_pushed = 0;
    slist->sgl_bytes_pushed = 0;
    slist->sgl_max_consec_fail = 0;
    slist->sgl_consec_fail = 0;
}

void lam_sgl_destroy(lam_seg_list_t *slist)
{
    lam_dbl_empty_list(&(slist->sgl_list));
    SUPER_DESTROY(slist, seg_list_cls.cls_parent);
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
        lam_dbl_append(&(slist->sgl_list), (lam_dbl_item_t *)ptr);
        ptr += elt_size;
    }
}
