/*
 * $HEADER$
 */

#ifndef SEG_LIST_H
#define SEG_LIST_H

#include "lam_config.h"
#include "lam/lfc/list.h"
#include "lam/threads/mutex.h"

struct lam_seg_list_t
{
    lam_object_t    super;
    size_t          sgl_min_bytes_pushed;
    size_t          sgl_max_bytes_pushed;
    size_t          sgl_bytes_pushed;
    int             sgl_max_consec_fail;
    int             sgl_consec_fail;
    lam_mutex_t     sgl_lock;
    lam_list_t      sgl_list;
};
typedef struct lam_seg_list_t lam_seg_list_t;

extern lam_class_info_t     lam_seg_list_cls;

void lam_sgl_init(lam_seg_list_t *slist);
void lam_sgl_destroy(lam_seg_list_t *slist);
void lam_sgl_append_elt_chunk(
    lam_seg_list_t *slist, 
    void *chunk, 
    size_t chunk_size,
    int n_elts, 
    size_t elt_size);

/*
 *
 *      Segment list accessor functions
 *
 */

#define lam_sgl_lock_list(slist)  lam_mutex_trylock(&slist->sgl_lock)
#define lam_sgl_unlock_list(slist) lam_mutex_unlock(&slist->sgl_lock)

static inline bool lam_sgl_is_locked(lam_seg_list_t *slist);
static inline bool lam_sgl_is_locked(lam_seg_list_t *slist)
{
    /* returns 1 if list is currently locked, otherwise 0. */
    int     ret;
    
    ret = lam_mutex_trylock(&slist->sgl_lock);
    if ( !ret )
        lam_mutex_unlock(&slist->sgl_lock);
    return !ret;
}

#define lam_sgl_get_min_bytes_pushed(slist) ((slist)->sgl_min_bytes_pushed)
#define lam_sgl_set_min_bytes_pushed(slist, min_bytes) \
    ((slist)->sgl_min_bytes_pushed = min_bytes)

#define lam_sgl_get_max_bytes_pushed(slist)  ((slist)->sgl_max_bytes_pushed)
#define lam_sgl_set_max_bytes_pushed(slist, min_bytes) \
    ((slist)->sgl_max_bytes_pushed = min_bytes)

#define lam_sgl_get_bytes_pushed(slist) ((slist)->sgl_bytes_pushed)
#define lam_sgl_set_bytes_pushed(slist, min_bytes) \
    ((slist)->sgl_bytes_pushed = min_bytes)

#define lam_sgl_get_max_consec_fail(slist) ((slist)->sgl_max_consec_fail)
#define lam_sgl_set_max_consec_fail(slist, max_fail) \
    ((slist)->sgl_max_consec_fail = max_fail)

#define lam_sgl_get_consec_fail(slist) ((slist)->sgl_consec_fail)
#define lam_sgl_set_consec_fail(slist, fail) \
    ((slist)->sgl_consec_fail = fail)

#define lam_sgl_inc_consec_fail(slist) \
    ((slist)->sgl_consec_fail++)

#endif  /* SEG_LIST_H */



