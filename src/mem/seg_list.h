/*
 * $HEADER$
 */

#ifndef SEG_LIST_H
#define SEG_LIST_H

#include "ompi_config.h"
#include "class/ompi_list.h"
#include "threads/mutex.h"

struct ompi_seg_list_t
{
    ompi_object_t    super;
    size_t          sgl_min_bytes_pushed;
    size_t          sgl_max_bytes_pushed;
    size_t          sgl_bytes_pushed;
    int             sgl_max_consec_fail;
    int             sgl_consec_fail;
    ompi_mutex_t     sgl_lock;
    ompi_list_t      sgl_list;
};
typedef struct ompi_seg_list_t ompi_seg_list_t;

extern ompi_class_t     ompi_seg_list_t_class;

void ompi_sgl_construct(ompi_seg_list_t *slist);
void ompi_sgl_destruct(ompi_seg_list_t *slist);
void ompi_sgl_append_elt_chunk(
    ompi_seg_list_t *slist, 
    void *chunk, 
    size_t chunk_size,
    int n_elts, 
    size_t elt_size);

/*
 *
 *      Segment list accessor functions
 *
 */

#define ompi_sgl_lock_list(slist)  ompi_mutex_trylock(&slist->sgl_lock)
#define ompi_sgl_unlock_list(slist) ompi_mutex_unlock(&slist->sgl_lock)

static inline bool ompi_sgl_is_locked(ompi_seg_list_t *slist);
static inline bool ompi_sgl_is_locked(ompi_seg_list_t *slist)
{
    /* returns 1 if list is currently locked, otherwise 0. */
    int     ret;
    
    ret = ompi_mutex_trylock(&slist->sgl_lock);
    if ( !ret )
        ompi_mutex_unlock(&slist->sgl_lock);
    return !ret;
}

#define ompi_sgl_get_min_bytes_pushed(slist) ((slist)->sgl_min_bytes_pushed)
#define ompi_sgl_set_min_bytes_pushed(slist, min_bytes) \
    ((slist)->sgl_min_bytes_pushed = min_bytes)

#define ompi_sgl_get_max_bytes_pushed(slist)  ((slist)->sgl_max_bytes_pushed)
#define ompi_sgl_set_max_bytes_pushed(slist, min_bytes) \
    ((slist)->sgl_max_bytes_pushed = min_bytes)

#define ompi_sgl_get_bytes_pushed(slist) ((slist)->sgl_bytes_pushed)
#define ompi_sgl_set_bytes_pushed(slist, min_bytes) \
    ((slist)->sgl_bytes_pushed = min_bytes)

#define ompi_sgl_get_max_consec_fail(slist) ((slist)->sgl_max_consec_fail)
#define ompi_sgl_set_max_consec_fail(slist, max_fail) \
    ((slist)->sgl_max_consec_fail = max_fail)

#define ompi_sgl_get_consec_fail(slist) ((slist)->sgl_consec_fail)
#define ompi_sgl_set_consec_fail(slist, fail) \
    ((slist)->sgl_consec_fail = fail)

#define ompi_sgl_inc_consec_fail(slist) \
    ((slist)->sgl_consec_fail++)

#endif  /* SEG_LIST_H */



