/*
 * $HEADER$
 *
 * Copyright 2002-2003. The Regents of the University of California. This material
 * was produced under U.S. Government contract W-7405-ENG-36 for Los Alamos
 * National Laboratory, which is operated by the University of California for
 * the U.S. Department of Energy. The Government is granted for itself and
 * others acting on its behalf a paid-up, nonexclusive, irrevocable worldwide
 * license in this material to reproduce, prepare derivative works, and
 * perform publicly and display publicly. Beginning five (5) years after
 * October 10,2002 subject to additional five-year worldwide renewals, the
 * Government is granted for itself and others acting on its behalf a paid-up,
 * nonexclusive, irrevocable worldwide license in this material to reproduce,
 * prepare derivative works, distribute copies to the public, perform publicly
 * and display publicly, and to permit others to do so. NEITHER THE UNITED
 * STATES NOR THE UNITED STATES DEPARTMENT OF ENERGY, NOR THE UNIVERSITY OF
 * CALIFORNIA, NOR ANY OF THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR
 * IMPLIED, OR ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY,
 * COMPLETENESS, OR USEFULNESS OF ANY INFORMATION, APPARATUS, PRODUCT, OR
 * PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD NOT INFRINGE PRIVATELY
 * OWNED RIGHTS.
                                                                                                     
 * Additionally, this program is free software; you can distribute it and/or
 * modify it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2 of the License,
 * or any later version.  Accordingly, this program is distributed in the hope
 * that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#ifndef LAM_SEG_LIST_H
#define LAM_SEG_LIST_H

#include "lam/lfc/list.h"
#include "lam/threads/mutex.h"

typedef struct lam_seg_list
{
    lam_object_t    super;
    size_t          sgl_min_bytes_pushed;
    size_t          sgl_max_bytes_pushed;
    size_t          sgl_bytes_pushed;
    int             sgl_max_consec_fail;
    int             sgl_consec_fail;
    lam_mutex_t     sgl_lock;
    lam_dbl_list_t  sgl_list;
} lam_seg_list_t;

extern lam_class_info_t     seg_list_cls;

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

#define lam_sgl_lock_list(slist)  lam_mtx_trylock(&slist->sgl_lock)
#define lam_sgl_unlock_list(slist) lam_mtx_unlock(&slist->sgl_lock)

static inline int lam_sgl_is_locked(lam_seg_list_t *slist)
{
    /* returns 1 if list is currently locked, otherwise 0. */
    int     ret;
    
    ret = lam_mtx_trylock(&slist->sgl_lock);
    if ( !ret )
        lam_mtx_unlock(&slist->sgl_lock);
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

#endif  /* LAM_SEG_LIST_H */
