/*
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

#include "lam/mem/seg_list.h"

lam_class_info_t seg_list_cls = {"lam_seg_list_t", &object_cls, 
    (class_init_t)lam_sgl_init, (class_destroy_t)lam_sgl_destroy};

void lam_sgl_init(lam_seg_list_t *slist)
{
    SUPER_INIT(slist, seg_list_cls.cls_parent);
    lam_dbl_list_init(&slist->sgl_list);
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
