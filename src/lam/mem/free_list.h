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

#ifndef _FREE_LIST_
#define _FREE_LIST_

#include "lam/base/list.h"
#include "lam/threads/mutex.h"
#include "lam/mem/seg_list.h"
#include "lam/mem/mem_pool.h"

/*
 *  Free list element interface
 *  Any item that goes into the free list must
 *  inherit from this class.
 */

typedef struct lam_flist_elt
{
    lam_dbl_item_t      super;
    int                 fle_pool_idx;
} lam_flist_elt_t;

void lam_flist_elt_init(lam_flist_elt_t*);
void lam_flist_elt_destroy(lam_flist_elt_t*);

#define lam_fle_get_idx(elt) (elt)->fle_pool_idx
#define lam_fle_set_idx(elt, idx) ((elt)->fle_pool_idx = idx)

/*
 * Memory affinity is almost certainly an int everywhere, but let's
 * make it a typedef in case we need to make it OS dependenent
 * sometime...
 */

typedef int lam_affinity_t;

typedef struct lam_free_list
{
    lam_object_t        super;
    lam_mutex_t         fl_lock;
    int                 fl_is_shared;
    lam_mem_pool_t     *fl_pool;
    const char         *fl_description;
    int                 fl_nlists;
    int                 fl_elt_per_chunk;
    size_t              fl_elt_size;
    lam_seg_list_t    **fl_free_lists;
    int                 fl_retry_more_resources;
    int                 fl_enforce_affinity;
    lam_affinity_t     *fl_affinity;            /* array of lam_affinity_t */
    int                 fl_threshold_grow;
    lam_class_info_t   *fl_elt_cls;        /* this will be used to create new free list elements. */
    
    /* for mem profiling */
    int           *fl_elt_out;
    int           *fl_elt_max;
    int           *fl_elt_sum;
    int           *fl_nevents;
    int           *fl_chunks_req;
    int           *fl_chunks_returned;
} lam_free_list_t;

extern lam_class_info_t free_list_cls;

void lam_frl_init(lam_free_list_t *flist);
void lam_frl_destroy(lam_free_list_t *flist);

/* lam_frl_init must have been called prior to calling this function */
int lam_frl_init_with(lam_free_list_t *flist, 
                  int nlists,
                  int pages_per_list,
                  size_t chunk_size, 
                  size_t page_size,
                  size_t element_size,
                  int min_pages_per_list, 
                  int max_pages_per_list,
                  int max_consec_req_fail,
                  const char *description,
                  bool_t retry_for_more_resources,
                  lam_affinity_t *affinity,
                  bool_t enforce_affinity,
                  lam_mem_pool_t *pool);


lam_flist_elt_t *lam_frl_get_elt(lam_free_list_t *flist, int index, int *err);
lam_flist_elt_t *lam_frl_get_elt_nl(lam_free_list_t *flist, int index, int *err);

int lam_frl_return_elt(lam_free_list_t *flist, int index, lam_flist_elt_t *item);
int lam_frl_return_elt_nl(lam_free_list_t *flist, int index, lam_flist_elt_t *item);

/*
 *      Accessor functions
 */

int lam_frl_get_thresh_grow(lam_free_list_t *flist);
void lam_frl_set_thresh_grow(lam_free_list_t *flist, int to_grow);

#endif 


