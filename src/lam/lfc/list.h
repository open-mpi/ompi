/*
 * $HEADER*
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

#ifndef LAM_LIST_H
#define LAM_LIST_H

#include "lam/lfc/object.h"

/*
 *
 *      Available Classes
 *
 */

extern lam_class_info_t   lam_dbl_item_cls;
extern lam_class_info_t   lam_dbl_list_cls;
typedef int               lam_dbl_type_t;


/*
 *
 *      lam_dbl_item_t interface
 *
 */
 
typedef struct _lam_dbl_item
{
    lam_object_t            super;
    lam_dbl_type_t          lam_dbl_type;
    struct _lam_dbl_item   *lam_dbl_next;
    struct _lam_dbl_item   *lam_dbl_prev;
} lam_dbl_item_t;

void lam_dbl_item_init(lam_dbl_item_t *item);
void lam_dbl_item_destroy(lam_dbl_item_t *item);

#define lam_dbl_get_next(item) \
    ((item) ? (((lam_dbl_item_t*)(item))->lam_dbl_next) : 0)

#define lam_dbl_get_prev(item) \
    ((item) ? (((lam_dbl_item_t*)(item))->lam_dbl_prev) : 0)

/*
 *
 *      lam_dbl_list_t interface
 *
 */
 
typedef struct _lam_dbl_list
{
    lam_object_t        super;
    lam_dbl_item_t     *lam_dbl_head;
    lam_dbl_item_t     *lam_dbl_tail;
    lam_dbl_type_t      lam_dbl_type;
    volatile size_t     lam_dbl_length;
} lam_dbl_list_t;


void lam_dbl_init(lam_dbl_list_t *list);
void lam_dbl_destroy(lam_dbl_list_t *list);

/*
 * Inlined accessor functions
 */

#define lam_dbl_get_type(list) \
    ((lam_dbl_list_t*)list)->lam_dbl_type

#define lam_dbl_set_type(list, type) \
    (((lam_dbl_list_t*)list)->lam_dbl_type = type)

#define lam_dbl_get_size(list) \
    ((lam_dbl_list_t*)list)->lam_dbl_length


/* 
 * Returns first item on list, but does not remove it from the list. 
 */
#define lam_dbl_get_first(list) \
    ((lam_dbl_list_t*)list)->lam_dbl_head


/* 
 * Returns last item on list, but does not remove it from the list. 
 */
#define lam_dbl_get_last(list) \
    ((lam_dbl_list_t*)list)->lam_dbl_tail


/* 
 * Adds item to the end of the list but does not retain item. 
 */
void lam_dbl_append(lam_dbl_list_t *list, lam_dbl_item_t *item);


/*
 * Remove item from the list.
 */
lam_dbl_item_t* lam_dbl_remove(lam_dbl_list_t *list, lam_dbl_item_t *item);


/* 
 * Removes all items in list and sets each
 * item's next and prev pointer to 0. 
 */
void lam_dbl_empty_list(lam_dbl_list_t *list);


/* Adds item to list at index and retains item. 
    Returns 1 if successful, 0 otherwise.
    0 <= idx < length_m
    Example: if idx = 2 and list = item1->item2->item3->item4, then
    after insert, list = item1->item2->item->item3->item4
*/
int lam_dbl_insert(lam_dbl_list_t *list, lam_dbl_item_t *item, long long idx);


/* 
 * Adds item to the front of the list and retains item. 
 */

void lam_dbl_prepend(lam_dbl_list_t *list, lam_dbl_item_t *item);


/*   
 *  Removes and returns first item on list.
 */
lam_dbl_item_t *lam_dbl_remove_first(lam_dbl_list_t *list);


/*   
 *  Removes and returns last item on list.
 */
lam_dbl_item_t *lam_dbl_remove_last(lam_dbl_list_t *list);

#endif /* LAM_LIST_H */
