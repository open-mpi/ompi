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

#include "lam/lfc/list.h"


/*
 *  List classes
 */


lam_class_info_t  lam_dbl_item_cls = {"lam_dbl_link_item_t", &lam_object_cls, 
    (class_init_t) lam_dbl_item_init, (class_destroy_t)lam_obj_destroy};
lam_class_info_t  lam_dbl_list_cls = {"lam_dbl_list_t", &lam_object_cls,
    (class_init_t)lam_dbl_init, (class_destroy_t)lam_dbl_destroy};


/*
 *
 *      lam_dbl_link_item_t interface
 *
 */

void lam_dbl_item_init(lam_dbl_item_t *item)
{
    SUPER_INIT(item, lam_dbl_item_cls.cls_parent);
    item->lam_dbl_next = item->lam_dbl_prev = 0;
    item->lam_dbl_type = 0;
}


/*
 *
 *      lam_dbl_list_t interface
 *
 */

void lam_dbl_init(lam_dbl_list_t *list)
{
    SUPER_INIT(list, lam_dbl_list_cls.cls_parent);
    list->lam_dbl_head = list->lam_dbl_tail = 0;
    list->lam_dbl_type = 0;
    list->lam_dbl_length = 0;
}

void lam_dbl_destroy(lam_dbl_list_t *list)
{
    /* release all items in list */
    lam_dbl_empty_list(list);
    SUPER_DESTROY(list, lam_dbl_list_cls.cls_parent);
}


void lam_dbl_append(lam_dbl_list_t *list, lam_dbl_item_t *item)
{
    /* Adds item to the end of the list. */
    item->lam_dbl_next = 0;
    item->lam_dbl_prev = list->lam_dbl_tail;
    if ( !(list->lam_dbl_head) )
        list->lam_dbl_head = item;
    if ( list->lam_dbl_tail )
        list->lam_dbl_tail->lam_dbl_next = item;
    
    list->lam_dbl_tail = item;
    list->lam_dbl_length++;
}

void lam_dbl_empty_list(lam_dbl_list_t *list)
{
    /* Since we don't retain the items, simply set
        each item's next and prev pointers to 0. */
    lam_dbl_item_t     *ptr, *next;

    ptr = list->lam_dbl_head;
    while ( ptr )
    {
        next = ptr->lam_dbl_next;
        ptr->lam_dbl_next = ptr->lam_dbl_prev = 0;
        ptr = next;
    }
    list->lam_dbl_head = list->lam_dbl_tail = 0;
    list->lam_dbl_length = 0;
}


int lam_dbl_insert(lam_dbl_list_t *list, lam_dbl_item_t *item, long long idx)
{
    /* Adds item to list at index and retains item. */
    int     i;
    lam_dbl_item_t     *ptr, *next;
    
    if ( idx >= list->lam_dbl_length )
        return 0;
    
    if ( 0 == idx )
    {
        lam_dbl_prepend(list, item);
    }
    else
    {
        ptr = list->lam_dbl_head;
        for ( i = 0; i < idx; i++ )
            ptr = ptr->lam_dbl_next;

        next = ptr->lam_dbl_next;
        item->lam_dbl_next = next;
        item->lam_dbl_prev = ptr;
        next->lam_dbl_prev = item;
        ptr->lam_dbl_next = item;
    }
    
    list->lam_dbl_length++;    
    return 1;
}

lam_dbl_item_t *lam_dbl_get_first_item(lam_dbl_list_t *list)
{
    /* Returns first item on list, but does not remove it from the list. */
    return list->lam_dbl_head;
}

lam_dbl_item_t *lam_dbl_get_last_item(lam_dbl_list_t *list)
{
    /* Returns last item on list, but does not remove it from the list. */
    return list->lam_dbl_tail;
}

void lam_dbl_prepend(lam_dbl_list_t *list, lam_dbl_item_t *item)
{
    /* Adds item to the front of the list and retains item. */
    item->lam_dbl_next = list->lam_dbl_head;
    item->lam_dbl_prev = 0;
    if ( list->lam_dbl_head )
        list->lam_dbl_head->lam_dbl_prev = item;
    list->lam_dbl_head = item;    
}

lam_dbl_item_t *lam_dbl_remove_first(lam_dbl_list_t *list)
{
    /*  Removes and returns first item on list.
        Caller now owns the item and should release the item
        when caller is done with it.
    */
    lam_dbl_item_t *item;
    if ( 0 == list->lam_dbl_length )
        return 0;
    
    list->lam_dbl_length--;
    item = list->lam_dbl_head;
    list->lam_dbl_head = item->lam_dbl_next;
    if ( list->lam_dbl_length )
    {
        item->lam_dbl_next->lam_dbl_prev = 0;
    }
    else
        list->lam_dbl_tail = 0;
    
    item->lam_dbl_next = item->lam_dbl_prev = 0;
    return item;
}

lam_dbl_item_t *lam_dbl_remove_last(lam_dbl_list_t *list)
{
    /*  Removes, releases and returns last item on list.
    Caller now owns the item and should release the item
    when caller is done with it.
    */
    lam_dbl_item_t  *item;
    
    if ( 0 == list->lam_dbl_length )
        return 0;
    
    list->lam_dbl_length--;
    item = list->lam_dbl_head;
    list->lam_dbl_tail = item->lam_dbl_prev;
    if ( list->lam_dbl_length )
        item->lam_dbl_prev->lam_dbl_next = 0;
    else
        list->lam_dbl_head = 0;
    item->lam_dbl_next = item->lam_dbl_prev = 0;
    return item;
}

