/*
 * $HEADER$
 */

#include "lam/lfc/list.h"

/*
 *  List classes
 */


lam_class_info_t  lam_list_item_cls = {"lam_list_link_item_t", &lam_object_cls, 
    (class_init_t) lam_list_item_init, (class_destroy_t)lam_obj_destroy};
lam_class_info_t  lam_list_cls = {"lam_list_t", &lam_object_cls,
    (class_init_t)lam_list_init, (class_destroy_t)lam_list_destroy};


/*
 *
 *      lam_list_link_item_t interface
 *
 */

void lam_list_item_init(lam_list_item_t *item)
{
    SUPER_INIT(item, lam_list_item_cls.cls_parent);
    item->lam_list_next = item->lam_list_prev = 0;
    item->lam_list_type = 0;
}


/*
 *
 *      lam_list_list_t interface
 *
 */

void lam_list_init(lam_list_t *list)
{
    SUPER_INIT(list, lam_list_cls.cls_parent);
    list->lam_list_head = list->lam_list_tail = 0;
    list->lam_list_type = 0;
    list->lam_list_length = 0;
}

void lam_list_destroy(lam_list_t *list)
{
    /* release all items in list */
    lam_list_empty_list(list);
    SUPER_DESTROY(list, lam_list_cls.cls_parent);
}


void lam_list_append(lam_list_t *list, lam_list_item_t *item)
{
    /* Adds item to the end of the list. */
    item->lam_list_next = 0;
    item->lam_list_prev = list->lam_list_tail;
    if ( !(list->lam_list_head) )
        list->lam_list_head = item;
    if ( list->lam_list_tail )
        list->lam_list_tail->lam_list_next = item;
    
    list->lam_list_tail = item;
    list->lam_list_length++;
}

void lam_list_empty_list(lam_list_t *list)
{
    /* Since we don't retain the items, simply set
        each item's next and prev pointers to 0. */
    lam_list_item_t     *ptr, *next;

    ptr = list->lam_list_head;
    while ( ptr )
    {
        next = ptr->lam_list_next;
        ptr->lam_list_next = ptr->lam_list_prev = 0;
        ptr = next;
    }
    list->lam_list_head = list->lam_list_tail = 0;
    list->lam_list_length = 0;
}


int lam_list_insert(lam_list_t *list, lam_list_item_t *item, long long idx)
{
    /* Adds item to list at index and retains item. */
    int     i;
    lam_list_item_t     *ptr, *next;
    
    if ( idx >= list->lam_list_length )
        return 0;
    
    if ( 0 == idx )
    {
        lam_list_prepend(list, item);
    }
    else
    {
        ptr = list->lam_list_head;
        for ( i = 0; i < idx; i++ )
            ptr = ptr->lam_list_next;

        next = ptr->lam_list_next;
        item->lam_list_next = next;
        item->lam_list_prev = ptr;
        next->lam_list_prev = item;
        ptr->lam_list_next = item;
    }
    
    list->lam_list_length++;    
    return 1;
}

void lam_list_prepend(lam_list_t *list, lam_list_item_t *item)
{
    /* Adds item to the front of the list and retains item. */
    item->lam_list_next = list->lam_list_head;
    item->lam_list_prev = 0;
    if ( list->lam_list_head )
        list->lam_list_head->lam_list_prev = item;
    list->lam_list_head = item;    
}

lam_list_item_t *lam_list_remove_first(lam_list_t *list)
{
    /*  Removes and returns first item on list.
        Caller now owns the item and should release the item
        when caller is done with it.
    */
    lam_list_item_t *item;
    if ( 0 == list->lam_list_length )
        return 0;
    
    list->lam_list_length--;
    item = list->lam_list_head;
    list->lam_list_head = item->lam_list_next;
    if ( list->lam_list_length )
    {
        item->lam_list_next->lam_list_prev = 0;
    }
    else
        list->lam_list_tail = 0;
    
    item->lam_list_next = item->lam_list_prev = 0;
    return item;
}

lam_list_item_t *lam_list_remove_last(lam_list_t *list)
{
    /*  Removes, releases and returns last item on list.
    Caller now owns the item and should release the item
    when caller is done with it.
    */
    lam_list_item_t  *item;
    
    if ( 0 == list->lam_list_length )
        return 0;
    
    list->lam_list_length--;
    item = list->lam_list_head;
    list->lam_list_tail = item->lam_list_prev;
    if ( list->lam_list_length )
        item->lam_list_prev->lam_list_next = 0;
    else
        list->lam_list_head = 0;
    item->lam_list_next = item->lam_list_prev = 0;
    return item;
}

