/*
 * $HEADER$
 */

#include "lam/lfc/list.h"

/*
 *  List classes
 */


lam_class_info_t  lam_list_item_cls = {"lam_list_item_t", &lam_object_cls, 
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
    item->lam_list_next = item->lam_list_prev = NULL;
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
    list->lam_list_head.lam_list_prev = NULL;
    list->lam_list_head.lam_list_next = &list->lam_list_tail;
    list->lam_list_tail.lam_list_prev = &list->lam_list_head;
    list->lam_list_tail.lam_list_next = NULL;
    list->lam_list_type = 0;
    list->lam_list_length = 0;
}


void lam_list_destroy(lam_list_t *list)
{
    /* release all items in list */
    lam_list_init(list);
    SUPER_DESTROY(list, lam_list_cls.cls_parent);
}

    
/**
 * Adds item to the end of the list. 
 *
 * @param list List accepting new item (IN/OUT)
 *
 * @param item Item being put on the new list (IN)
 *
 */
void lam_list_append(lam_list_t *list, lam_list_item_t *item)
{
    /* set new element's previous pointer */
    item->lam_list_prev=list->lam_list_tail.lam_list_prev;

    /* reset previous pointer on current last element */
    list->lam_list_tail.lam_list_prev->lam_list_next=item;

    /* reset new element's next pointer */
    item->lam_list_next=&(list->lam_list_tail);

    /* reset the list's tail element previous pointer */
    list->lam_list_tail.lam_list_prev = item;

    /* increment list element counter */
    list->lam_list_length++;
}

int lam_list_insert(lam_list_t *list, lam_list_item_t *item, long long idx)
{
    /* Adds item to list at index and retains item. */
    int     i;
    volatile lam_list_item_t     *ptr, *next;
    
    if ( idx >= list->lam_list_length )
        return 0;
    
    if ( 0 == idx )
    {
        lam_list_prepend(list, item);
    }
    else
    {
        /* pointer to element 0 */
        ptr = list->lam_list_head.lam_list_next;
        for ( i = 0; i < idx-1; i++ )
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

/* 
 * Adds item to the front of the list and retains item.
 *
 * @param list List accepting new item (IN/OUT)
 *
 * @param item Item being put on the new list (IN)
 *
 */
void lam_list_prepend(lam_list_t *list, lam_list_item_t *item)
{
    /* reset item's next pointer */
    item->lam_list_next = list->lam_list_head.lam_list_next;

    /* reset item's previous pointer */
    item->lam_list_prev = &(list->lam_list_head);

    /* reset previous first element's previous poiner */
    list->lam_list_head.lam_list_next->lam_list_prev = item;

    /* reset head's next pointer */
    list->lam_list_head.lam_list_next = item;

    /* increment list element counter */
    list->lam_list_length++;    
}

lam_list_item_t *lam_list_remove_first(lam_list_t *list)
{
    /*  Removes and returns first item on list.
        Caller now owns the item and should release the item
        when caller is done with it.
    */
    volatile lam_list_item_t *item;
    if ( 0 == list->lam_list_length )
        return (lam_list_item_t *)NULL;
    
    /* reset list length counter */
    list->lam_list_length--;

    /* get pointer to first element on the list */
    item = list->lam_list_head.lam_list_next;

    /* reset previous pointer of next item on the list */
    item->lam_list_next->lam_list_prev=item->lam_list_prev;

    /* reset the head next pointer */
    list->lam_list_head.lam_list_next=item->lam_list_next;

#ifdef LAM_ENABLE_DEBUG
    /* debug code */
    item->lam_list_prev=(lam_list_item_t *)NULL;
    item->lam_list_next=(lam_list_item_t *)NULL;
#endif

    return (lam_list_item_t *) item;
}

lam_list_item_t *lam_list_remove_last(lam_list_t *list)
{
    /*  Removes, releases and returns last item on list.
    Caller now owns the item and should release the item
    when caller is done with it.
    */
    volatile lam_list_item_t  *item;
    
    if ( 0 == list->lam_list_length )
        return (lam_list_item_t *)NULL;
    
    /* reset list length counter */
    list->lam_list_length--;

    /* get item */
    item = list->lam_list_tail.lam_list_prev;

    /* reset previous pointer on next to last pointer */
    item->lam_list_prev->lam_list_next=item->lam_list_next;

    /* reset tail's previous pointer */
    list->lam_list_tail.lam_list_prev=item->lam_list_prev;
    
#ifdef LAM_ENABLE_DEBUG
    /* debug code */
    item->lam_list_next = item->lam_list_prev = (lam_list_item_t *)NULL;
#endif

    return (lam_list_item_t *) item;
}

