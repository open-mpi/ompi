/*
 * $HEADER$
 */

#include "lfc/lam_list.h"

/*
 *  List classes
 */

static void lam_list_item_construct(lam_list_item_t*);
static void lam_list_item_destruct(lam_list_item_t*);

lam_class_t  lam_list_item_t_class = {
    "lam_list_item_t",
    OBJ_CLASS(lam_object_t), 
    (lam_construct_t) lam_list_item_construct,
    (lam_destruct_t) lam_list_item_destruct
};

static void lam_list_construct(lam_list_t*);
static void lam_list_destruct(lam_list_t*);

OBJ_CLASS_INSTANCE(
    lam_list_t,
    lam_object_t,
    lam_list_construct,
    lam_list_destruct
);


/*
 *
 *      lam_list_link_item_t interface
 *
 */

static void lam_list_item_construct(lam_list_item_t *item)
{
    item->lam_list_next = item->lam_list_prev = NULL;
    item->lam_list_type = 0;
}

static void lam_list_item_destruct(lam_list_item_t *item)
{
}


/*
 *
 *      lam_list_list_t interface
 *
 */

static void lam_list_construct(lam_list_t *list)
{
    list->lam_list_head.lam_list_prev = NULL;
    list->lam_list_head.lam_list_next = &list->lam_list_tail;
    list->lam_list_tail.lam_list_prev = &list->lam_list_head;
    list->lam_list_tail.lam_list_next = NULL;
    list->lam_list_type = 0;
    list->lam_list_length = 0;
}


static void lam_list_destruct(lam_list_t *list)
{
    /* release all items in list */
    lam_list_construct(list);
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

