/*
 * $HEADER$
 */

#include "class/ompi_list.h"

/*
 *  List classes
 */

static void ompi_list_item_construct(ompi_list_item_t*);
static void ompi_list_item_destruct(ompi_list_item_t*);

ompi_class_t  ompi_list_item_t_class = {
    "ompi_list_item_t",
    OBJ_CLASS(ompi_object_t), 
    (ompi_construct_t) ompi_list_item_construct,
    (ompi_destruct_t) ompi_list_item_destruct
};

static void ompi_list_construct(ompi_list_t*);
static void ompi_list_destruct(ompi_list_t*);

OBJ_CLASS_INSTANCE(
    ompi_list_t,
    ompi_object_t,
    ompi_list_construct,
    ompi_list_destruct
);


/*
 *
 *      ompi_list_link_item_t interface
 *
 */

static void ompi_list_item_construct(ompi_list_item_t *item)
{
    item->ompi_list_next = item->ompi_list_prev = NULL;
}

static void ompi_list_item_destruct(ompi_list_item_t *item)
{
}


/*
 *
 *      ompi_list_list_t interface
 *
 */

static void ompi_list_construct(ompi_list_t *list)
{
    list->ompi_list_head.ompi_list_prev = NULL;
    list->ompi_list_head.ompi_list_next = &list->ompi_list_tail;
    list->ompi_list_tail.ompi_list_prev = &list->ompi_list_head;
    list->ompi_list_tail.ompi_list_next = NULL;
    list->ompi_list_length = 0;
}


static void ompi_list_destruct(ompi_list_t *list)
{
    /* release all items in list */
    ompi_list_construct(list);
}

int ompi_list_insert(ompi_list_t *list, ompi_list_item_t *item, long long idx)
{
    /* Adds item to list at index and retains item. */
    int     i;
    volatile ompi_list_item_t     *ptr, *next;
    
    if ( idx >= list->ompi_list_length )
        return 0;
    
    if ( 0 == idx )
    {
        ompi_list_prepend(list, item);
    }
    else
    {
        /* pointer to element 0 */
        ptr = list->ompi_list_head.ompi_list_next;
        for ( i = 0; i < idx-1; i++ )
            ptr = ptr->ompi_list_next;

        next = ptr->ompi_list_next;
        item->ompi_list_next = next;
        item->ompi_list_prev = ptr;
        next->ompi_list_prev = item;
        ptr->ompi_list_next = item;
    }
    
    list->ompi_list_length++;    
    return 1;
}

