/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "include/constants.h"
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


/*
 * Reset all the pointers to be NULL -- do not actually destroy
 * anything.
 */
static void ompi_list_destruct(ompi_list_t *list)
{
/*     ompi_list_item_t *item; */

/*     if (NULL != list) { */
/* 	while (NULL != (item = ompi_list_remove_first(list))) { */
/* 	    OBJ_RELEASE(item); */
/* 	} */

	ompi_list_construct(list);
/*     } */
}


/*
 * Insert an item at a specific place in a list
 */
bool ompi_list_insert(ompi_list_t *list, ompi_list_item_t *item, long long idx)
{
    /* Adds item to list at index and retains item. */
    int     i;
    volatile ompi_list_item_t *ptr, *next;
    
    if ( idx >= (long long)list->ompi_list_length ) {
        return false;
    }
    
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
    return true;
}


static
void
ompi_list_transfer(ompi_list_item_t *pos, ompi_list_item_t *begin,
                   ompi_list_item_t *end)
{
    volatile ompi_list_item_t *tmp;

    if (pos != end) {
        /* remove [begin, end) */
        end->ompi_list_prev->ompi_list_next = pos;
        begin->ompi_list_prev->ompi_list_next = end;
        pos->ompi_list_prev->ompi_list_next = begin;

        /* splice into new position before pos */
        tmp = pos->ompi_list_prev;
        pos->ompi_list_prev = end->ompi_list_prev;
        end->ompi_list_prev = begin->ompi_list_prev;
        begin->ompi_list_prev = tmp;
    }
}


void
ompi_list_join(ompi_list_t *thislist, ompi_list_item_t *pos, 
               ompi_list_t *xlist)
{
    if (0 != ompi_list_get_size(xlist)) {
        ompi_list_transfer(pos, ompi_list_get_first(xlist),
                           ompi_list_get_end(xlist));

        /* fix the sizes */
        thislist->ompi_list_length += xlist->ompi_list_length;
        xlist->ompi_list_length = 0;
    }
}


void
ompi_list_splice(ompi_list_t *thislist, ompi_list_item_t *pos,
                 ompi_list_t *xlist, ompi_list_item_t *first,
                 ompi_list_item_t *last)
{ 
    size_t change = 0;
    ompi_list_item_t *tmp;

    if (first != last) {
        /* figure out how many things we are going to move (have to do
         * first, since last might be end and then we wouldn't be able
         * to run the loop) 
         */
        for (tmp = first ; tmp != last ; tmp = ompi_list_get_next(tmp)) {
            change++;
        }

        ompi_list_transfer(pos, first, last);

        /* fix the sizes */
        thislist->ompi_list_length += change;
        xlist->ompi_list_length -= change;
    }
}


int ompi_list_sort(ompi_list_t* list, ompi_list_item_compare_fn_t compare)
{
    ompi_list_item_t* item;
    ompi_list_item_t** items;
    size_t i, index=0;

    if (0 == list->ompi_list_length) {
        return OMPI_SUCCESS;
    }
    items = (ompi_list_item_t**)malloc(sizeof(ompi_list_item_t*) * 
                                       list->ompi_list_length);

    if (NULL == items) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    while(NULL != (item = ompi_list_remove_first(list))) {
        items[index++] = item;
    }
    
    qsort(items, index, sizeof(ompi_list_item_t*), 
          (int(*)(const void*,const void*))compare);
    for (i=0; i<index; i++) {
        ompi_list_append(list,items[i]);
    }
    free(items);
    return OMPI_SUCCESS;
}
