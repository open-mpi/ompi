/*
 * $HEADER$
 */

#ifndef OMPI_LIST_H
#define OMPI_LIST_H

#include <stdio.h>
#include "class/ompi_object.h"

/*
 *
 *      Available Classes
 *
 */

extern ompi_class_t   ompi_list_item_t_class;
extern ompi_class_t   ompi_list_t_class;
typedef int               ompi_list_type_t;


/*
 *
 *      ompi_dbl_item_t interface
 *
 */
 
typedef struct ompi_list_item
{
    ompi_object_t            super;
    ompi_list_type_t         ompi_list_type;
    volatile struct ompi_list_item   *ompi_list_next;
    volatile struct ompi_list_item   *ompi_list_prev;
} ompi_list_item_t;


#define ompi_list_get_next(item) \
    ((item) ? ((ompi_list_item_t*) ((ompi_list_item_t*)(item))->ompi_list_next) : NULL)

#define ompi_list_get_prev(item) \
    ((item) ? ((ompi_list_item_t*) ((ompi_list_item_t*)(item))->ompi_list_prev) : NULL)

/*
 *
 *      ompi_list_list_t interface
 *
 */
 
typedef struct ompi_list
{
    ompi_object_t        super;
    ompi_list_item_t     ompi_list_head;
    ompi_list_item_t     ompi_list_tail;
    ompi_list_type_t     ompi_list_type;
    volatile size_t     ompi_list_length;
} ompi_list_t;


/*
 * Inlined accessor functions
 */

static inline ompi_list_type_t ompi_list_get_type(ompi_list_t* list)
{
    return list->ompi_list_type;
}

static inline void ompi_list_set_type(ompi_list_t* list, ompi_list_type_t type)
{
    list->ompi_list_type = type;
}

static inline size_t ompi_list_get_size(ompi_list_t* list)
{
    return list->ompi_list_length;
}

static inline void ompi_list_set_size(ompi_list_t* list, size_t size)
{
    list->ompi_list_length=size;
}

/* 
 * Returns first item on list, but does not remove it from the list. 
 */
static inline ompi_list_item_t* ompi_list_get_first(ompi_list_t* list)
{
    return (ompi_list_item_t *)list->ompi_list_head.ompi_list_next;
}

/* 
 * Returns last item on list, but does not remove it from the list. 
 */
static inline ompi_list_item_t* ompi_list_get_last(ompi_list_t* list)
{
    return (ompi_list_item_t *)list->ompi_list_tail.ompi_list_prev;
}

/* 
 * Returns beginning of list, an invalid list entry.
 */
static inline ompi_list_item_t* ompi_list_get_begin(ompi_list_t* list)
{
    return &(list->ompi_list_head);
}

/* 
 * Returns end of list, an invalid list entry.
 */
static inline ompi_list_item_t* ompi_list_get_end(ompi_list_t* list)
{
    return &(list->ompi_list_tail);
}

/*
 * Removes the specified item from the list.  It retuns the element
 * pointing to item, so that a loop traversing the list from the
 * top of the list down can proceed.
 */
static inline ompi_list_item_t *ompi_list_remove_item
  (ompi_list_t *list, ompi_list_item_t *item)
{
#if OMPI_ENABLE_DEBUG
    ompi_list_item_t *item_ptr;
    bool found;
#endif

#if OMPI_ENABLE_DEBUG
    found = false;
#endif

#if OMPI_ENABLE_DEBUG
    /* check to see that the item is in the list */
    for (item_ptr = ompi_list_get_first(list);
            item_ptr != ompi_list_get_end(list);
            item_ptr = (ompi_list_item_t *)(item_ptr->ompi_list_next)) {
        if (item_ptr == (ompi_list_item_t *) item) {
            found = true;
            break;
        }
    }
    if (!found) {
        fprintf(stderr," Warning :: ompi_list_remove_item - the item %p is not on the list %p \n",(void*) item, (void*) list);
        fflush(stderr);
        return (ompi_list_item_t *)NULL;
    }
#endif

    /* reset next pointer of previous element */
    item->ompi_list_prev->ompi_list_next=item->ompi_list_next;

    /* reset previous pointer of next element */
    item->ompi_list_next->ompi_list_prev=item->ompi_list_prev;

    list->ompi_list_length--;
    return (ompi_list_item_t *)item->ompi_list_prev;
}

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  /* 
   * Adds item to the end of the list but does not retain item. 
   */
                                                                                                      
    static inline void ompi_list_append(ompi_list_t *list, ompi_list_item_t *item)
    {
        /* set new element's previous pointer */
        item->ompi_list_prev=list->ompi_list_tail.ompi_list_prev;

        /* reset previous pointer on current last element */
        list->ompi_list_tail.ompi_list_prev->ompi_list_next=item;

        /* reset new element's next pointer */
        item->ompi_list_next=&(list->ompi_list_tail);

        /* reset the list's tail element previous pointer */
        list->ompi_list_tail.ompi_list_prev = item;

        /* increment list element counter */
        list->ompi_list_length++;
    }


  /* Adds item to list at index and retains item. 
     Returns 1 if successful, 0 otherwise.
     0 <= idx < length_m
     Example: if idx = 2 and list = item1->item2->item3->item4, then
     after insert, list = item1->item2->item->item3->item4
  */
   int ompi_list_insert(ompi_list_t *list, ompi_list_item_t *item, long long idx);


  /* 
   * Adds item to the front of the list and retains item. 
   */
    static inline void ompi_list_prepend(ompi_list_t *list, ompi_list_item_t *item) 
    {
        /* reset item's next pointer */
        item->ompi_list_next = list->ompi_list_head.ompi_list_next;

        /* reset item's previous pointer */
        item->ompi_list_prev = &(list->ompi_list_head);

        /* reset previous first element's previous poiner */
        list->ompi_list_head.ompi_list_next->ompi_list_prev = item;

        /* reset head's next pointer */
        list->ompi_list_head.ompi_list_next = item;

        /* increment list element counter */
        list->ompi_list_length++;
    }
   


  /*   
   *  Removes and returns first item on list.
   */
    static inline ompi_list_item_t *ompi_list_remove_first(ompi_list_t *list)
    {
        /*  Removes and returns first item on list.
            Caller now owns the item and should release the item
            when caller is done with it.
        */
        volatile ompi_list_item_t *item;
        if ( 0 == list->ompi_list_length )
            return (ompi_list_item_t *)NULL;
                                                                                                          
        /* reset list length counter */
        list->ompi_list_length--;

        /* get pointer to first element on the list */
        item = list->ompi_list_head.ompi_list_next;
                                                                                                      
        /* reset previous pointer of next item on the list */
        item->ompi_list_next->ompi_list_prev=item->ompi_list_prev;
                                                                                                      
        /* reset the head next pointer */
        list->ompi_list_head.ompi_list_next=item->ompi_list_next;
                                                                                                      
#if OMPI_ENABLE_DEBUG
        /* debug code */
        item->ompi_list_prev=(ompi_list_item_t *)NULL;
        item->ompi_list_next=(ompi_list_item_t *)NULL;
#endif
        return (ompi_list_item_t *) item;
    }

  /*   
   *  Removes and returns last item on list.
   */
   static inline ompi_list_item_t *ompi_list_remove_last(ompi_list_t *list)
   {
        /*  Removes, releases and returns last item on list.
        Caller now owns the item and should release the item
        when caller is done with it.
        */
        volatile ompi_list_item_t  *item;
        if ( 0 == list->ompi_list_length )
            return (ompi_list_item_t *)NULL;
                                                                                                       
        /* reset list length counter */
        list->ompi_list_length--;

        /* get item */
        item = list->ompi_list_tail.ompi_list_prev;
                                                                                                       
        /* reset previous pointer on next to last pointer */
        item->ompi_list_prev->ompi_list_next=item->ompi_list_next;
                                                                                                       
        /* reset tail's previous pointer */
        list->ompi_list_tail.ompi_list_prev=item->ompi_list_prev;

#if OMPI_ENABLE_DEBUG
        /* debug code */
        item->ompi_list_next = item->ompi_list_prev = (ompi_list_item_t *)NULL;
#endif
        return (ompi_list_item_t *) item;
    }

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* OMPI_LIST_H */
