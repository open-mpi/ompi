/*
 * $HEADER$
 */

#ifndef LAM_LIST_H
#define LAM_LIST_H

#include <stdio.h>
#include "lfc/lam_object.h"

/*
 *
 *      Available Classes
 *
 */

extern lam_class_t   lam_list_item_t_class;
extern lam_class_t   lam_list_t_class;
typedef int               lam_list_type_t;


/*
 *
 *      lam_dbl_item_t interface
 *
 */
 
typedef struct lam_list_item
{
    lam_object_t            super;
    lam_list_type_t         lam_list_type;
    volatile struct lam_list_item   *lam_list_next;
    volatile struct lam_list_item   *lam_list_prev;
} lam_list_item_t;


#define lam_list_get_next(item) \
    ((item) ? ((lam_list_item_t*) ((lam_list_item_t*)(item))->lam_list_next) : NULL)

#define lam_list_get_prev(item) \
    ((item) ? ((lam_list_item_t*) ((lam_list_item_t*)(item))->lam_list_prev) : NULL)

/*
 *
 *      lam_list_list_t interface
 *
 */
 
typedef struct lam_list
{
    lam_object_t        super;
    lam_list_item_t     lam_list_head;
    lam_list_item_t     lam_list_tail;
    lam_list_type_t     lam_list_type;
    volatile size_t     lam_list_length;
} lam_list_t;


/*
 * Inlined accessor functions
 */

static inline lam_list_type_t lam_list_get_type(lam_list_t* list)
{
    return list->lam_list_type;
}

static inline void lam_list_set_type(lam_list_t* list, lam_list_type_t type)
{
    list->lam_list_type = type;
}

static inline size_t lam_list_get_size(lam_list_t* list)
{
    return list->lam_list_length;
}

static inline void lam_list_set_size(lam_list_t* list, size_t size)
{
    list->lam_list_length=size;
}

/* 
 * Returns first item on list, but does not remove it from the list. 
 */
static inline lam_list_item_t* lam_list_get_first(lam_list_t* list)
{
    return (lam_list_item_t *)list->lam_list_head.lam_list_next;
}

/* 
 * Returns last item on list, but does not remove it from the list. 
 */
static inline lam_list_item_t* lam_list_get_last(lam_list_t* list)
{
    return (lam_list_item_t *)list->lam_list_tail.lam_list_prev;
}

/* 
 * Returns beginning of list, an invalid list entry.
 */
static inline lam_list_item_t* lam_list_get_begin(lam_list_t* list)
{
    return &(list->lam_list_head);
}

/* 
 * Returns end of list, an invalid list entry.
 */
static inline lam_list_item_t* lam_list_get_end(lam_list_t* list)
{
    return &(list->lam_list_tail);
}

/*
 * Removes the specified item from the list.  It retuns the element
 * pointing to item, so that a loop traversing the list from the
 * top of the list down can proceed.
 */
static inline lam_list_item_t *lam_list_remove_item
  (lam_list_t *list, lam_list_item_t *item)
{
#if LAM_ENABLE_DEBUG
    lam_list_item_t *item_ptr;
    bool found;
#endif

#if LAM_ENABLE_DEBUG
    found = false;
#endif

#if LAM_ENABLE_DEBUG
    /* check to see that the item is in the list */
    for (item_ptr = lam_list_get_first(list);
            item_ptr != lam_list_get_end(list);
            item_ptr = (lam_list_item_t *)(item_ptr->lam_list_next)) {
        if (item_ptr == (lam_list_item_t *) item) {
            found = true;
            break;
        }
    }
    if (!found) {
        fprintf(stderr," Warning :: lam_list_remove_item - the item %p is not on the list %p \n",(void*) item, (void*) list);
        fflush(stderr);
        return (lam_list_item_t *)NULL;
    }
#endif

    /* reset next pointer of previous element */
    item->lam_list_prev->lam_list_next=item->lam_list_next;

    /* reset previous pointer of next element */
    item->lam_list_next->lam_list_prev=item->lam_list_prev;

    list->lam_list_length--;
    return (lam_list_item_t *)item->lam_list_prev;
}

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  /* 
   * Adds item to the end of the list but does not retain item. 
   */
                                                                                                      
    static inline void lam_list_append(lam_list_t *list, lam_list_item_t *item)
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


  /* Adds item to list at index and retains item. 
     Returns 1 if successful, 0 otherwise.
     0 <= idx < length_m
     Example: if idx = 2 and list = item1->item2->item3->item4, then
     after insert, list = item1->item2->item->item3->item4
  */
   int lam_list_insert(lam_list_t *list, lam_list_item_t *item, long long idx);


  /* 
   * Adds item to the front of the list and retains item. 
   */
    static inline void lam_list_prepend(lam_list_t *list, lam_list_item_t *item) 
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
   


  /*   
   *  Removes and returns first item on list.
   */
    static inline lam_list_item_t *lam_list_remove_first(lam_list_t *list)
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
                                                                                                      
#if LAM_ENABLE_DEBUG
        /* debug code */
        item->lam_list_prev=(lam_list_item_t *)NULL;
        item->lam_list_next=(lam_list_item_t *)NULL;
#endif
        return (lam_list_item_t *) item;
    }

  /*   
   *  Removes and returns last item on list.
   */
   static inline lam_list_item_t *lam_list_remove_last(lam_list_t *list)
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

#if LAM_ENABLE_DEBUG
        /* debug code */
        item->lam_list_next = item->lam_list_prev = (lam_list_item_t *)NULL;
#endif
        return (lam_list_item_t *) item;
    }

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* LAM_LIST_H */
