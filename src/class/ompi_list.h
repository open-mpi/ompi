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
/**
 * @file 
 *
 * The ompi_list_t interface is used to provide a generic
 * doubly-linked list container for Open MPI.  It was inspired by (but
 * is slightly different than) the Stantard Template Library (STL)
 * std::list class.  One notable difference from std::list is that
 * when an ompi_list_t is destroyed, all of the ompi_list_item_t
 * objects that it contains are orphaned -- they are \em not
 * destroyed.
 *
 * The general idea is that ompi_list_item_t objects can be put on an
 * ompi_list_t.  Hence, you create a new type that derives from
 * ompi_list_item_t; this new type can then be used with ompi_list_t
 * containers.
 *
 * NOTE: ompi_list_item_t instances can only be on \em one list at a
 * time.  Specifically, if you add an ompi_list_item_t to one list,
 * and then add it to another list (without first removing it from the
 * first list), you will effectively be hosing the first list.  You
 * have been warned.
 */

#ifndef OMPI_LIST_H
#define OMPI_LIST_H

#include <stdio.h>
#include <stdlib.h>
#include "class/ompi_object.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
/**
 * \internal
 *
 * The class for the list container.
 */
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_list_t);
/**
 * \internal
 *
 * Base class for items that are put in list (ompi_list_t) containers.
 */
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_list_item_t);


/**
 * \internal
 * 
 * Struct of an ompi_list_item_t
 */
struct ompi_list_item_t
{
    ompi_object_t super;
    /**< Generic parent class for all Open MPI objects */
    volatile struct ompi_list_item_t *ompi_list_next;
    /**< Pointer to next list item */
    volatile struct ompi_list_item_t *ompi_list_prev;
    /**< Pointer to previous list item */
};
/**
 * Base type for items that are put in a list (ompi_list_t) containers.
 */
typedef struct ompi_list_item_t ompi_list_item_t;


/**
 * Get the next item in a list.
 *
 * @param item A list item.
 *
 * @returns The next item in the list
 */
#define ompi_list_get_next(item) \
    ((item) ? ((ompi_list_item_t*) ((ompi_list_item_t*)(item))->ompi_list_next) : NULL)

/**
 * Get the next item in a list.
 *
 * @param item A list item.
 *
 * @returns The next item in the list
 */
#define ompi_list_get_prev(item) \
    ((item) ? ((ompi_list_item_t*) ((ompi_list_item_t*)(item))->ompi_list_prev) : NULL)


/**
 * \internal
 *
 * Struct of an ompi_list_t
 */
struct ompi_list_t
{
    ompi_object_t       super;
    /**< Generic parent class for all Open MPI objects */
    ompi_list_item_t    ompi_list_head;
    /**< Head item of the list */
    ompi_list_item_t    ompi_list_tail;
    /**< Tail item of the list */
    volatile size_t     ompi_list_length;
    /**< Quick reference to the number of items in the list */
};
/**
 * List container type.
 */
typedef struct ompi_list_t ompi_list_t;


/**
 * Check for empty list
 *
 * @param list The list container
 *
 * @returns true if list's size is 0, false otherwise
 *
 * This is an O(1) operation.
 *
 * This is an inlined function in compilers that support inlining,
 * so it's usually a cheap operation.
 */
static inline bool ompi_list_is_empty(ompi_list_t* list)
{
    return (list->ompi_list_head.ompi_list_next == 
            &(list->ompi_list_tail));
}


/**
 * Return the first item on the list (does not remove it).
 *
 * @param list The list container
 *
 * @returns A pointer to the first item on the list
 *
 * This is an O(1) operation to return the first item on the list.  It
 * should be compared against the returned value from
 * ompi_list_get_end() to ensure that the list is not empty.
 *
 * This is an inlined function in compilers that support inlining, so
 * it's usually a cheap operation.
 */
static inline ompi_list_item_t* ompi_list_get_first(ompi_list_t* list)
{
    return (ompi_list_item_t *)list->ompi_list_head.ompi_list_next;
}

/**
 * Return the last item on the list (does not remove it).
 *
 * @param list The list container
 *
 * @returns A pointer to the last item on the list
 *
 * This is an O(1) operation to return the last item on the list.  It
 * should be compared against the returned value from
 * ompi_list_get_begin() to ensure that the list is not empty.
 *
 * This is an inlined function in compilers that support inlining, so
 * it's usually a cheap operation.
 */
static inline ompi_list_item_t* ompi_list_get_last(ompi_list_t* list)
{
    return (ompi_list_item_t *)list->ompi_list_tail.ompi_list_prev;
}

/**
 * Return the beginning of the list; an invalid list entry suitable
 * for comparison only.
 *
 * @param list The list container
 *
 * @returns A pointer to the beginning of the list.
 *
 * This is an O(1) operation to return the beginning of the list.
 * Similar to the STL, this is a special invalid list item -- it
 * should \em not be used for storage.  It is only suitable for
 * comparison to other items in the list to see if they are valid or
 * not; it's ususally used when iterating through the items in a list.
 *
 * This is an inlined function in compilers that support inlining, so
 * it's usually a cheap operation.
 */
static inline ompi_list_item_t* ompi_list_get_begin(ompi_list_t* list)
{
    return &(list->ompi_list_head);
}

/**
 * Return the end of the list; an invalid list entry suitable for
 * comparison only.
 *
 * @param list The list container
 *
 * @returns A pointer to the end of the list.
 *
 * This is an O(1) operation to return the end of the list.
 * Similar to the STL, this is a special invalid list item -- it
 * should \em not be used for storage.  It is only suitable for
 * comparison to other items in the list to see if they are valid or
 * not; it's ususally used when iterating through the items in a list.
 *
 * This is an inlined function in compilers that support inlining, so
 * it's usually a cheap operation.
 */
static inline ompi_list_item_t* ompi_list_get_end(ompi_list_t* list)
{
    return &(list->ompi_list_tail);
}


/**
 * Return the number of items in a list
 *
 * @param list The list container
 *
 * @returns The size of the list (size_t)
 *
 * This is an O(1) lookup to return the size of the list.  
 *
 * This is an inlined function in compilers that support inlining, so
 * it's usually a cheap operation.
 *
 * \warning The size of the list is cached as part of the list.  In
 * the future, calling \c ompi_list_splice or \c ompi_list_join may
 * result in this function recomputing the list size, which would be
 * an O(N) operation.  If \c ompi_list_splice or \c ompi_list_join is
 * never called on the specified list, this function will always be
 * O(1).
 */
static inline size_t ompi_list_get_size(ompi_list_t* list)
{
#if OMPI_ENABLE_DEBUG && 0
    /* not sure if we really want this running in devel, as it does
     * slow things down.  Wanted for development of splice / join to
     * make sure length was reset properly 
     */
    size_t check_len = 0;
    ompi_list_item_t *item;

    for (item = ompi_list_get_first(list) ;
         item != ompi_list_get_end(list) ;
         item = ompi_list_get_next(item)) {
        check_len++;
    }

    if (check_len != list->ompi_list_length) {
        fprintf(stderr," Error :: ompi_list_get_size - ompi_list_length does not match actual list length\n");
        fflush(stderr);
        abort();
    }
#endif

    return list->ompi_list_length;
}


/**
 * Remove an item from a list.
 *
 * @param list The list container
 * @param item The item to remove
 *
 * @returns A pointer to the item on the list previous to the one
 * that was removed.
 *
 * This is an O(1) operation to remove an item from the list.  The
 * forward / reverse pointers in the list are updated and the item is
 * removed.  The list item that is returned is now "owned" by the
 * caller -- they are responsible for OBJ_RELEASE()'ing it.
 *
 * If debugging is enabled (specifically, if --enable-debug was used
 * to configure Open MPI), this is an O(N) operation because it checks
 * to see if the item is actually in the list first.
 *
 * This is an inlined function in compilers that support inlining, so
 * it's usually a cheap operation.
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


/**
 * Append an item to the end of the list.
 *
 * @param list The list container
 * @param item The item to append
 *
 * This is an O(1) operation to append an item to the end of a list.
 * The ompi_list_item_t is not OBJ_RETAIN()'ed; it is assumed that
 * "ownership" of the item is passed from the caller to the list.
 *
 * This is an inlined function in compilers that support inlining, so
 * it's usually a cheap operation.
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


/**
 * Prepend an item to the beginning of the list.
 *
 * @param list The list container
 * @param item The item to prepend
 *
 * This is an O(1) operation to prepend an item to the beginning of a
 * list.  The ompi_list_item_t is not OBJ_RETAIN()'ed; it is assumed
 * that "ownership" of the item is passed from the caller to the list.
 *
 * This is an inlined function in compilers that support inlining, so
 * it's usually a cheap operation.
 */
static inline void ompi_list_prepend(ompi_list_t *list, 
                                     ompi_list_item_t *item) 
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


/**
 * Remove the first item from the list and return it.
 *
 * @param list The list container
 *
 * @returns The first item on the list.  If the list is empty,
 * NULL will be returned
 *
 * This is an O(1) operation to return the first item on the list.  If
 * the list is not empty, a pointer to the first item in the list will
 * be returned.  Ownership of the item is transferred from the list to
 * the caller; no calls to OBJ_RETAIN() or OBJ_RELEASE() are invoked.
 *
 * This is an inlined function in compilers that support inlining, so
 * it's usually a cheap operation.
 */
static inline ompi_list_item_t *ompi_list_remove_first(ompi_list_t *list)
{
  /*  Removes and returns first item on list.
      Caller now owns the item and should release the item
      when caller is done with it.
  */
  volatile ompi_list_item_t *item;
  if ( 0 == list->ompi_list_length ) {
    return (ompi_list_item_t *)NULL;
  }
  
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


/**
 * Remove the last item from the list and return it.
 *
 * @param list The list container
 *
 * @returns The last item on the list.  If the list is empty,
 * NULL will be returned
 *
 * This is an O(1) operation to return the last item on the list.  If
 * the list is not empty, a pointer to the last item in the list will
 * be returned.  Ownership of the item is transferred from the list to
 * the caller; no calls to OBJ_RETAIN() or OBJ_RELEASE() are invoked.
 *
 * This is an inlined function in compilers that support inlining, so
 * it's usually a cheap operation.
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

  /**
   * Add an item to the list before a given element
   *
   * @param list The list container
   * @param pos List element to insert \c item before
   * @param item The item to insert
   *
   * Inserts \c item before \c pos.  This is an O(1) operation.
   */
static inline void ompi_list_insert_pos(ompi_list_t *list, ompi_list_item_t *pos,
                                        ompi_list_item_t *item)
{
    /* point item at the existing elements */
    item->ompi_list_next = pos;
    item->ompi_list_prev = pos->ompi_list_prev;

    /* splice into the list */
    pos->ompi_list_prev->ompi_list_next = item;
    pos->ompi_list_prev = item;

    /* reset list length counter */
    list->ompi_list_length++;
}

  /**
   * Add an item to the list at a specific index location in the list.
   *
   * @param list The list container
   * @param item The item to insert
   * @param index Location to add the item
   *
   * @returns true if insertion succeeded; otherwise false
   *
   * This is potentially an O(N) operation to traverse down to the
   * correct location in the list and add an item.
   *
   * Example: if idx = 2 and list = item1->item2->item3->item4, then
   * after insert, list = item1->item2->item->item3->item4.
   *
   * If index is greater than the length of the list, no action is
   * performed and false is returned.
   */
  bool ompi_list_insert(ompi_list_t *list, ompi_list_item_t *item, 
                        long long idx);


    /**
     * Join a list into another list
     *
     * @param thislist List container for list being operated on
     * @param pos List item in \c thislist marking the position before
     *              which items are inserted
     * @param xlist List container for list being spliced from
     *
     * Join a list into another list.  All of the elements of \c xlist
     * are inserted before \c pos and removed from \c xlist.  
     *
     * This operation is an O(1) operation.  Both \c thislist and \c
     * xlist must be valid list containsers.  \c xlist will be empty
     * but valid after the call.  All pointers to \c ompi_list_item_t
     * containers remain valid, including those that point to elements
     * in \c xlist.
     */
    void ompi_list_join(ompi_list_t *thislist, ompi_list_item_t *pos, 
                        ompi_list_t *xlist);


    /**
     * Splice a list into another list
     *
     * @param thislist List container for list being operated on
     * @param pos List item in \c thislist marking the position before
     *             which items are inserted
     * @param xlist List container for list being spliced from
     * @param first List item in \c xlist marking the start of elements 
     *             to be copied into \c thislist
     * @param last List item in \c xlist marking the end of elements
     * to be copied into \c thislist
     *
     * Splice a subset of a list into another list.  The \c [first,
     * last) elements of \c xlist are moved into \c thislist,
     * inserting them before \c pos.  \c pos must be a valid iterator
     * in \c thislist and \c [first, last) must be a valid range in \c
     * xlist.  \c postition must not be in the range \c [first, last).
     * It is, however, valid for \c xlist and \c thislist to be the
     * same list.
     *
     * This is an O(N) operation because the length of both lists must
     * be recomputed.
     */
    void ompi_list_splice(ompi_list_t *thislist, ompi_list_item_t *pos,
                          ompi_list_t *xlist, ompi_list_item_t *first,
                          ompi_list_item_t *last);

    /**
     * Comparison function for ompi_list_sort(), below.
     *
     * @param a Pointer to a pointer to an ompi_list_item_t.
     * Explanation below.
     * @param b Pointer to a pointer to an ompi_list_item_t.
     * Explanation below.
     * @retval 1 if \em a is greater than \em b
     * @retval 0 if \em a is equal to \em b
     * @retval 11 if \em a is less than \em b
     *
     * This function is invoked by qsort(3) from within
     * ompi_list_sort().  It is important to understand what
     * ompi_list_sort() does before invoking qsort, so go read that
     * documentation first.
     *
     * The important thing to realize here is that a and b will be \em
     * double pointers to the items that you need to compare.  Here's
     * a sample compare function to illustrate this point:
     *
     * \verb
     * static int compare(ompi_list_item_t **a, ompi_list_item_t **b)
     * {
     *     orte_pls_base_cmp_t *aa = *((orte_pls_base_cmp_t **) a);
     *     orte_pls_base_cmp_t *bb = *((orte_pls_base_cmp_t **) b);
     *
     *     if (bb->priority > aa->priority) {
     *         return 1;
     *     } else if (bb->priority == aa->priority) {
     *         return 0;
     *     } else {
     *         return -1;
     *     }
     * }
     * \endverb
     */
    typedef int (*ompi_list_item_compare_fn_t)(ompi_list_item_t **a,
                                               ompi_list_item_t **b);

    /**
     * Sort a list with a provided compare function.
     *
     * @param list The list to sort
     * @param compare Compare function
     *
     * Put crassly, this function's complexity is O(N) + O(log(N)).
     * Its algorithm is:
     *
     * - remove every item from the list and put the corresponding
     *    (ompi_list_item_t*)'s in an array
     * - call qsort(3) with that array and your compare function
     * - re-add every element of the now-sorted array to the list
     *
     * The resulting list is now ordered.  Note, however, that since
     * an array of pointers is sorted, the comparison function must do
     * a double de-reference to get to the actual ompi_list_item_t (or
     * whatever the underlying type is).  See the documentation of
     * ompi_list_item_compare_fn_t for an example).
     */
    int ompi_list_sort(ompi_list_t* list, ompi_list_item_compare_fn_t compare);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif



#endif /* OMPI_LIST_H */
