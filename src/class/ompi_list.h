/*
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
#include "class/ompi_object.h"

/**
 * \internal
 *
 * The class for the list container.
 */
OBJ_CLASS_DECLARATION(ompi_list_t);
/**
 * \internal
 *
 * Base class for items that are put in list (ompi_list_t) containers.
 */
OBJ_CLASS_DECLARATION(ompi_list_item_t);


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
 * Return the number of items in a list (O(1) operation)
 *
 * @param list The list container
 *
 * @returns The size of the list (size_t)
 *
 * This is an O(1) lookup to return the size of the list.  
 *
 * This is an inlined function in compilers that support inlining, so
 * it's usually a cheap operation.
 */
static inline size_t ompi_list_get_size(ompi_list_t* list)
{
    return list->ompi_list_length;
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


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
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
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif



#endif /* OMPI_LIST_H */
