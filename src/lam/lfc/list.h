/*
 * $HEADER$
 */

#ifndef LAM_LIST_H
#define LAM_LIST_H

#include "lam/lfc/object.h"

/*
 *
 *      Available Classes
 *
 */

extern lam_class_info_t   lam_list_item_cls;
extern lam_class_info_t   lam_list_cls;
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
    struct lam_list_item   *lam_list_next;
    struct lam_list_item   *lam_list_prev;
} lam_list_item_t;

void lam_list_item_init(lam_list_item_t *item);
void lam_list_item_destroy(lam_list_item_t *item);

#define lam_list_get_next(item) \
    ((item) ? (((lam_list_item_t*)(item))->lam_list_next) : 0)

#define lam_list_get_prev(item) \
    ((item) ? (((lam_list_item_t*)(item))->lam_list_prev) : 0)

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
    lam_list_type_t      lam_list_type;
    volatile size_t     lam_list_length;
} lam_list_t;


void lam_list_init(lam_list_t *list);
void lam_list_destroy(lam_list_t *list);

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
    return 0;
}

/* 
 * Returns last item on list, but does not remove it from the list. 
 */
static inline lam_list_item_t* lam_list_get_last(lam_list_t* list)
{
    return 0;
}

/* 
 * Returns end of list, an invalid list entry.
 */
static inline lam_list_item_t* lam_list_get_end(lam_list_t* list)
{
    return 0;
}


/* 
 * Adds item to the end of the list but does not retain item. 
 */
void lam_list_append(lam_list_t *list, lam_list_item_t *item);


/*
 * Remove item from the list.
 */
lam_list_item_t* lam_list_remove(lam_list_t *list, lam_list_item_t *item);

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

void lam_list_prepend(lam_list_t *list, lam_list_item_t *item);


/*   
 *  Removes and returns first item on list.
 */
lam_list_item_t *lam_list_remove_first(lam_list_t *list);


/*   
 *  Removes and returns last item on list.
 */
lam_list_item_t *lam_list_remove_last(lam_list_t *list);

#endif /* LAM_LIST_H */
