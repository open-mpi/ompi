
/*
 * $HEADER$
 */

/**
 * Utility functions to manage fortran <-> c opaque object translation
 */

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "include/constants.h"
#include "lfc/lam_pointer_array.h"
#include "util/output.h"

static void lam_pointer_array_construct(lam_pointer_array_t *);
static void lam_pointer_array_destruct(lam_pointer_array_t *);
enum { TABLE_INIT = 1, TABLE_GROW = 2 };

lam_class_t lam_pointer_array_t_class = {
    "lam_pointer_array_t",
    OBJ_CLASS(lam_object_t),
    (lam_construct_t) lam_pointer_array_construct,
    (lam_destruct_t) lam_pointer_array_destruct
};

/**
 * lam_pointer_array constructor
 */
void lam_pointer_array_construct(lam_pointer_array_t *array)
{
    OBJ_CONSTRUCT(&array->lock, lam_mutex_t);
    array->lowest_free = 0;
    array->number_free = 0;
    array->size = 0;
    array->addr = 0;
}

/**
 * lam_pointer_array destructor
 */
void lam_pointer_array_destruct(lam_pointer_array_t *array){

    /* free table */
    if( NULL != array->addr)
        free(array->addr);

    OBJ_DESTRUCT(&array->lock);
    return;
}

/**
 * add a pointer to dynamic pointer table
 *
 * @param table Pointer to lam_pointer_array_t object (IN)
 * @param ptr Pointer to be added to table    (IN)
 *
 * @return Array index where ptr is inserted
 */
size_t lam_pointer_array_add(lam_pointer_array_t *table, void *ptr)
{
    void **p;
    int	i;
    size_t index;

    if (0) {
        lam_output(0,"lam_pointer_array_add:  IN:  "
                " table %p (size %ld, lowest free %ld, number free %ld)"
                " ptr = %p\n",
                table, table->size, table->lowest_free, table->number_free,
                ptr);
    }

    assert(table != NULL);

    THREAD_LOCK(&(table->lock));

    if (table->addr == NULL) {

	/*
	 * first time
	 */

        if (0) {
	    lam_output(0,"lam_pointer_array_add:  INIT: table %p\n", table);
        }

	p = malloc(TABLE_INIT * sizeof(void *));
	if (p == NULL) {
        THREAD_UNLOCK(&(table->lock));
        return -1;
	}
	table->lowest_free = 0;
	table->number_free = TABLE_INIT;
	table->size = TABLE_INIT;
	table->addr = p;
        for (i = 0; i < table->size; i++) {
            table->addr[i] = NULL;
        }

    } else if (table->number_free == 0) {

        /*
         * grow table
         */

        if (0) {
            lam_output(0,"lam_pointer_array_add:  GROW: table %p growing %d -> %d\n",
                    table, table->size, table->size * TABLE_GROW);
        }

	p = realloc(table->addr, TABLE_GROW * table->size * sizeof(void *));
	if (p == NULL) {
        THREAD_UNLOCK(&(table->lock));
	    return -1;
	}
	table->lowest_free = table->size;
	table->number_free = (TABLE_GROW - 1) * table->size;
	table->size *= TABLE_GROW;
	table->addr = p;
        for (i = table->lowest_free; i < table->size; i++) {
            table->addr[i] = NULL;
        }
    }

    assert(table->addr != NULL);
    assert(table->size > 0);
    assert(table->lowest_free >= 0);
    assert(table->lowest_free < table->size);
    assert(table->number_free > 0);
    assert(table->number_free <= table->size);

    /*
     * add pointer to table, and return the index
     */

    index = table->lowest_free;
    assert(table->addr[index] == NULL);
    table->addr[index] = ptr;
    table->number_free--;
    if (table->number_free > 0) {
        for (i = table->lowest_free + 1; i < table->size; i++) {
            if (table->addr[i] == NULL) {
                table->lowest_free = i;
                break;
            }
        }
    }
    else {
        table->lowest_free = table->size;
    }

    if (0) {
        lam_output(0,"lam_pointer_array_add:  OUT: "
                " table %p (size %ld, lowest free %ld, number free %ld)"
                " addr[%d] = %p\n",
                table, table->size, table->lowest_free, table->number_free,
                index, ptr);
    }

    THREAD_UNLOCK(&(table->lock));

    return index;
}

/**
 * free a slot in dynamic pointer table for reuse
 *
 *
 * @param table Pointer to lam_pointer_array_t object (IN)
 * @param ptr Pointer to be added to table    (IN)
 *
 * @return Error code
 *
 * Assumption: NULL element is free element.
 */
int lam_pointer_array_set_item(lam_pointer_array_t *table, size_t index,
        void * value)
{
    int return_value;
    assert(table != NULL);

#if 0
    lam_output(0,"lam_pointer_array_set_item: IN:  "
                " table %p (size %ld, lowest free %ld, number free %ld)"
                " addr[%d] = %p\n",
                table, table->size, table->lowest_free, table->number_free,
                index, table->addr[index]);
#endif

    /* expand table if required to set a specific index */
    THREAD_LOCK(&(table->lock));
    if(table->size <= index) {
        size_t i, new_size = (((index / TABLE_GROW) + 1) * TABLE_GROW);
	void *p = realloc(table->addr, new_size * sizeof(void *));
	if (p == NULL) {
            THREAD_UNLOCK(&(table->lock));
	    return -1;
	}
	table->number_free += new_size - table->size;
	table->addr = p;
        for (i = table->size; i < new_size; i++) {
            table->addr[i] = NULL;
        }
	table->size = new_size;
    }

    /* 
     * allow a specific index to be changed.
     */
    table->addr[index] = value;
    /* mark element as free, if NULL element */
    if( NULL == value ) {
        if (index < table->lowest_free) {
            table->lowest_free = index;
        }
        table->number_free++;
    }

#if 0
    lam_output(0,"lam_pointer_array_set_item: OUT: "
                " table %p (size %ld, lowest free %ld, number free %ld)"
                " addr[%d] = %p\n",
                table, table->size, table->lowest_free, table->number_free,
                index, table->addr[index]);
#endif

    THREAD_UNLOCK(&(table->lock));
    return LAM_SUCCESS;
}

/**
 * lookup pointer by index in pointer table
 *
 * @param table Pointer to lam_pointer_array_t object (IN)
 * @param ptr Pointer to be added to table    (IN)
 *
 * @return Pointer
 */
void *lam_pointer_array_get_item(lam_pointer_array_t *table, size_t index)
{
    void *p;

#if 0
    lam_output(0,"lam_pointer_array_get_item: IN: "
                " table %p (size %ld, lowest free %ld, number free %ld)"
                " addr[%d] = %p\n",
                table, table->size, table->lowest_free, table->number_free,
                index, table->addr[index]);
#endif

    THREAD_LOCK(&(table->lock));

    assert(table != NULL);
    assert(table->addr != NULL);
    assert(index >= 0);
    assert(index < table->size);

    p = table->addr[index];

    THREAD_UNLOCK(&(table->lock));

#if 0
    lam_output(0,"lam_pointer_array_get_item: OUT:"
                " table %p (size %ld, lowest free %ld, number free %ld)"
                " addr[%d] = %p\n",
                table, table->size, table->lowest_free, table->number_free,
                index, table->addr[index]);
#endif
    return p;
}


