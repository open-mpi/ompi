
/*
 * $HEADER$
 */

/**
 * Utility functions to manage fortran <-> c opaque object translation
 */

#include <stdlib.h>
#include <assert.h>

#include "lam/lfc/lam_pointer_array.h"
#include "lam/util/output.h"

/**
 * add a pointer to dynamic pointer table
 *
 * @param table Pointer to lam_pointer_array_t object (IN)
 * @param ptr Pointer to be added to table    (IN)
 *
 * @return Array index where ptr is inserted
 */
int lam_pointer_array_add(lam_pointer_array_t *table, void *ptr)
{
    void **p;
    int	index, i;
    enum { TABLE_INIT = 1, TABLE_GROW = 2 };

    if (LAM_ENABLE_DEBUG) {
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

        if (LAM_ENABLE_DEBUG) {
	    lam_output(0,"lam_pointer_array_add:  INIT: table %p\n", table);
        }

	p = malloc(TABLE_INIT * sizeof(void *));
	if (p == NULL) {
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

        if (LAM_ENABLE_DEBUG) {
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

    if (LAM_ENABLE_DEBUG) {
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
 */
int lam_pointer_array_set_item(lam_pointer_array_t *table, size_t index,
        void * value)
{
    assert(table != NULL);
    assert(table->addr != NULL);
    assert(index >= 0);
    assert(index < table->size);

    if (LAM_ENABLE_DEBUG) {
        lam_output(0,"lam_pointer_array_set_item: IN:  "
                " table %p (size %ld, lowest free %ld, number free %ld)"
                " addr[%d] = %p\n",
                table, table->size, table->lowest_free, table->number_free,
                index, table->addr[index]);
    }

    THREAD_LOCK(&(table->lock));

    table->addr[index] = value;
    if (index < table->lowest_free) {
        table->lowest_free = index;
    }
    table->number_free++;

    if (LAM_ENABLE_DEBUG) {
        lam_output(0,"lam_pointer_array_set_item: OUT: "
                " table %p (size %ld, lowest free %ld, number free %ld)"
                " addr[%d] = %p\n",
                table, table->size, table->lowest_free, table->number_free,
                index, table->addr[index]);
    }

    THREAD_UNLOCK(&(table->lock));

    return 0;
}

/**
 * lookup pointer by index in pointer table
 *
 * @param table Pointer to lam_pointer_array_t object (IN)
 * @param ptr Pointer to be added to table    (IN)
 *
 * @return Pointer
 */
void *lam_pointer_array_get_item(lam_pointer_array_t *table, int index)
{
    void *p;

    if (LAM_ENABLE_DEBUG) {
        lam_output(0,"lam_pointer_array_get_item: IN: "
                " table %p (size %ld, lowest free %ld, number free %ld)"
                " addr[%d] = %p\n",
                table, table->size, table->lowest_free, table->number_free,
                index, table->addr[index]);
    }

    THREAD_LOCK(&(table->lock));

    assert(table != NULL);
    assert(table->addr != NULL);
    assert(index >= 0);
    assert(index < table->size);

    p = table->addr[index];

    THREAD_UNLOCK(&(table->lock));

    if (LAM_ENABLE_DEBUG) {

        lam_output(0,"lam_pointer_array_get_item: OUT:"
                " table %p (size %ld, lowest free %ld, number free %ld)"
                " addr[%d] = %p\n",
                table, table->size, table->lowest_free, table->number_free,
                index, table->addr[index]);
    }

    return p;
}
