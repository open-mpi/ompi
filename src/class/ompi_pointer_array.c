
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
#include "class/ompi_pointer_array.h"
#include "util/output.h"

static void ompi_pointer_array_construct(ompi_pointer_array_t *);
static void ompi_pointer_array_destruct(ompi_pointer_array_t *);
enum { TABLE_INIT = 1, TABLE_GROW = 2 };

ompi_class_t ompi_pointer_array_t_class = {
    "ompi_pointer_array_t",
    OBJ_CLASS(ompi_object_t),
    (ompi_construct_t) ompi_pointer_array_construct,
    (ompi_destruct_t) ompi_pointer_array_destruct
};

/**
 * ompi_pointer_array constructor
 */
void ompi_pointer_array_construct(ompi_pointer_array_t *array)
{
    OBJ_CONSTRUCT(&array->lock, ompi_mutex_t);
    array->lowest_free = 0;
    array->number_free = 0;
    array->size = 0;
    array->addr = 0;
}

/**
 * ompi_pointer_array destructor
 */
void ompi_pointer_array_destruct(ompi_pointer_array_t *array){

    /* free table */
    if( NULL != array->addr)
        free(array->addr);

    OBJ_DESTRUCT(&array->lock);
    return;
}

/**
 * add a pointer to dynamic pointer table
 *
 * @param table Pointer to ompi_pointer_array_t object (IN)
 * @param ptr Pointer to be added to table    (IN)
 *
 * @return Array index where ptr is inserted or OMPI_ERROR if it fails
 */
size_t ompi_pointer_array_add(ompi_pointer_array_t *table, void *ptr)
{
    void **p;
    int	i;
    size_t index;

    if (0) {
        ompi_output(0,"ompi_pointer_array_add:  IN:  "
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
	    ompi_output(0,"ompi_pointer_array_add:  INIT: table %p\n", table);
        }

	p = malloc(TABLE_INIT * sizeof(void *));
	if (p == NULL) {
        THREAD_UNLOCK(&(table->lock));
        return OMPI_ERROR;
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
            ompi_output(0,"ompi_pointer_array_add:  GROW: table %p growing %d -> %d\n",
                    table, table->size, table->size * TABLE_GROW);
        }

	p = realloc(table->addr, TABLE_GROW * table->size * sizeof(void *));
	if (p == NULL) {
        THREAD_UNLOCK(&(table->lock));
	    return OMPI_ERROR;
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
        ompi_output(0,"ompi_pointer_array_add:  OUT: "
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
 * @param table Pointer to ompi_pointer_array_t object (IN)
 * @param ptr Pointer to be added to table    (IN)
 *
 * @return Error code
 *
 * Assumption: NULL element is free element.
 */
int ompi_pointer_array_set_item(ompi_pointer_array_t *table, size_t index,
        void * value)
{
    assert(table != NULL);

#if 0
    ompi_output(0,"ompi_pointer_array_set_item: IN:  "
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
	    return OMPI_ERROR;
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
    else {
        /* Reset lowest_free if required */
        if ( index == table->lowest_free ) {
            size_t i;
            
            for ( i=table->lowest_free; i<table->size; i++) {
                if ( NULL == table->addr[i] ){
                    table->lowest_free = i;
                    break;
                }                    
            }
        }
    }

#if 0
    ompi_output(0,"ompi_pointer_array_set_item: OUT: "
                " table %p (size %ld, lowest free %ld, number free %ld)"
                " addr[%d] = %p\n",
                table, table->size, table->lowest_free, table->number_free,
                index, table->addr[index]);
#endif

    THREAD_UNLOCK(&(table->lock));
    return OMPI_SUCCESS;
}

/**
 * Test whether a certain element is already in use. If not yet
 * in use, reserve it.
 *
 * @param array Pointer to array (IN)
 * @param index Index of element to be tested (IN)
 * @param value New value to be set at element index (IN)
 *
 * @return true/false True if element could be reserved
 *                    False if element could not be reserved (e.g.in use).
 *
 * In contrary to array_set, this function does not allow to overwrite 
 * a value, unless the previous value is NULL ( equiv. to free ).
 */
int ompi_pointer_array_test_and_set_item (ompi_pointer_array_t *table, size_t index,
                                         void *value)
{
    int flag=true;

    assert(table != NULL);
    assert(index >= 0);

#if 0
    ompi_output(0,"ompi_pointer_array_test_and_set_item: IN:  "
               " table %p (size %ld, lowest free %ld, number free %ld)"
               " addr[%d] = %p\n",
               table, table->size, table->lowest_free, table->number_free,
               index, table->addr[index]);
#endif

    /* expand table if required to set a specific index */
    THREAD_LOCK(&(table->lock));
    if ( index < table->size && table->addr[index] != NULL ) {
        /* This element is already in use */
        flag = false;
        THREAD_UNLOCK(&(table->lock));
        return flag;
    }

    if(table->size <= index) {
        size_t i, new_size = (((index / TABLE_GROW) + 1) * TABLE_GROW);
	void *p = realloc(table->addr, new_size * sizeof(void *));
	if (p == NULL) {
            THREAD_UNLOCK(&(table->lock));
	    return OMPI_ERROR;
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

    /* Reset lowest_free if required */
    if ( index == table->lowest_free ) {
        size_t i;

        for ( i=table->lowest_free; i<table->size; i++) {
            if ( NULL == table->addr[i] ){
                table->lowest_free = i;
                break;
            }                    
        }
    }

#if 0
    ompi_output(0,"ompi_pointer_array_test_and_set_item: OUT: "
               " table %p (size %ld, lowest free %ld, number free %ld)"
               " addr[%d] = %p\n",
               table, table->size, table->lowest_free, table->number_free,
               index, table->addr[index]);
#endif

    THREAD_UNLOCK(&(table->lock));
    return flag;
}
