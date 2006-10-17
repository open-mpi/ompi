/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "ompi/constants.h"
#include "ompi/class/ompi_pointer_array.h"
#include "opal/util/output.h"

enum { TABLE_INIT = 1, TABLE_GROW = 2 };

static void ompi_pointer_array_construct(ompi_pointer_array_t *);
static void ompi_pointer_array_destruct(ompi_pointer_array_t *);
static bool grow_table(ompi_pointer_array_t *table, size_t soft, size_t hard);

OBJ_CLASS_INSTANCE(ompi_pointer_array_t, opal_object_t,
                   ompi_pointer_array_construct,
                   ompi_pointer_array_destruct);

/*
 * ompi_pointer_array constructor
 */
void ompi_pointer_array_construct(ompi_pointer_array_t *array)
{
    OBJ_CONSTRUCT(&array->lock, opal_mutex_t);
    array->lowest_free = 0;
    array->number_free = 0;
    array->size = 0;
    array->addr = 0;
}

/*
 * ompi_pointer_array destructor
 */
void ompi_pointer_array_destruct(ompi_pointer_array_t *array)
{
    /* free table */
    if( NULL != array->addr) {
        free(array->addr);
    }

    OBJ_DESTRUCT(&array->lock);
}

/**
 * add a pointer to dynamic pointer table
 *
 * @param table Pointer to ompi_pointer_array_t object (IN)
 * @param ptr Pointer to be added to table    (IN)
 *
 * @return Array index where ptr is inserted or OMPI_ERROR if it fails
 */
int ompi_pointer_array_add(ompi_pointer_array_t *table, void *ptr)
{
    int i;
    int index;

    OPAL_THREAD_LOCK(&(table->lock));

    if (table->number_free == 0) {
        /* need to grow table */
        if (!grow_table(table, 
						(NULL == table->addr ? TABLE_INIT : table->size * TABLE_GROW), 
                        OMPI_FORTRAN_HANDLE_MAX)) {
            OPAL_THREAD_UNLOCK(&(table->lock));
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
    }

    assert( (table->addr != NULL) && (table->size > 0) );
    assert( (table->lowest_free >= 0) && (table->lowest_free < table->size) );
    assert( (table->number_free > 0) && (table->number_free <= table->size) );

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

    OPAL_THREAD_UNLOCK(&(table->lock));
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
int ompi_pointer_array_set_item(ompi_pointer_array_t *table, int index,
                                void * value)
{
    assert(table != NULL);

    /* expand table if required to set a specific index */

    OPAL_THREAD_LOCK(&(table->lock));
    if (table->size <= index) {
        if (!grow_table(table, ((index / TABLE_GROW) + 1) * TABLE_GROW,
                        index)) {
            OPAL_THREAD_UNLOCK(&(table->lock));
			return OMPI_ERROR;
        }
    }

    /* 
     * allow a specific index to be changed.
     */
    
    if ( NULL == table->addr[index] ) {
        table->addr[index] = value;
		/* mark element as free, if NULL element */
		if( NULL == value ) {
			if (index < table->lowest_free) {
				table->lowest_free = index;
			}
		}
		else {
			table->number_free--;
			/* Reset lowest_free if required */
			if ( index == table->lowest_free ) {
				int i;
            
				table->lowest_free=table->size;
				for ( i=index; i<table->size; i++) {
					if ( NULL == table->addr[i] ){
						table->lowest_free = i;
						break;
					}                    
				}
			}
		}
    }
    else {
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
				int i;
            
				table->lowest_free=table->size;
				for ( i=index; i<table->size; i++) {
					if ( NULL == table->addr[i] ){
						table->lowest_free = i;
						break;
					}                    
				}
			}
		}
    }
	

#if 0
    opal_output(0,"ompi_pointer_array_set_item: OUT: "
                " table %p (size %ld, lowest free %ld, number free %ld)"
                " addr[%d] = %p\n",
                table, table->size, table->lowest_free, table->number_free,
                index, table->addr[index]);
#endif

    OPAL_THREAD_UNLOCK(&(table->lock));
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
bool ompi_pointer_array_test_and_set_item (ompi_pointer_array_t *table, 
                                           int index, void *value)
{
    assert(table != NULL);
    assert(index >= 0);

#if 0
    opal_output(0,"ompi_pointer_array_test_and_set_item: IN:  "
               " table %p (size %ld, lowest free %ld, number free %ld)"
               " addr[%d] = %p\n",
               table, table->size, table->lowest_free, table->number_free,
               index, table->addr[index]);
#endif

    /* expand table if required to set a specific index */
    OPAL_THREAD_LOCK(&(table->lock));
    if ( index < table->size && table->addr[index] != NULL ) {
        /* This element is already in use */
        OPAL_THREAD_UNLOCK(&(table->lock));
        return false;
    }

    /* Do we need to grow the table? */

    if (table->size <= index) {
        if (!grow_table(table, (((index / TABLE_GROW) + 1) * TABLE_GROW),
                        index)) {
            OPAL_THREAD_UNLOCK(&(table->lock));
            return false;
        }
    }

    /* 
     * allow a specific index to be changed.
     */
    table->addr[index] = value;
    table->number_free--;
    /* Reset lowest_free if required */
    if ( index == table->lowest_free ) {
        int i;

	table->lowest_free = table->size;
        for ( i=index; i<table->size; i++) {
            if ( NULL == table->addr[i] ){
                table->lowest_free = i;
                break;
            }                    
        }
    }

#if 0
    opal_output(0,"ompi_pointer_array_test_and_set_item: OUT: "
               " table %p (size %ld, lowest free %ld, number free %ld)"
               " addr[%d] = %p\n",
               table, table->size, table->lowest_free, table->number_free,
               index, table->addr[index]);
#endif

    OPAL_THREAD_UNLOCK(&(table->lock));
    return true;
}


static bool grow_table(ompi_pointer_array_t *table, size_t soft, size_t hard)
{
    size_t new_size;
    int i, new_size_int;
    void *p;

    /* Ensure that we have room to grow -- stay less than
       OMPI_FORTRAN_HANDLE_MAX.  Note that OMPI_FORTRAN_HANDLE_MAX
       is min(INT_MAX, fortran INTEGER max), so it's guaranteed to
       fit within a [signed] int. */
    
    if (table->size >= OMPI_FORTRAN_HANDLE_MAX) {
        return false;
    }
    if (soft > OMPI_FORTRAN_HANDLE_MAX) {
        if (hard > OMPI_FORTRAN_HANDLE_MAX) {
            return false;
        } else {
            new_size = hard;
        }
    } else {
        new_size = soft;
    }
    
    p = (void **) realloc(table->addr, new_size * sizeof(void *));
    if (p == NULL) {
        return false;
    }
    
    /* We've already established (above) that the arithimetic
       below will be less than OMPI_FORTRAN_HANDLE_MAX */
    
    new_size_int = (int) new_size;
    table->number_free += new_size_int - table->size;
    table->addr = (void**)p;
    for (i = table->size; i < new_size_int; ++i) {
        table->addr[i] = NULL;
    }
    table->size = new_size_int;

    return true;
}
