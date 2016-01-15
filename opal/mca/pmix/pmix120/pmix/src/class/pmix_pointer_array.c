/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2013-2015 Intel, Inc. All rights reserved
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <private/autogen/config.h>

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#if HAVE_STDBOOL_H
#include <stdbool.h>
#endif
#include <limits.h>

#include "src/class/pmix_pointer_array.h"
#include "src/util/output.h"
#include <pmix.h>

enum { TABLE_INIT = 1, TABLE_GROW = 2 };

static void pmix_pointer_array_construct(pmix_pointer_array_t *);
static void pmix_pointer_array_destruct(pmix_pointer_array_t *);
static bool grow_table(pmix_pointer_array_t *table, int soft, int hard);

PMIX_CLASS_INSTANCE(pmix_pointer_array_t, pmix_object_t,
                   pmix_pointer_array_construct,
                   pmix_pointer_array_destruct);

/*
 * pmix_pointer_array constructor
 */
static void pmix_pointer_array_construct(pmix_pointer_array_t *array)
{
    array->lowest_free = 0;
    array->number_free = 0;
    array->size = 0;
    array->max_size = INT_MAX;
    array->block_size = 0;
    array->addr = 0;
}

/*
 * pmix_pointer_array destructor
 */
static void pmix_pointer_array_destruct(pmix_pointer_array_t *array)
{
    /* free table */
    if( NULL != array->addr) {
        free(array->addr);
        array->addr = NULL;
    }

    array->size = 0;
}

/**
 * initialize an array object
 */
pmix_status_t pmix_pointer_array_init(pmix_pointer_array_t* array,
                                      int initial_allocation,
                                      int max_size, int block_size)
{
    size_t num_bytes;

    /* check for errors */
    if (NULL == array || max_size < block_size) {
        return PMIX_ERR_BAD_PARAM;
    }

    array->max_size = max_size;
    array->block_size = block_size;

    num_bytes = (0 < initial_allocation ? initial_allocation : block_size);
    array->number_free = num_bytes;
    array->size = num_bytes;
    num_bytes *= sizeof(void*);

    /* Allocate and set the array to NULL */
    array->addr = (void **)calloc(num_bytes, 1);
    if (NULL == array->addr) { /* out of memory */
        return PMIX_ERR_OUT_OF_RESOURCE;
    }

    return PMIX_SUCCESS;
}

/**
 * add a pointer to dynamic pointer table
 *
 * @param table Pointer to pmix_pointer_array_t object (IN)
 * @param ptr Pointer to be added to table    (IN)
 *
 * @return Array index where ptr is inserted or PMIX_ERROR if it fails
 */
int pmix_pointer_array_add(pmix_pointer_array_t *table, void *ptr)
{
    int i, index;

    if (table->number_free == 0) {
        /* need to grow table */
        if (!grow_table(table,
                        (NULL == table->addr ? TABLE_INIT : table->size * TABLE_GROW),
                        INT_MAX)) {
            return PMIX_ERR_OUT_OF_RESOURCE;
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

    return index;
}

/**
 * Set the value of the dynamic array at a specified location.
 *
 *
 * @param table Pointer to pmix_pointer_array_t object (IN)
 * @param ptr Pointer to be added to table    (IN)
 *
 * @return Error code
 *
 * Assumption: NULL element is free element.
 */
pmix_status_t pmix_pointer_array_set_item(pmix_pointer_array_t *table, int index,
                                          void * value)
{
    assert(table != NULL);

    /* expand table if required to set a specific index */

    if (table->size <= index) {
        if (!grow_table(table, ((index / TABLE_GROW) + 1) * TABLE_GROW,
                        index)) {
            return PMIX_ERROR;
        }
    }

    /* mark element as free, if NULL element */
    if( NULL == value ) {
        if (index < table->lowest_free) {
            table->lowest_free = index;
        }
        if( NULL != table->addr[index] ) {
            table->number_free++;
        }
    } else {
        if (NULL == table->addr[index]) {
            table->number_free--;
        }
        /* Reset lowest_free if required */
        if ( index == table->lowest_free ) {
            int i;

            table->lowest_free = table->size;
            for ( i=index + 1; i<table->size; i++) {
                if ( NULL == table->addr[i] ){
                    table->lowest_free = i;
                    break;
                }
            }
        }
    }
    table->addr[index] = value;

#if 0
    pmix_output(0,"pmix_pointer_array_set_item: OUT: "
                " table %p (size %ld, lowest free %ld, number free %ld)"
                " addr[%d] = %p\n",
                table, table->size, table->lowest_free, table->number_free,
                index, table->addr[index]);
#endif

    return PMIX_SUCCESS;
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
bool pmix_pointer_array_test_and_set_item (pmix_pointer_array_t *table,
                                           int index, void *value)
{
    assert(table != NULL);
    assert(index >= 0);

#if 0
    pmix_output(0,"pmix_pointer_array_test_and_set_item: IN:  "
               " table %p (size %ld, lowest free %ld, number free %ld)"
               " addr[%d] = %p\n",
               table, table->size, table->lowest_free, table->number_free,
               index, table->addr[index]);
#endif

    /* expand table if required to set a specific index */
    if ( index < table->size && table->addr[index] != NULL ) {
        /* This element is already in use */
        return false;
    }

    /* Do we need to grow the table? */

    if (table->size <= index) {
        if (!grow_table(table, (((index / TABLE_GROW) + 1) * TABLE_GROW),
                        index)) {
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
    pmix_output(0,"pmix_pointer_array_test_and_set_item: OUT: "
               " table %p (size %ld, lowest free %ld, number free %ld)"
               " addr[%d] = %p\n",
               table, table->size, table->lowest_free, table->number_free,
               index, table->addr[index]);
#endif

    return true;
}

pmix_status_t pmix_pointer_array_set_size(pmix_pointer_array_t *array, int new_size)
{
    if(new_size > array->size) {
        if (!grow_table(array, new_size, new_size)) {
            return PMIX_ERROR;
        }
    }
    return PMIX_SUCCESS;
}

static bool grow_table(pmix_pointer_array_t *table, int soft, int hard)
{
    int new_size;
    int i, new_size_int;
    void *p;

    /* new_size = ((table->size + num_needed + table->block_size - 1) /
       table->block_size) * table->block_size; */
    new_size = soft;
    if( soft > table->max_size ) {
        if( hard > table->max_size ) {
            return false;
        }
        new_size = hard;
    }
    if( new_size >= table->max_size ) {
        return false;
    }

    p = (void **) realloc(table->addr, new_size * sizeof(void *));
    if (p == NULL) {
        return false;
    }

    new_size_int = (int) new_size;
    table->number_free += new_size_int - table->size;
    table->addr = (void**)p;
    for (i = table->size; i < new_size_int; ++i) {
        table->addr[i] = NULL;
    }
    table->size = new_size_int;

    return true;
}
