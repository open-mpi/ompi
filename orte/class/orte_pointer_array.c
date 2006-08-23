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

#include "orte_config.h"

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "orte/orte_constants.h"
#include "orte/class/orte_pointer_array.h"
#include "opal/util/output.h"

static void orte_pointer_array_construct(orte_pointer_array_t *);
static void orte_pointer_array_destruct(orte_pointer_array_t *);
static bool grow_table(orte_pointer_array_t *table);

OBJ_CLASS_INSTANCE(orte_pointer_array_t, opal_object_t,
                   orte_pointer_array_construct,
                   orte_pointer_array_destruct);

/*
 * orte_pointer_array constructor
 */
void orte_pointer_array_construct(orte_pointer_array_t *array)
{
    OBJ_CONSTRUCT(&array->lock, opal_mutex_t);
    array->lowest_free = 0;
    array->number_free = 0;
    array->size = 0;
    array->max_size = 0;
    array->block_size = 0;
    array->addr = 0;
}

/*
 * orte_pointer_array destructor
 */
void orte_pointer_array_destruct(orte_pointer_array_t *array)
{
    /* free table */
    if( NULL != array->addr) {
        free(array->addr);
    }

    OBJ_DESTRUCT(&array->lock);
}

/**
 * initialize an array object
 */
int orte_pointer_array_init(orte_pointer_array_t **array,
                            orte_std_cntr_t initial_allocation,
                            orte_std_cntr_t max_size, orte_std_cntr_t block_size)
{
    orte_std_cntr_t num_bytes;
    
    /* check for errors */
    if (NULL == array || max_size < block_size) {
       return ORTE_ERR_BAD_PARAM;
    }
    
   *array = OBJ_NEW(orte_pointer_array_t);
   if (NULL == *array) {
       return ORTE_ERR_OUT_OF_RESOURCE;
   }
   
   (*array)->max_size = max_size;
   (*array)->block_size = block_size;
   
   if (0 < initial_allocation) {
       num_bytes = initial_allocation * sizeof(void*);
       (*array)->number_free = initial_allocation;
       (*array)->size = initial_allocation;
   } else {
        num_bytes = block_size * sizeof(void*);
        (*array)->number_free = block_size;
        (*array)->size = block_size;
   }
   
   (*array)->addr = (void **)malloc(num_bytes);
   if (NULL == (*array)->addr) { /* out of memory */
        OBJ_RELEASE(*array);
        return ORTE_ERR_OUT_OF_RESOURCE;
   }

   /* init the array elements to NULL */
   memset((*array)->addr, 0, num_bytes);
   
   return ORTE_SUCCESS;
}

/**
 * add a pointer to dynamic pointer table
 *
 * @param table Pointer to orte_pointer_array_t object (IN)
 * @param ptr Pointer to be added to table    (IN)
 *
 * @param (OUT) Array index where ptr is inserted
 * @return ORTE_ERROR_code if it fails
 */
int orte_pointer_array_add(orte_std_cntr_t *location, orte_pointer_array_t *table, void *ptr)
{
    orte_std_cntr_t i, index;

    if (0) {
        opal_output(0,"orte_pointer_array_add:  IN:  "
                " table %p (size %ld, lowest free %ld, number free %ld)"
                " ptr = %p\n",
                table, table->size, table->lowest_free, table->number_free,
                ptr);
    }

    assert(table != NULL);

    OPAL_THREAD_LOCK(&(table->lock));

    if (table->number_free == 0) {

        /* need to grow table */

        if (!grow_table(table)) {
            OPAL_THREAD_UNLOCK(&(table->lock));
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
    }

    assert(table->addr != NULL);
    assert(table->size > 0);
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
        opal_output(0,"orte_pointer_array_add:  OUT: "
                " table %p (size %ld, lowest free %ld, number free %ld)"
                " addr[%d] = %p\n",
                table, table->size, table->lowest_free, table->number_free,
                index, ptr);
    }

    OPAL_THREAD_UNLOCK(&(table->lock));
    *location = index;
    return ORTE_SUCCESS;
}

/**
 * free a slot in dynamic pointer table for reuse
 *
 *
 * @param table Pointer to orte_pointer_array_t object (IN)
 * @param ptr Pointer to be added to table    (IN)
 *
 * @return Error code
 *
 * Assumption: NULL element is free element.
 */
int orte_pointer_array_set_item(orte_pointer_array_t *table, orte_std_cntr_t index,
                                void * value)
{
    assert(table != NULL);

#if 0
    opal_output(0,"orte_pointer_array_set_item: IN:  "
                " table %p (size %ld, lowest free %ld, number free %ld)"
                " addr[%d] = %p\n",
                table, table->size, table->lowest_free, table->number_free,
                index, table->addr[index]);
#endif

    /* expand table if required to set a specific index */

    OPAL_THREAD_LOCK(&(table->lock));
    if (table->size <= index) {
        if (!grow_table(table)) {
            OPAL_THREAD_UNLOCK(&(table->lock));
	        return ORTE_ERROR;
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
        		     orte_std_cntr_t i;
                    
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
            		orte_std_cntr_t i;
                        
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
    opal_output(0,"orte_pointer_array_set_item: OUT: "
                " table %p (size %ld, lowest free %ld, number free %ld)"
                " addr[%d] = %p\n",
                table, table->size, table->lowest_free, table->number_free,
                index, table->addr[index]);
#endif

    OPAL_THREAD_UNLOCK(&(table->lock));
    return ORTE_SUCCESS;
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
bool orte_pointer_array_test_and_set_item (orte_pointer_array_t *table, 
                                           orte_std_cntr_t index, void *value)
{
    assert(table != NULL);

#if 0
    opal_output(0,"orte_pointer_array_test_and_set_item: IN:  "
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
        if (!grow_table(table)) {
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
        orte_std_cntr_t i;

	    table->lowest_free = table->size;
        for ( i=index; i<table->size; i++) {
            if ( NULL == table->addr[i] ){
                table->lowest_free = i;
                break;
            }                    
        }
    }

#if 0
    opal_output(0,"orte_pointer_array_test_and_set_item: OUT: "
               " table %p (size %ld, lowest free %ld, number free %ld)"
               " addr[%d] = %p\n",
               table, table->size, table->lowest_free, table->number_free,
               index, table->addr[index]);
#endif

    OPAL_THREAD_UNLOCK(&(table->lock));
    return true;
}


int orte_pointer_array_set_size(orte_pointer_array_t *array, orte_std_cntr_t new_size)
{
    OPAL_THREAD_LOCK(&(array->lock));
    while (new_size > orte_pointer_array_get_size(array)) {
        if (!grow_table(array)) {
            OPAL_THREAD_UNLOCK(&(array->lock));
            return ORTE_ERROR;
        }
    }
    OPAL_THREAD_UNLOCK(&(array->lock));
    return ORTE_SUCCESS;
}


static bool grow_table(orte_pointer_array_t *table)
{
    orte_std_cntr_t new_size, i;
    void *p;

    /* Ensure that we have room to grow -- stay less than
     * specified maximum
     */
    
    if (table->size >= table->max_size) {
        return false;
    }
    
    if (table->block_size > (table->max_size - table->size)) { /* not enough space for a full block */
        new_size = table->max_size;
    } else {
        new_size = table->size + table->block_size;
    }

    p = (void **) realloc(table->addr, new_size * sizeof(void *));
    if (p == NULL) {
        return false;
    }
    
    /* Adjust structure counters and pointers */
    
    table->number_free += new_size - table->size;
    table->addr = (void**)p;
    for (i = table->size; i < new_size; ++i) {
        table->addr[i] = NULL;
    }
    table->size = new_size;

    return true;
}
