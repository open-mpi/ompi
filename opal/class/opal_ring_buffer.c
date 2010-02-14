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
 * Copyright (c) 2010      Cisco Systems, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "opal/constants.h"
#include "opal/class/opal_ring_buffer.h"
#include "opal/util/output.h"

static void opal_ring_buffer_construct(opal_ring_buffer_t *);
static void opal_ring_buffer_destruct(opal_ring_buffer_t *);

OBJ_CLASS_INSTANCE(opal_ring_buffer_t, opal_object_t,
                   opal_ring_buffer_construct,
                   opal_ring_buffer_destruct);

/*
 * opal_ring_buffer constructor
 */
static void opal_ring_buffer_construct(opal_ring_buffer_t *ring)
{
    OBJ_CONSTRUCT(&ring->lock, opal_mutex_t);
    ring->head = NULL;
    ring->tail = NULL;
    ring->size = 0;
    ring->addr = NULL;
}

/*
 * opal_ring_buffer destructor
 */
static void opal_ring_buffer_destruct(opal_ring_buffer_t *ring)
{
    if( NULL != ring->addr) {
        free(ring->addr);
        ring->addr = NULL;
    }

    ring->size = 0;

    OBJ_DESTRUCT(&ring->lock);
}

/**
 * initialize a ring object
 */
int opal_ring_buffer_init(opal_ring_buffer_t* ring, int size)
{
    /* check for errors */
    if (NULL == ring) {
        return OPAL_ERR_BAD_PARAM;
    }
    
    /* Allocate and set the ring to NULL */   
    ring->addr = (char **)calloc(size * sizeof(char*), 1);
    if (NULL == ring->addr) { /* out of memory */
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    ring->size = size;
    /* point the head to the first location */
    ring->head = &ring->addr[0];

    return OPAL_SUCCESS;
}
