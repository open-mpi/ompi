/* -*- C -*-
 *
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
 * Copyright (c) 2007-2011 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc. All rights reserved.
 * Copyright (c) 2014-2015 Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 *
 * Buffer management types.
 */

#ifndef PMIX_BFROP_TYPES_H_
#define PMIX_BFROP_TYPES_H_

#include <private/autogen/config.h>
#include <pmix/rename.h>

#include "src/class/pmix_object.h"
#include "src/class/pmix_pointer_array.h"
#include "src/class/pmix_list.h"
#include <pmix/pmix_common.h>

BEGIN_C_DECLS

/* define the results values for comparisons so we can change them in only one place */
#define PMIX_VALUE1_GREATER  +1
#define PMIX_VALUE2_GREATER  -1
#define PMIX_EQUAL            0

/**
 * buffer type
 */
enum pmix_bfrop_buffer_type_t {
    PMIX_BFROP_BUFFER_NON_DESC   = 0x00,
    PMIX_BFROP_BUFFER_FULLY_DESC = 0x01
};

typedef enum pmix_bfrop_buffer_type_t pmix_bfrop_buffer_type_t;

#define PMIX_BFROP_BUFFER_TYPE_HTON(h);
#define PMIX_BFROP_BUFFER_TYPE_NTOH(h);

/**
 * Structure for holding a buffer */
typedef struct {
    /** First member must be the object's parent */
    pmix_object_t parent;
    /** type of buffer */
    pmix_bfrop_buffer_type_t type;
    /** Start of my memory */
    char *base_ptr;
    /** Where the next data will be packed to (within the allocated
        memory starting at base_ptr) */
    char *pack_ptr;
    /** Where the next data will be unpacked from (within the
        allocated memory starting as base_ptr) */
    char *unpack_ptr;

    /** Number of bytes allocated (starting at base_ptr) */
    size_t bytes_allocated;
    /** Number of bytes used by the buffer (i.e., amount of data --
        including overhead -- packed in the buffer) */
    size_t bytes_used;
} pmix_buffer_t;
PMIX_DECLSPEC PMIX_CLASS_DECLARATION (pmix_buffer_t);

/* these classes are required by the regex code shared
 * between the client and server implementations - it
 * is put here so that both can access these objects */
typedef struct {
    pmix_list_item_t super;
    int start;
    int cnt;
} pmix_regex_range_t;
PMIX_DECLSPEC PMIX_CLASS_DECLARATION(pmix_regex_range_t);

typedef struct {
    /* list object */
    pmix_list_item_t super;
    char *prefix;
    char *suffix;
    int num_digits;
    pmix_list_t ranges;
} pmix_regex_value_t;
PMIX_DECLSPEC PMIX_CLASS_DECLARATION(pmix_regex_value_t);

END_C_DECLS

#endif /* PMIX_BFROP_TYPES_H */
