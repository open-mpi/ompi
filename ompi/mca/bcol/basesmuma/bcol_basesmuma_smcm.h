/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 *
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2014      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef BCOL_BASESMUMA_SMCM_H
#define BCOL_BASESMUMA_SMCM_H

#include <sys/mman.h>
#include <stdio.h>

#include "ompi_config.h"
#include "ompi/proc/proc.h"

#include "opal/class/opal_object.h"
#include "opal/class/opal_list.h"
#include "opal/sys/atomic.h"



typedef struct bcol_basesmuma_smcm_file_header_t {
    /* lock to control atomic access */
    opal_atomic_lock_t seg_lock;

    /* is the segment ready for use */
    volatile int32_t seg_inited;

    /* Offset to next available memory location available for allocation */
    size_t seg_offset;

    /* total size of the segment */
    size_t seg_size;
} bcol_basesmuma_smcm_file_header_t;


typedef struct bcol_basesmuma_smcm_mmap_t {
    /* double link list element */
    opal_list_item_t super;
    /* pointer to header imbeded in the shared memory file */
    bcol_basesmuma_smcm_file_header_t *map_seg;
    /* base address of the mmap'ed file */
    unsigned char *map_addr;
    /* base address of data segment */
    unsigned char *data_addr;
    /* How big it is (in bytes) */
    size_t map_size;
    /* Filename */
    char *map_path;
} bcol_basesmuma_smcm_mmap_t;

OBJ_CLASS_DECLARATION(bcol_basesmuma_smcm_mmap_t);


/* Struct that characterizes a shared memory file */
struct bcol_basesmuma_smcm_file_t {

    char *file_name;
    size_t size;
    size_t size_ctl_structure;
    size_t data_seg_alignment;
    size_t mpool_size;

};
typedef struct bcol_basesmuma_smcm_file_t bcol_basesmuma_smcm_file_t;


struct bcol_basesmuma_smcm_proc_item_t {
    opal_list_item_t item;          /* can put me on a free list */
    int refcnt;
    ompi_process_name_t peer;
    bcol_basesmuma_smcm_file_t sm_file;
    bcol_basesmuma_smcm_mmap_t *sm_mmap;   /* Pointer to peer's sm file */

};
typedef struct bcol_basesmuma_smcm_proc_item_t bcol_basesmuma_smcm_proc_item_t;

OBJ_CLASS_DECLARATION(bcol_basesmuma_smcm_proc_item_t);


/* allocate shared memory file 
 *   in_ptr - pointer to preallocated memory (if NULL, this will be mmaped)
 *   alignment - region memory alignment
 *   file name - fully qualified backing file name
*/

OMPI_DECLSPEC extern bcol_basesmuma_smcm_mmap_t *bcol_basesmuma_smcm_mem_reg(void *in_ptr,
                size_t length,
                size_t alignment,
                char* file_name);

OMPI_DECLSPEC extern bcol_basesmuma_smcm_mmap_t* bcol_basesmuma_smcm_create_mmap(int fd, 
        size_t size, char *file_name,
        size_t size_ctl_structure,
        size_t data_seg_alignment);

#endif
