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

#ifndef OPAL_MEMORY_MEMORY_INTERNAL_H
#define OPAL_MEMORY_MEMORY_INTERNAL_H

#define OPAL_MEMORY_FREE_SUPPORT   0x0001
#define OPAL_MEMORY_MALLOC_SUPPORT 0x0002
#define OPAL_MEMORY_CHUNK_SUPPORT  0x0004
#define OPAL_MEMORY_MMAP_SUPPORT   0x0008

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

void opal_mem_hooks_set_support(int support);

void opal_mem_hooks_release_hook(void *buf, size_t length, bool from_alloc);
void opal_mem_hooks_alloc_hook(void *buf, size_t length, bool from_alloc);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


#endif /* OPAL_MEMORY_MEMORY_INTERNAL_H */
