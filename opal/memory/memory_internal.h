/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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

void opal_mem_free_set_free_support(int support);
void opal_mem_free_release_hook(void *buf, size_t length);

#endif /* OPAL_MEMORY_MEMORY_INTERNAL_H */
