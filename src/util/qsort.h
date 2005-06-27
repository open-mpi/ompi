/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#ifndef OMPI_QSORT_H
#define OMPI_QSORT_H

#ifdef HAV_SYS_TYPES_H
#include <sys/types.h> /* for size_t */
#endif

void ompi_qsort(void *a, size_t n, size_t es, int (*cmp)(const void *, const void*));

#endif
