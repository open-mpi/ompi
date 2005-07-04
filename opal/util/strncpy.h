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

#ifndef OPAL_STRNCPY_H
#define OPAL_STRNCPY_H

#include "ompi_config.h"
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

/*
 * Use opal_strncpy() instead of strncpy()
 */
#if defined(strncpy)
#undef strncpy
#endif
#define strncpy opal_strncpy


OMPI_DECLSPEC char *opal_strncpy(char *dest, const char *src, size_t len);

#endif /* OPAL_STRNCPY_H */
