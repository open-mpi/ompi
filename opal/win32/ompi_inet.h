/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2009 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_INET_H
#define OMPI_INET_H


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

OPAL_DECLSPEC int inet_pton(int af, const char *src, void *dst);
OPAL_DECLSPEC const char *inet_ntop(int af, const void *src, char *dst, size_t size);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* OMPI_INET_H */