/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
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

#ifndef OPAL_UTIL_PTY_H
#define OPAL_UTIL_PTY_H

#ifdef HAVE_UTIL_H
#include <util.h>
#endif
#ifdef HAVE_TERMIOS_H
# include <termios.h>
#else
# ifdef HAVE_TERMIO_H
#  include <termio.h>
# endif
#endif

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

#if OMPI_ENABLE_PTY_SUPPORT 

OPAL_DECLSPEC int opal_openpty(int *amaster, int *aslave, char *name, 
                               struct termios *termp, struct winsize *winp);

#else

OPAL_DECLSPEC int opal_openpty(int *amaster, int *aslave, char *name,
                               void *termp, void *winpp);

#endif

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* OPAL_UTIL_PTY_H */
