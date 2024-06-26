/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* vim: set ft=c.mpich : */
/*
 *  (C) 2018 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef MPL_MISC_H_INCLUDED
#define MPL_MISC_H_INCLUDED

#include "mplconfig.h"

/* Returns the number of processors currently available in the system */
int MPL_get_nprocs(void);

#if defined (MPL_HAVE_MKSTEMP) && defined (MPL_NEEDS_MKSTEMP_DECL)
int mkstemp(char *template);
#endif

#if defined MPL_HAVE_MKSTEMP
#define MPL_mkstemp mkstemp
#else
int MPL_mkstemp(char *template);
#endif

int MPL_hex_encode(int size, const void *src, char *dest);
int MPL_hex_decode(int size, const char *src, void *dest);

#endif /* MPL_MISC_H_INCLUDED */
