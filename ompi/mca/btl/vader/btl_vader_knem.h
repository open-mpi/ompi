/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#if !defined(BTL_VADER_KNEM_H)
#define BTL_VADER_KNEM_H

#if OMPI_BTL_VADER_HAVE_KNEM

#include <knem_io.h>
#include <sys/mman.h>

int mca_btl_vader_knem_init (void);
int mca_btl_vader_knem_fini (void);
int mca_btl_vader_knem_progress (void);

#endif /* OMPI_BTL_VADER_HAVE_KNEM */

#endif /* defined(BTL_VADER_KNEM_H) */
