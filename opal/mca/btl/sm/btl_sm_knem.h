/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef BTL_SM_KNEM_H
#define BTL_SM_KNEM_H

#if OPAL_BTL_SM_HAVE_KNEM

#    include <knem_io.h>

int mca_btl_sm_knem_init(void);
int mca_btl_sm_knem_fini(void);
int mca_btl_sm_knem_progress(void);

#endif /* OPAL_BTL_SM_HAVE_KNEM */

#endif /* BTL_SM_KNEM_H */
