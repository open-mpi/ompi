/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2025      Google, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#if !defined(MCA_BTL_UCT_MODEX_H)
#define MCA_BTL_UCT_MODEX_H

#include "btl_uct.h"

int mca_btl_uct_component_modex_send(void);

uint8_t *mca_btl_uct_find_modex(mca_btl_uct_modex_t *modex, mca_btl_uct_tl_t *tl, int *remote_module_index);

#endif /* !defined(MCA_BTL_UCT_MODEX_H) */
