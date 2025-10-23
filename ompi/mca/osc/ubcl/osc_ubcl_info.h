/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2025 Bull SAS.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_OSC_UBCL_INFO_H
#define MCA_OSC_UBCL_INFO_H

#include "ompi/mca/osc/ubcl/osc_ubcl.h"
#include "opal/util/info.h"
#include "ompi/win/win.h"

int osc_ubcl_read_info(struct opal_info_t *info, struct ompi_win_t *win);
int osc_ubcl_sync_disp_unit(mca_osc_ubcl_module_t *module, int disp_unit, bool need_synchro);
int osc_ubcl_get_disp_unit(mca_osc_ubcl_module_t *module, int target);
void osc_ubcl_fini_disp_unit(mca_osc_ubcl_module_t *module);

#endif /* MCA_OSC_UBCL_INFO_H */
