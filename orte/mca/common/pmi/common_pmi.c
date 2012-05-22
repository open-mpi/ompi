/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2007      The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC. All
 *                         rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include <string.h>
#include <pmi.h>
#if WANT_CRAY_PMI2_EXT
#include <pmi2.h>
#endif

#include "common_pmi.h"

static int mca_common_pmi_init_count = 0;

bool mca_common_pmi_init (void) {
    if (0 < mca_common_pmi_init_count++) {
        return true;
    }

#if WANT_CRAY_PMI2_EXT
    int spawned, size, rank, appnum;

    /* if we can't startup PMI, we can't be used */
    if (PMI2_Initialized ()) {
        return true;
    }

    if (PMI_SUCCESS != PMI2_Init(&spawned, &size, &rank, &appnum)) {
        mca_common_pmi_init_count--;
        return false;
    }
#else
    PMI_BOOL initialized;

    if (PMI_SUCCESS != PMI_Initialized(&initialized)) {
        mca_common_pmi_init_count--;
        return false;
    }

    if (PMI_TRUE != initialized && PMI_SUCCESS != PMI_Init(&initialized)) {
        mca_common_pmi_init_count--;
        return false;
    }
#endif

    return true;
}

void mca_common_pmi_finalize (void) {
    if (0 == mca_common_pmi_init_count) {
        return;
    }

    if (0 == --mca_common_pmi_init_count) {
#if WANT_CRAY_PMI2_EXT
        PMI2_Finalize ();
#else
        PMI_Finalize ();
#endif
    }
}
