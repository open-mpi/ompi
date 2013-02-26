/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2007      The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC. All
 *                         rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/constants.h"
#include "opal/types.h"

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
    {
        int spawned, size, rank, appnum;

        /* if we can't startup PMI, we can't be used */
        if (PMI2_Initialized ()) {
            return true;
        }

        if (PMI_SUCCESS != PMI2_Init(&spawned, &size, &rank, &appnum)) {
            mca_common_pmi_init_count--;
            return false;
        }
    }
#else
    {
        PMI_BOOL initialized;

        if (PMI_SUCCESS != PMI_Initialized(&initialized)) {
            mca_common_pmi_init_count--;
            return false;
        }

        if (PMI_TRUE != initialized && PMI_SUCCESS != PMI_Init(&initialized)) {
            mca_common_pmi_init_count--;
            return false;
        }
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

/* useful util */
char* opal_errmgr_base_pmi_error(int pmi_err)
{
    char * err_msg;

    switch(pmi_err) {
        case PMI_FAIL: err_msg = "Operation failed"; break;
        case PMI_ERR_INIT: err_msg = "PMI is not initialized"; break;
        case PMI_ERR_NOMEM: err_msg = "Input buffer not large enough"; break;
        case PMI_ERR_INVALID_ARG: err_msg = "Invalid argument"; break;
        case PMI_ERR_INVALID_KEY: err_msg = "Invalid key argument"; break;
        case PMI_ERR_INVALID_KEY_LENGTH: err_msg = "Invalid key length argument"; break;
        case PMI_ERR_INVALID_VAL: err_msg = "Invalid value argument"; break;
        case PMI_ERR_INVALID_VAL_LENGTH: err_msg = "Invalid value length argument"; break;
        case PMI_ERR_INVALID_LENGTH: err_msg = "Invalid length argument"; break;
        case PMI_ERR_INVALID_NUM_ARGS: err_msg = "Invalid number of arguments"; break;
        case PMI_ERR_INVALID_ARGS: err_msg = "Invalid args argument"; break;
        case PMI_ERR_INVALID_NUM_PARSED: err_msg = "Invalid num_parsed length argument"; break;
        case PMI_ERR_INVALID_KEYVALP: err_msg = "Invalid keyvalp argument"; break;
        case PMI_ERR_INVALID_SIZE: err_msg = "Invalid size argument"; break;
#if defined(PMI_ERR_INVALID_KVS)
	/* pmi.h calls this a valid return code but mpich doesn't define it (slurm does). */
        case PMI_ERR_INVALID_KVS: err_msg = "Invalid kvs argument"; break;
#endif
        case PMI_SUCCESS: err_msg = "Success"; break;
        default: err_msg = "Unkown error";
    }
    return err_msg;
}
