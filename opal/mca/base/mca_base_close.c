/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009-2022 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/constants.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_component_repository.h"
#include "opal/mca/mca.h"
#include "opal/util/output.h"

extern int mca_base_opened;

/*
 * Main MCA shutdown.
 */

void mca_base_close(void)
{
    assert(mca_base_opened);
    if (--mca_base_opened) {
        return;
    }

    /* deregister all MCA base parameters */
    int group_id = mca_base_var_group_find("opal", "mca", "base");

    if (-1 < group_id) {
        mca_base_var_group_deregister(group_id);
    }

    /* release the default paths */
    free(mca_base_system_default_path);
    mca_base_system_default_path = NULL;

    free(mca_base_user_default_path);
    mca_base_user_default_path = NULL;

    /* Close down the component repository */
    mca_base_component_repository_finalize();

    /* Shut down the dynamic component finder */
    mca_base_component_find_finalize();

    /* Shut down the show_load_errors processing */
    mca_base_show_load_errors_finalize();

    /* Close opal output stream 0 */
    opal_output_close(0);
}
