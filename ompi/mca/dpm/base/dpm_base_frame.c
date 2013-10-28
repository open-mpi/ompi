/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"

#include "ompi/mca/dpm/dpm.h"
#include "ompi/mca/dpm/base/base.h"

#include "ompi/mca/dpm/base/static-components.h"

/*
 * Globals
 */
OMPI_DECLSPEC ompi_dpm_base_module_t ompi_dpm = {
    NULL,
    ompi_dpm_base_null_connect_accept,
    ompi_dpm_base_null_disconnect,
    ompi_dpm_base_null_spawn,
    ompi_dpm_base_null_dyn_init,
    ompi_dpm_base_null_dyn_finalize,
    ompi_dpm_base_null_mark_dyncomm,
    ompi_dpm_base_null_open_port,
    ompi_dpm_base_null_parse_port, 
    ompi_dpm_base_null_route_to_port,
    ompi_dpm_base_null_close_port,
    NULL
};
ompi_dpm_base_component_t ompi_dpm_base_selected_component;

static int ompi_dpm_base_close(void)
{
    /* Close the selected component */
    if( NULL != ompi_dpm.finalize ) {
        ompi_dpm.finalize();
    }

    /* Close all available modules that are open */
    return mca_base_framework_components_close(&ompi_dpm_base_framework, NULL);
}

MCA_BASE_FRAMEWORK_DECLARE(ompi, dpm, NULL, NULL, NULL, ompi_dpm_base_close,
                           mca_dpm_base_static_components, 0);
