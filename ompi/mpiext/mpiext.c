/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2018      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdlib.h>

#include "ompi/constants.h"
#include "ompi/mpiext/mpiext.h"
#include "ompi/mpiext/static-components.h"
#include "ompi/instance/instance.h"

static int ompi_mpiext_fini (void)
{
    const ompi_mpiext_component_t **tmp = ompi_mpiext_components;
    int ret;

    while (NULL != (*tmp)) {
        if (NULL != (*tmp)->fini) {
            ret = (*tmp)->fini();
            if (OMPI_SUCCESS != ret) return ret;
        }
        tmp++;
    }

    return OMPI_SUCCESS;
}

int
ompi_mpiext_init(void)
{
    const ompi_mpiext_component_t **tmp = ompi_mpiext_components;
    int ret;

    while (NULL != (*tmp)) {
        if (NULL != (*tmp)->init) {
            ret = (*tmp)->init();
            if (OMPI_SUCCESS != ret) return ret;
        }
        tmp++;
    }

    ompi_mpi_instance_append_finalize (ompi_mpiext_fini);

    return OMPI_SUCCESS;
}
