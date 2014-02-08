/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "oshmem_config.h"

#include <stdlib.h>

#include "orte/util/show_help.h"

#include "opal/runtime/opal_cr.h"
#include "opal/util/output.h"

#include "oshmem/constants.h" 
#include "oshmem/include/shmem.h"
#include "oshmem/runtime/params.h"
#include "oshmem/runtime/runtime.h"
#include "oshmem/shmem/shmem_api_logger.h"

void start_pes(int npes)
{
    /* spec says that npes are ignored for now */
    shmem_init();
}

#if !defined(OSHMEM_PROFILING) || (OSHMEM_PROFILING == 0)
int shmem_api_logger_output = -1;

void shmem_init(void)
{
    int err = OSHMEM_SUCCESS;
    int provided;
    int required = SHMEM_THREAD_SINGLE;

    if (oshmem_shmem_initialized) {
        /*
         * SPEC: If start_pes() is called multiple times, subsequent calls have no effect.
         */
        return;
    }

    shmem_api_logger_output = opal_output_open(NULL);
    opal_output_set_verbosity(shmem_api_logger_output,
                              oshmem_shmem_api_verbose);

    err = oshmem_shmem_init(0, NULL, required, &provided);
    if (OSHMEM_SUCCESS != err) {
        /* since spec does not propagete error to user we can only abort */
        SHMEM_API_ERROR("SHMEM failed to initialize - aborting");
        oshmem_shmem_abort(-1);
    }

    OPAL_CR_INIT_LIBRARY();
}

#endif /* OSHMEM_PROFILING */

