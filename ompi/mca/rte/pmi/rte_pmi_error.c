/*
 * Copyright (c) 2013      Sandia National Laboratories. All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include <stdio.h>
#include <stdarg.h>
#include <pmi.h>
#if WANT_PMI2_SUPPORT
#include <pmi2.h>
#endif

#include "opal/util/error.h"
#include "opal/util/output.h"
#include "ompi/constants.h"
#include "ompi/mca/rte/rte.h"

#include "rte_pmi.h"


void
ompi_rte_abort(int error_code, char *fmt, ...)
{
    char *msg;
    int ret;
    va_list ap;

    va_start(ap, fmt);

    ret = vasprintf(&msg, fmt, ap);
    if (-1 == ret) msg = "";

    va_end(ap);

    PMI_Abort(error_code, msg);
}


int
ompi_rte_abort_peers(ompi_process_name_t *procs, size_t nprocs, int status)
{
    PMI_Abort(status, "");
    return OMPI_SUCCESS;
}


int
ompi_rte_error_log(const char *file, int line, 
                   const char *func, int ret)
{
    opal_output(0, "%s:%d:%s: Error: %s\n", file, line, func, opal_strerror(ret));
    return OMPI_SUCCESS;
}


void
ompi_rte_set_fault_callback(void (*callback)(opal_pointer_array_t*))
{
    /* This is intentionally a no-op.  We don't get async errors from PMI. */
}
