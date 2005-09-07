/* @file */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "opal/util/output.h"

#include "opal/util/trace.h"

int opal_trace_handle;

void opal_trace_init(void)
{
#if ENABLE_TRACE
     opal_output_stream_t tracer;

   /* get a file setup for opal_output to use for the trace */
    tracer.lds_file_suffix = "trace";
    tracer.lds_want_file = true;

    opal_trace_handle = opal_output_open(&tracer);
#endif
}

void opal_trace_finalize(void)
{
#if ENABLE_TRACE
    opal_output_close(opal_trace_handle);
#endif
}
