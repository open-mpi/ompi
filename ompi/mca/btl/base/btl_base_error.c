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
#include <stdio.h>
#include <stdarg.h>

#include "btl_base_error.h"

int mca_btl_base_debug;

int mca_btl_base_err(const char* fmt, ...)
{
    va_list list;
    int ret;

    va_start(list, fmt);
    ret = vfprintf(stderr, fmt, list);
    va_end(list);
    return ret;
}


int mca_btl_base_out(const char* fmt, ...)
{
    va_list list;
    int ret;

    va_start(list, fmt);
    ret = vfprintf(stdout, fmt, list);
    va_end(list);
    return ret;
}


