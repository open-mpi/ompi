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
#include "opal/util/show_help.h"
#include "orte/util/sys_info.h"

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


void mca_btl_base_error_no_nics(const char* transport, 
                                const char* nic_name)
{
    char *procid;
    asprintf(&procid, "[%lu,%lu,%lu]", 
             ORTE_NAME_ARGS(orte_process_info.my_name));

    opal_show_help("help-mpi-btl-base.txt", "btl:no-nics",
                   true, procid, transport, orte_system_info.nodename,
                   nic_name);
    free(procid);
}
