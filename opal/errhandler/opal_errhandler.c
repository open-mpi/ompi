/*
 * Copyright (c) 2015      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/errhandler/opal_errhandler.h"

opal_errhandler_fn_t errhandler = NULL;
void *cbdata = NULL;

void opal_register_errhandler(opal_errhandler_fn_t newerr, void *cbd)
{
    errhandler = newerr;
    cbdata = cbd;
}

void opal_deregister_errhandler(void)
{
    errhandler = NULL;
    cbdata = NULL;
}

void opal_invoke_errhandler(int status, opal_proc_t *proc)
{
    if (NULL != errhandler) {
        errhandler(status, proc, cbdata);
    }
}
