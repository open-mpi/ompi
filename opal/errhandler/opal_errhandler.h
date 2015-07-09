/*
 * Copyright (c) 2015      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_ERRHANDLER_H
#define OPAL_ERRHANDLER_H

#include "opal_config.h"

#include "opal/util/proc.h"

typedef void (*opal_errhandler_fn_t)(int status, opal_proc_t *proc, void *cbdata);

OPAL_DECLSPEC void opal_register_errhandler(opal_errhandler_fn_t errhandler, void *cbdata);

OPAL_DECLSPEC void opal_deregister_errhandler(void);

OPAL_DECLSPEC void opal_invoke_errhandler(int status, opal_proc_t *proc);

#endif
