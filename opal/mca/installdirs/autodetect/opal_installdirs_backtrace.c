/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2009 Sun Microsystems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include <execinfo.h>

uintptr_t
opal_installdirs_autodetect_pc()
{
    void *pc = 0;

    backtrace(&pc, 1);
    return pc;
}
