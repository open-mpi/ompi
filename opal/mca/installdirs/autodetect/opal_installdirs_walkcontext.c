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

#include <ucontext.h>

static int
savepc(uintptr_t pc, int sig, void *storage)
{
    *(uintptr_t*)storage = pc;
    return 1;
}

uintptr_t
opal_installdirs_autodetect_pc()
{
    ucontext_t ctx;
    uintptr_t value = 0;

    if (getcontext(&ctx) == -1) {
        return 0;
    }
    walkcontext(&ctx, savepc, &value);
    return value;
}
