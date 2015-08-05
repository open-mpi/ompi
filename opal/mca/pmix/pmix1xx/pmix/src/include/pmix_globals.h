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
 * Copyright (c) 2014-2015 Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef PMIX_GLOBALS_H
#define PMIX_GLOBALS_H

#include <private/autogen/config.h>
#include <pmix/rename.h>
#include <private/types.h>

#include PMIX_EVENT_HEADER

#include <pmix_common.h>

BEGIN_C_DECLS

#define PMIX_MAX_CRED_SIZE  131072   // set max at 128kbytes

/* define a global construct that includes values that must be shared
 * between various parts of the code library. Both the client
 * and server libraries must instance this structure */
typedef struct {
    char nspace[PMIX_MAX_NSLEN+1];
    int rank, pindex;
    pmix_event_base_t *evbase;
    int debug_output;
    pmix_notification_fn_t errhandler;
    bool server;
    bool connected;
} pmix_globals_t;

extern pmix_globals_t pmix_globals;

END_C_DECLS

#endif /* PMIX_GLOBALS_H */
