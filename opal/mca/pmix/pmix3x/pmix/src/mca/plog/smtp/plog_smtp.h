/* -*- C -*-
 *
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009 Cisco Systems, Inc. All rights reserved.
 * Copyright (c) 2014-2018 Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */
#ifndef PLOG_SMTP_H
#define PLOG_SMTP_H

#include "pmix_config.h"

#include <netdb.h>

#include "libesmtp.h"

#include "src/mca/plog/plog.h"

BEGIN_C_DECLS

typedef struct {
    pmix_plog_base_component_t super;

    /* libesmtp version */
    char *version;

    /* SMTP server name and port */
    char *server;
    int port;

    /* To, From, Subject */
    char *to, **to_argv, *from_name, *from_addr, *subject;

    /* Mail body prefix and suffix */
    char *body_prefix, *body_suffix;

    /* struct hostent from resolved SMTP server name */
    struct hostent *server_hostent;

    /* Priority of this component */
    int priority;
} pmix_plog_smtp_component_t;


/*
 * Plog interfaces
 */
PMIX_EXPORT extern pmix_plog_smtp_component_t mca_plog_smtp_component;
extern pmix_plog_module_t pmix_plog_smtp_module;

END_C_DECLS

#endif
