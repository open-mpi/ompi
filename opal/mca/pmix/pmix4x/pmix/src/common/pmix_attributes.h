/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2015-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2017      IBM Corporation.  All rights reserved.
 * Copyright (c) 2017      Mellanox Technologies. All rights reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 *
 * I/O Forwarding Service
 */

#ifndef PMIX_ATTRIBUTES_H
#define PMIX_ATTRIBUTES_H

#include <src/include/pmix_config.h>

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif
#ifdef HAVE_NET_UIO_H
#include <net/uio.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "src/class/pmix_list.h"

BEGIN_C_DECLS

PMIX_EXPORT void pmix_init_registered_attrs(void);
PMIX_EXPORT void pmix_release_registered_attrs(void);
PMIX_EXPORT pmix_status_t pmix_register_tool_attrs(void);

PMIX_EXPORT pmix_status_t pmix_register_client_attrs(void);
PMIX_EXPORT pmix_status_t pmix_register_server_attrs(void);

PMIX_EXPORT char** pmix_attributes_print_functions(char *level);
PMIX_EXPORT char** pmix_attributes_print_attr(char *level, char *function);
PMIX_EXPORT void pmix_attributes_print_attrs(char ***ans, char *function,
                                 pmix_regattr_t *attrs,
                                 size_t nattrs);
PMIX_EXPORT void pmix_attributes_print_headers(char ***ans, char *level);

PMIX_EXPORT void pmix_attrs_query_support(int sd, short args, void *cbdata);

END_C_DECLS

#endif /* PMIX_IOF_H */
