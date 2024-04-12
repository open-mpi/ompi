/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2010-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef _MCA_OOB_TCP_COMMON_H_
#define _MCA_OOB_TCP_COMMON_H_

#include "prte_config.h"

#include "oob_tcp.h"
#include "oob_tcp_peer.h"

PRTE_MODULE_EXPORT void prte_oob_tcp_set_socket_options(int sd);
PRTE_MODULE_EXPORT char *prte_oob_tcp_state_print(prte_oob_tcp_state_t state);
PRTE_MODULE_EXPORT prte_oob_tcp_peer_t *prte_oob_tcp_peer_lookup(const pmix_proc_t *name);
#endif /* _MCA_OOB_TCP_COMMON_H_ */
