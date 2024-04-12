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
 * Copyright (c) 2007-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2016-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 *
 * The hnp IOF component is used in HNP processes only.  It is the
 * "hub" for all IOF activity, meaning that *all* IOF traffic is
 * routed to the hnp component, and this component figures out where
 * it is supposed to go from there.  Specifically: there is *no*
 * direct proxy-to-proxy IOF communication.  If a proxy/orted wants to
 * get a stream from another proxy/orted, the stream will go
 * proxy/orted -> HNP -> proxy/orted.
 *
 * The hnp IOF component does two things: 1. forward fragments between
 * file descriptors and streams, and 2. maintain forwarding tables to
 * "route" incoming fragments to outgoing destinations (both file
 * descriptors and other published streams).
 *
 */

#ifndef PRTE_IOF_HNP_H
#define PRTE_IOF_HNP_H

#include "prte_config.h"

#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif /* HAVE_SYS_TYPES_H */
#ifdef HAVE_SYS_UIO_H
#    include <sys/uio.h>
#endif /* HAVE_SYS_UIO_H */
#ifdef HAVE_NET_UIO_H
#    include <net/uio.h>
#endif /* HAVE_NET_UIO_H */

#include "src/mca/iof/base/base.h"
#include "src/mca/iof/iof.h"

BEGIN_C_DECLS

/**
 * IOF HNP Component
 */
struct prte_mca_iof_hnp_component_t {
    prte_iof_base_component_t super;
    pmix_list_t procs;
    prte_event_t stdinsig;
};
typedef struct prte_mca_iof_hnp_component_t prte_mca_iof_hnp_component_t;

PRTE_MODULE_EXPORT extern prte_mca_iof_hnp_component_t prte_mca_iof_hnp_component;
extern prte_iof_base_module_t prte_iof_hnp_module;

void prte_iof_hnp_recv(int status, pmix_proc_t *sender, pmix_data_buffer_t *buffer,
                       prte_rml_tag_t tag, void *cbdata);

void prte_iof_hnp_read_local_handler(int fd, short event, void *cbdata);
void prte_iof_hnp_stdin_cb(int fd, short event, void *cbdata);
bool prte_iof_hnp_stdin_check(int fd);

int prte_iof_hnp_send_data_to_endpoint(const pmix_proc_t *host,
                                       const pmix_proc_t *target,
                                       prte_iof_tag_t tag,
                                       unsigned char *data, int numbytes);

END_C_DECLS

#endif
