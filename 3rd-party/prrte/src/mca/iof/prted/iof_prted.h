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
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
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
 * The prted IOF component is used in daemons.  It is used
 * to prted all IOF actions back to the "hnp" IOF component (i.e., the
 * IOF component that runs in the HNP).  The prted IOF component is
 * loaded in an prted and then tied to the stdin, stdout,
 * and stderr streams of created child processes via pipes.  The prted
 * IOF component in the prted then acts as the relay between the
 * stdin/stdout/stderr pipes and the IOF component in the HNP.
 * This design allows us to manipulate stdin/stdout/stderr from before
 * main() in the child process.
 *
 * Much of the intelligence of this component is actually contained in
 * iof_base_endpoint.c (reading and writing to local file descriptors,
 * setting up events based on file descriptors, etc.).
 *
 * A non-blocking OOB receive is posted at the initialization of this
 * component to receive all messages from the HNP (e.g., data
 * fragments from streams, ACKs to fragments).
 *
 * Flow control is employed on a per-stream basis to ensure that
 * SOURCEs don't overwhelm SINK resources (E.g., send an entire input
 * file to an prted before the target process has read any of it).
 *
 */
#ifndef PRTE_IOF_PRTED_H
#define PRTE_IOF_PRTED_H

#include "prte_config.h"

#include "src/class/pmix_list.h"

#include "src/mca/iof/iof.h"
#include "src/rml/rml_types.h"

BEGIN_C_DECLS

/**
 * IOF PRTED Component
 */
struct prte_mca_iof_prted_component_t {
    prte_iof_base_component_t super;
    pmix_list_t procs;
    bool xoff;
};
typedef struct prte_mca_iof_prted_component_t prte_mca_iof_prted_component_t;

PRTE_MODULE_EXPORT extern prte_mca_iof_prted_component_t prte_mca_iof_prted_component;
extern prte_iof_base_module_t prte_iof_prted_module;

void prte_iof_prted_recv(int status, pmix_proc_t *sender, pmix_data_buffer_t *buffer,
                         prte_rml_tag_t tag, void *cbdata);

void prte_iof_prted_read_handler(int fd, short event, void *data);
void prte_iof_prted_send_xonxoff(prte_iof_tag_t tag);

END_C_DECLS

#endif
