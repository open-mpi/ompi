/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC.  All rights reserved.
 * Copyright (c) 2015-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 */

#ifndef MCA_PLM_BASE_H
#define MCA_PLM_BASE_H

/*
 * includes
 */
#include "prte_config.h"

#include "src/class/pmix_list.h"
#include "src/mca/base/pmix_mca_base_framework.h"
#include "src/mca/mca.h"
#include "src/util/pmix_printf.h"

#include "src/mca/plm/plm.h"

BEGIN_C_DECLS

/*
 * MCA framework
 */
PRTE_EXPORT extern pmix_mca_base_framework_t prte_plm_base_framework;
/*
 * Select an available component.
 */
PRTE_EXPORT int prte_plm_base_select(void);

/**
 * Functions that other frameworks may need to call directly
 * Specifically, the ODLS needs to access some of these
 * to avoid recursive callbacks
 */
PRTE_EXPORT void prte_plm_base_app_report_launch(int fd, short event, void *data);
PRTE_EXPORT void prte_plm_base_receive_process_msg(int fd, short event, void *data);

PRTE_EXPORT void prte_plm_base_set_slots(prte_node_t *node);
PRTE_EXPORT void prte_plm_base_setup_job(int fd, short args, void *cbdata);
PRTE_EXPORT void prte_plm_base_setup_job_complete(int fd, short args, void *cbdata);
PRTE_EXPORT void prte_plm_base_complete_setup(int fd, short args, void *cbdata);
PRTE_EXPORT void prte_plm_base_daemons_reported(int fd, short args, void *cbdata);
PRTE_EXPORT void prte_plm_base_allocation_complete(int fd, short args, void *cbdata);
PRTE_EXPORT void prte_plm_base_daemons_launched(int fd, short args, void *cbdata);
PRTE_EXPORT void prte_plm_base_vm_ready(int fd, short args, void *cbdata);
PRTE_EXPORT void prte_plm_base_mapping_complete(int fd, short args, void *cbdata);
PRTE_EXPORT void prte_plm_base_launch_apps(int fd, short args, void *cbdata);
PRTE_EXPORT void prte_plm_base_send_launch_msg(int fd, short args, void *cbdata);
PRTE_EXPORT void prte_plm_base_post_launch(int fd, short args, void *cbdata);
PRTE_EXPORT void prte_plm_base_registered(int fd, short args, void *cbdata);
PRTE_EXPORT void prte_plm_base_wrap_args(char **args);
PRTE_EXPORT int prte_plm_base_spawn_response(int32_t status, prte_job_t *jdata);

END_C_DECLS

#endif
