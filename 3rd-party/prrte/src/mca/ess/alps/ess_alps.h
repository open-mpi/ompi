/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PRTE_ESS_ALPS_H
#define PRTE_ESS_ALPS_H

#include "prte_config.h"
#include "src/mca/ess/ess.h"
#include "src/mca/mca.h"

#include "alps/alps.h"
#include "alps/alps_toolAssist.h"
#include "alps/libalpslli.h"
#include "alps/libalpsutil.h"

BEGIN_C_DECLS

/*
 * Module open / close
 */
int prte_mca_ess_alps_component_open(void);
int prte_mca_ess_alps_component_close(void);
int prte_mca_ess_alps_component_query(pmix_mca_base_module_t **module, int *priority);

/*
 * alps component internal utility functions
 */

int prte_ess_alps_get_first_rank_on_node(int *first_rank);
int prte_ess_alps_sync_start(void);
int prte_ess_alps_sync_complete(void);

/*
 * ODLS Alps module
 */
extern prte_ess_base_module_t prte_ess_alps_module;
PRTE_MODULE_EXPORT extern prte_ess_base_component_t prte_mca_ess_alps_component;

END_C_DECLS

#endif /* PRTE_ESS_ALPS_H */
