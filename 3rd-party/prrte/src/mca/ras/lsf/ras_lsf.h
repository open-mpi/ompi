/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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
 * Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 *
 * Resource Allocation (LSF)
 */
#ifndef PRTE_RAS_LSF_H
#define PRTE_RAS_LSF_H

#include "prte_config.h"
#include "src/mca/ras/ras.h"

BEGIN_C_DECLS

/**
 * RAS Component
 */
PRTE_EXPORT extern prte_ras_base_component_t prte_mca_ras_lsf_component;
PRTE_EXPORT extern prte_ras_base_module_t prte_ras_lsf_module;

PRTE_EXPORT extern bool prte_ras_lsf_skip_affinity_file;

END_C_DECLS

#endif
