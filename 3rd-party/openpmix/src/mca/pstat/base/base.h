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
 * Copyright (c) 2007-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#ifndef PMIX_PSTAT_BASE_H
#define PMIX_PSTAT_BASE_H

#include "pmix_config.h"
#include "src/mca/base/pmix_mca_base_framework.h"
#include "src/mca/pstat/pstat.h"

/*
 * Global functions for MCA overall pstat open and close
 */

BEGIN_C_DECLS

/**
 * Framework structure declaration for this framework
 */
PMIX_EXPORT extern pmix_mca_base_framework_t pmix_pstat_base_framework;

/**
 * Select an available component.
 *
 * @return PMIX_SUCCESS Upon success.
 * @return PMIX_NOT_FOUND If no component can be selected.
 * @return PMIX_ERROR Upon other failure.
 *
 * At the end of this process, we'll either have a single
 * component that is selected and initialized, or no component was
 * selected.  If no component was selected, subsequent invocation
 * of the pstat functions will return an error indicating no data
 * could be obtained
 */
PMIX_EXPORT int pmix_pstat_base_select(void);

PMIX_EXPORT extern pmix_pstat_base_component_t *pmix_pstat_base_component;

END_C_DECLS

#endif /* PMIX_BASE_PSTAT_H */
