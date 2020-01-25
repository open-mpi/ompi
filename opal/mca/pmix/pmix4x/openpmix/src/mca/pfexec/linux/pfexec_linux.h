/*
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
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file:
 */

#ifndef PMIX_PFEXEC_LINUX_H
#define PMIX_PFEXEC_LINUX_H

#include "pmix_config.h"

#include "src/mca/mca.h"

#include "src/mca/pfexec/pfexec.h"

BEGIN_C_DECLS

/*
 * PFEXEC Linux module
 */
PMIX_EXPORT extern pmix_pfexec_base_module_t pmix_pfexec_linux_module;
PMIX_EXPORT extern pmix_pfexec_base_component_t mca_pfexec_linux_component;

END_C_DECLS

#endif /* PMIX_PFEXEC_H */
