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
 * Copyright (c) 2014      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file:
 */

#ifndef ORTE_ODLS_ALPS_H
#define ORTE_ODLS_ALPS_H

#include "orte_config.h"
#include "opal/mca/mca.h"
#include "orte/mca/odls/odls.h"

#include "alps/alps.h"
#include "alps/alps_toolAssist.h"
#include "alps/libalpsutil.h"
#include "alps/libalpslli.h"

BEGIN_C_DECLS

/*
 * Module open / close
 */
int orte_odls_alps_component_open(void);
int orte_odls_alps_component_close(void);
int orte_odls_alps_component_query(mca_base_module_t **module, int *priority);

/*
 * ODLS Alps module
 */
extern orte_odls_base_module_t orte_odls_alps_module;
ORTE_MODULE_DECLSPEC extern orte_odls_base_component_t mca_odls_alps_component;

/*
 * ODLS alps utils
 */

int orte_odls_alps_get_rdma_creds(void);
END_C_DECLS

#endif /* ORTE_ODLS_ALPS_H */
