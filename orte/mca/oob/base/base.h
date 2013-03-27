/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 *
 * the oob framework
 */

#ifndef _MCA_OOB_BASE_H_
#define _MCA_OOB_BASE_H_

#include "orte_config.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif
#ifdef HAVE_NET_UIO_H
#include <net/uio.h>
#endif

#include "orte/mca/oob/oob.h"

#include "opal/mca/mca.h"

BEGIN_C_DECLS

/*
 * MCA framework
 */
ORTE_DECLSPEC extern mca_base_framework_t orte_oob_base_framework;

/*
 * Global functions for MCA overall collective open and close
 */
ORTE_DECLSPEC int mca_oob_base_init(void);


/*
 * Global struct holding the selected module's function pointers
 */
extern char* mca_oob_base_include;
extern char* mca_oob_base_exclude;

END_C_DECLS
#endif

