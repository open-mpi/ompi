/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc.  All rights reserved.
 * Copyright (c) 2014-2016 Intel, Inc. All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 *
 */
#include <src/include/pmix_config.h>

#include <pmix_common.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "src/class/pmix_list.h"
#include "src/mca/psec/base/base.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "src/mca/psec/base/static-components.h"

/* Instantiate the global vars */
pmix_psec_globals_t pmix_psec_globals = {0};

static pmix_status_t pmix_psec_close(void)
{
  pmix_psec_base_active_module_t *active;

    if (!pmix_psec_globals.initialized) {
        return PMIX_SUCCESS;
    }
    pmix_psec_globals.initialized = false;

    PMIX_LIST_FOREACH(active, &pmix_psec_globals.actives, pmix_psec_base_active_module_t) {
      if (NULL != active->component->finalize) {
        active->component->finalize();
      }
    }
    PMIX_DESTRUCT(&pmix_psec_globals.actives);

    return PMIX_SUCCESS;
}

static pmix_status_t pmix_psec_open(pmix_mca_base_open_flag_t flags)
{
  /* initialize globals */
  pmix_psec_globals.initialized = true;
  PMIX_CONSTRUCT(&pmix_psec_globals.actives, pmix_list_t);
  return PMIX_SUCCESS;
}

PMIX_MCA_BASE_FRAMEWORK_DECLARE(pmix, psec, "PMIx Security Operations",
                                NULL, pmix_psec_open, pmix_psec_close,
                                mca_psec_base_static_components, 0);

PMIX_CLASS_INSTANCE(pmix_psec_base_active_module_t,
                    pmix_list_item_t,
                    NULL, NULL);
