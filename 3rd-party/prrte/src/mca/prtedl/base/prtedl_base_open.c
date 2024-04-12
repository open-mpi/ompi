/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2015-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"

#include "src/mca/prtedl/base/base.h"

#include "src/mca/prtedl/base/static-components.h"

/*
 * Globals
 */
prte_prtedl_base_module_t *prte_prtedl = NULL;
prte_prtedl_base_component_t *prte_prtedl_base_selected_component = NULL;

/*
 * Function for finding and opening either all MCA components,
 * or the one that was specifically requested via a MCA parameter.
 *
 * Note that we really don't need this function -- we could specify a
 * NULL pointer in the framework declare and the base would do this
 * exact same thing.  However, we need to have at least some
 * executable code in this file, or some linkers (cough cough OS X
 * cough cough) may not actually link in this .o file.
 */
int prte_dl_base_open(pmix_mca_base_open_flag_t flags)
{
    /* Open up all available components */
    return pmix_mca_base_framework_components_open(&prte_prtedl_base_framework, flags);
}

/* VERY IMPORTANT: This framework is static, and is opened before any
   other dyanmic frameworks are opened (which makes sense, of course).
   But we must mark this framework is NO_DSO so that the MCA framework
   base doesn't try to open any dynamic components in this
   framework. */
PMIX_MCA_BASE_FRAMEWORK_DECLARE(prte, prtedl, "Dynamic loader framework", NULL /* register */,
                                prte_dl_base_open /* open */, NULL /* close */,
                                prte_prtedl_base_static_components,
                                PMIX_MCA_BASE_FRAMEWORK_FLAG_NO_DSO);
