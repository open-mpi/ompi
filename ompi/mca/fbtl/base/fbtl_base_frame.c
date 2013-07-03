/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2011 University of Houston. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"
#include <stdio.h>

#include "ompi/class/ompi_free_list.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "ompi/mca/fbtl/fbtl.h"
#include "ompi/mca/fbtl/base/base.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */
#ifdef __WINDOWS__
    const mca_base_component_t *mca_fbtl_base_static_components[] = {NULL};
#else 
#include "ompi/mca/fbtl/base/static-components.h"
#endif

/*
 * Global variables; most of which are loaded by back-ends of MCA
 * variables
 */
mca_fbtl_base_component_t mca_fbtl_base_selected_component;
mca_fbtl_base_module_t mca_fbtl;

MCA_BASE_FRAMEWORK_DECLARE(ompi, fbtl, NULL, NULL, NULL, NULL,
                           mca_fbtl_base_static_components, 0);
