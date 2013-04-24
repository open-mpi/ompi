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
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2008-2011 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <string.h>

#include "mpi.h"

#include "opal/version.h"
#if OMPI_RTE_ORTE
#include "orte/version.h"
#endif
#include "ompi/version.h"
#include "opal/mca/base/base.h"
#include "opal/util/printf.h"
#include "opal/runtime/opal_info_support.h"

#if OMPI_RTE_ORTE
#include "orte/runtime/orte_info_support.h"
#endif

#include "ompi/tools/ompi_info/ompi_info.h"

/*
 * Show the version of Open MPI
 */
void ompi_info_show_ompi_version(const char *scope)
{
    char *tmp, *tmp2;

    opal_info_out("Package", "package", OPAL_PACKAGE_STRING);
    (void)asprintf(&tmp, "%s:version:full", ompi_info_type_ompi);
    tmp2 = opal_info_make_version_str(scope, 
                                      OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION, 
                                      OMPI_RELEASE_VERSION, 
                                      OMPI_GREEK_VERSION,
                                      OMPI_WANT_REPO_REV, OMPI_REPO_REV);
    opal_info_out("Open MPI", tmp, tmp2);
    free(tmp);
    free(tmp2);
    (void)asprintf(&tmp, "%s:version:repo", ompi_info_type_ompi);
    opal_info_out("Open MPI repo revision", tmp, OMPI_REPO_REV);
    free(tmp);
    (void)asprintf(&tmp, "%s:version:release_date", ompi_info_type_ompi);
    opal_info_out("Open MPI release date", tmp, OMPI_RELEASE_DATE);
    free(tmp);
    
#if OMPI_RTE_ORTE
    /* show the orte version */
    orte_info_show_orte_version(scope);
#endif

    /* show the opal version */
    opal_info_show_opal_version(scope);
    
    tmp2 = opal_info_make_version_str(scope, 
                                      MPI_VERSION, MPI_SUBVERSION, 
                                      0, "", 0, "");
    opal_info_out("MPI API", "mpi-api:version:full", tmp2);
    free(tmp2);

    opal_info_out("Ident string", "ident", OPAL_IDENT_STRING);
}


