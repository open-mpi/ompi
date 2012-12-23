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
#include "orte/version.h"
#include "ompi/version.h"
#include "opal/mca/base/base.h"
#include "opal/util/printf.h"
#include "opal/runtime/opal_info_support.h"

#include "orte/runtime/orte_info_support.h"

#include "ompi/tools/ompi_info/ompi_info.h"

/*
 * Public variables
 */


/*
 * Private functions
 */


/*
 * do_version
 *
 * Determines the version information related to the ompi components
 * being used.
 * Accepts: 
 *	- want_all: True if all components' info is required.
 *	- cmd_line: The constructed command line argument
 */
void ompi_info_do_version(bool want_all, opal_cmd_line_t *cmd_line,
                          opal_pointer_array_t *mca_types,
                          opal_pointer_array_t *component_map)
{
    unsigned int count;
    size_t i;
    char *arg1, *scope, *type, *component;
    char *pos;
    int j;
    
    if (want_all) {
        ompi_info_show_ompi_version(opal_info_ver_full);
        for (j = 0; j < mca_types->size; ++j) {
            if (NULL == (pos = (char*)opal_pointer_array_get_item(mca_types, j))) {
                continue;
            }
            opal_info_show_component_version(mca_types, component_map,
                                             pos, opal_info_component_all,
                                             opal_info_ver_full, opal_info_type_all);
        }
    } else {
        count = opal_cmd_line_get_ninsts(cmd_line, "version");
        for (i = 0; i < count; ++i) {
            arg1 = opal_cmd_line_get_param(cmd_line, "version", (int)i, 0);
            scope = opal_cmd_line_get_param(cmd_line, "version", (int)i, 1);
            
            /* Version of Open MPI */
            
            if (0 == strcmp(ompi_info_type_ompi, arg1)) {
                ompi_info_show_ompi_version(scope);
            } else if (0 == strcmp(orte_info_type_orte, arg1)) {
                orte_info_show_orte_version(scope);
            } else if (0 == strcmp(opal_info_type_opal, arg1)) {
                opal_info_show_opal_version(scope);
            } else if (NULL != (pos = strchr(arg1, ':'))) {
            /* Specific type and component */
                *pos = '\0';
                type = arg1;
                pos++;
                component = pos;
                
                opal_info_show_component_version(mca_types, component_map,
                                                 type, component, scope, opal_info_ver_all);
                
            }
            
            /* All components of a specific type */
            
            else {
                opal_info_show_component_version(mca_types, component_map,
                                                 arg1, opal_info_component_all, scope, opal_info_ver_all);
            }
        }
    }
}


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
    
    /* show the orte version */
    orte_info_show_orte_version(scope);

    /* show the opal version */
    opal_info_show_opal_version(scope);
    
    tmp2 = opal_info_make_version_str(scope, 
                                      MPI_VERSION, MPI_SUBVERSION, 
                                      0, "", 0, "");
    opal_info_out("MPI API", "mpi-api:version:full", tmp2);
    free(tmp2);

    opal_info_out("Ident string", "ident", OPAL_IDENT_STRING);
}


