/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2012 University of Houston. All rights reserved.
 * Copyright (c) 2010-2012 Los Alamos National Security, LLC.
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
#include <ctype.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#include <errno.h>

#include "opal/mca/installdirs/installdirs.h"
#include "opal/class/opal_object.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/runtime/opal.h"
#if OPAL_ENABLE_FT_CR == 1
#include "opal/runtime/opal_cr.h"
#endif
#include "opal/mca/base/base.h"
#include "opal/runtime/opal_info_support.h"
#include "opal/util/show_help.h"

#include "orte/runtime/orte_info_support.h"

#include "ompi/communicator/communicator.h"
#include "ompi/tools/ompi_info/ompi_info.h"

/*
 * Public variables
 */

const char *ompi_info_type_ompi = "ompi";
const char *ompi_info_type_base = "base";


int main(int argc, char *argv[])
{
    int ret = 0;
    bool acted = false;
    bool want_all = false;
    char **app_env = NULL, **global_env = NULL;
    int i;
    char *str;
    opal_cmd_line_t *ompi_info_cmd_line;
    opal_pointer_array_t mca_types;
    opal_pointer_array_t component_map;
    opal_info_component_map_t *map;

    /* Initialize the argv parsing handle */
    if (OPAL_SUCCESS != opal_init_util(&argc, &argv)) {
        opal_show_help("help-opal_info.txt", "lib-call-fail", true, 
                       "opal_init_util", __FILE__, __LINE__, NULL);
        exit(ret);
    }

    ompi_info_cmd_line = OBJ_NEW(opal_cmd_line_t);
    if (NULL == ompi_info_cmd_line) {
        ret = errno;
        opal_show_help("help-opal_info.txt", "lib-call-fail", true, 
                       "opal_cmd_line_create", __FILE__, __LINE__, NULL);
        exit(ret);
    }

    /* initialize the command line, parse it, and return the directives
     * telling us what the user wants output
     */
    if (OPAL_SUCCESS != (ret = opal_info_init(argc, argv, ompi_info_cmd_line))) {
        exit(ret);
    }

    /* setup the mca_types array */
    OBJ_CONSTRUCT(&mca_types, opal_pointer_array_t);
    opal_pointer_array_init(&mca_types, 256, INT_MAX, 128);
    
    /* add in the opal frameworks */
    opal_info_register_types(&mca_types);

    /* add in the orte frameworks */
    orte_info_register_types(&mca_types);

    /* add in the ompi frameworks */
    opal_pointer_array_add(&mca_types, "allocator");
    opal_pointer_array_add(&mca_types, "bml");
    opal_pointer_array_add(&mca_types, "btl");
    opal_pointer_array_add(&mca_types, "coll");
    opal_pointer_array_add(&mca_types, "common");
#if OPAL_ENABLE_FT_CR == 1
    opal_pointer_array_add(&mca_types, "crcp");
#endif
    opal_pointer_array_add(&mca_types, "dpm");
    opal_pointer_array_add(&mca_types, "fbtl");
    opal_pointer_array_add(&mca_types, "fcoll");
    opal_pointer_array_add(&mca_types, "fs");
    opal_pointer_array_add(&mca_types, "io");
    opal_pointer_array_add(&mca_types, "mpi");
    opal_pointer_array_add(&mca_types, "mpool");
    opal_pointer_array_add(&mca_types, "mtl");
    opal_pointer_array_add(&mca_types, "ompi");
    opal_pointer_array_add(&mca_types, "op");
    opal_pointer_array_add(&mca_types, "osc");
    opal_pointer_array_add(&mca_types, "pml");
    opal_pointer_array_add(&mca_types, "pubsub");
    opal_pointer_array_add(&mca_types, "rcache");
    opal_pointer_array_add(&mca_types, "sharedfp");
    opal_pointer_array_add(&mca_types, "topo");
    
    /* init the component map */
    OBJ_CONSTRUCT(&component_map, opal_pointer_array_t);
    opal_pointer_array_init(&component_map, 256, INT_MAX, 128);
    
    /* Register OPAL's params */
    if (OPAL_SUCCESS != (ret = opal_info_register_components(&mca_types, &component_map))) {
        if (OPAL_ERR_BAD_PARAM == ret) {
            /* output where the error occurred */
            opal_info_err_params(&component_map);
        }
        exit(1);
    }

    /* Register ORTE's params */
    if (ORTE_SUCCESS != (ret = orte_info_register_components(&mca_types, &component_map))) {
        if (OPAL_ERR_BAD_PARAM == ret) {
            /* output what we got */
            opal_info_do_params(true, opal_cmd_line_is_taken(ompi_info_cmd_line, "internal"),
                                &mca_types, NULL);
        }
        exit(1);
    }

    /* Register OMPI's params */
    if (OMPI_SUCCESS != (ret = ompi_info_register_components(&mca_types, &component_map))) {
        if (OMPI_ERR_BAD_PARAM == ret) {
            /* output what we got */
            opal_info_do_params(true, opal_cmd_line_is_taken(ompi_info_cmd_line, "internal"),
                                &mca_types, NULL);
        }
        exit(1);
    }

    /* Execute the desired action(s) */    
    want_all = opal_cmd_line_is_taken(ompi_info_cmd_line, "all");
    if (want_all || opal_cmd_line_is_taken(ompi_info_cmd_line, "version")) {
        ompi_info_do_version(want_all, ompi_info_cmd_line,
                             &mca_types, &component_map);
        acted = true;
    }
    if (want_all || opal_cmd_line_is_taken(ompi_info_cmd_line, "path")) {
        opal_info_do_path(want_all, ompi_info_cmd_line);
        acted = true;
    }
    if (want_all || opal_cmd_line_is_taken(ompi_info_cmd_line, "arch")) {
        opal_info_do_arch();
        acted = true;
    }
    if (want_all || opal_cmd_line_is_taken(ompi_info_cmd_line, "hostname")) {
        opal_info_do_hostname();
        acted = true;
    }
    if (want_all || opal_cmd_line_is_taken(ompi_info_cmd_line, "config")) {
        ompi_info_do_config(true);
        acted = true;
    }
    if (want_all || opal_cmd_line_is_taken(ompi_info_cmd_line, "param") ||
        opal_cmd_line_is_taken(ompi_info_cmd_line, "params")) {
        opal_info_do_params(want_all, opal_cmd_line_is_taken(ompi_info_cmd_line, "internal"),
                            &mca_types, ompi_info_cmd_line);
        acted = true;
    }
    
    /* If no command line args are specified, show default set */
    
    if (!acted) {
        ompi_info_show_ompi_version(opal_info_ver_full);
        opal_info_show_path(opal_info_path_prefix, opal_install_dirs.prefix);
        opal_info_do_arch();
        opal_info_do_hostname();
        ompi_info_do_config(false);
        for (i = 0; i < mca_types.size; ++i) {
            if (NULL == (str = (char*)opal_pointer_array_get_item(&mca_types, i))) {
                continue;
            }
            if (0 != strcmp("mpi", str)) {
                opal_info_show_component_version(&mca_types, &component_map,
                                                 str, opal_info_component_all, 
                                                 opal_info_ver_full, opal_info_type_all);
            }
        }
    }
    
    /* All done */
    
    if (NULL != app_env) {
        opal_argv_free(app_env);
    }
    if (NULL != global_env) {
        opal_argv_free(global_env);
    }
    ompi_info_close_components();
    OBJ_RELEASE(ompi_info_cmd_line);
    OBJ_DESTRUCT(&mca_types);
    for (i=0; i < component_map.size; i++) {
        if (NULL != (map = (opal_info_component_map_t*)opal_pointer_array_get_item(&component_map, i))) {
            OBJ_RELEASE(map);
        }
    }
    OBJ_DESTRUCT(&component_map);

    opal_info_finalize();

    /* Put our own call to opal_finalize_util() here because we called
       it up above (and it refcounts) */
    opal_finalize_util();
    
    return 0;
}
