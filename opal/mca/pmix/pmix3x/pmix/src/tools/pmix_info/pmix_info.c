/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2010-2016 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014-2018 Intel, Inc. All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "pmix_config.h"

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
#include <signal.h>

#include "src/mca/pinstalldirs/base/base.h"
#include "src/class/pmix_object.h"
#include "src/class/pmix_pointer_array.h"
#include "src/util/argv.h"
#include "src/util/cmd_line.h"
#include "src/util/error.h"
#include "src/util/error.h"
#include "src/util/keyval_parse.h"
#include "src/util/output.h"
#include "src/util/show_help.h"
#include "src/mca/base/base.h"
#include "src/runtime/pmix_rte.h"

#include "pinfo.h"
#include "support.h"

/*
 * Public variables
 */

pmix_cmd_line_t *pmix_info_cmd_line = NULL;

const char *pmix_info_type_base = "base";

int main(int argc, char *argv[])
{
    int ret = 0;
    bool acted = false;
    bool want_all = false;
    int i;
    pmix_pointer_array_t mca_types;
    pmix_pointer_array_t component_map;
    pmix_info_component_map_t *map;

    /* protect against problems if someone passes us thru a pipe
     * and then abnormally terminates the pipe early */
    signal(SIGPIPE, SIG_IGN);

    /* initialize the output system */
    if (!pmix_output_init()) {
        return PMIX_ERROR;
    }

    /* initialize install dirs code */
    if (PMIX_SUCCESS != (ret = pmix_mca_base_framework_open(&pmix_pinstalldirs_base_framework, 0))) {
        fprintf(stderr, "pmix_pinstalldirs_base_open() failed -- process will likely abort (%s:%d, returned %d instead of PMIX_SUCCESS)\n",
                __FILE__, __LINE__, ret);
        return ret;
    }

    /* initialize the help system */
    pmix_show_help_init();

    /* keyval lex-based parser */
    if (PMIX_SUCCESS != (ret = pmix_util_keyval_parse_init())) {
        fprintf(stderr, "pmix_util_keyval_parse_init failed with %d\n", ret);
        return PMIX_ERROR;
    }

    /* Setup the parameter system */
    if (PMIX_SUCCESS != (ret = pmix_mca_base_var_init())) {
        fprintf(stderr, "pmix_mca_base_var_init failed with %d\n", ret);
        return PMIX_ERROR;
    }

    /* register params for pmix */
    if (PMIX_SUCCESS != (ret = pmix_register_params())) {
        fprintf(stderr, "pmix_register_params failed with %d\n", ret);
        return PMIX_ERROR;
    }

    pmix_info_cmd_line = PMIX_NEW(pmix_cmd_line_t);
    if (NULL == pmix_info_cmd_line) {
        ret = errno;
        pmix_show_help("help-pmix-info.txt", "lib-call-fail", true,
                       "pmix_cmd_line_create", __FILE__, __LINE__, NULL);
        exit(ret);
    }

    if (PMIX_SUCCESS != (ret = pmix_info_init(argc, argv, pmix_info_cmd_line))) {
        return ret;
    }

    /* setup the mca_types array */
    PMIX_CONSTRUCT(&mca_types, pmix_pointer_array_t);
    pmix_pointer_array_init(&mca_types, 256, INT_MAX, 128);
    pmix_info_register_types(&mca_types);

    /* init the component map */
    PMIX_CONSTRUCT(&component_map, pmix_pointer_array_t);
    pmix_pointer_array_init(&component_map, 64, INT_MAX, 32);

    /* Register PMIx's params */
    if (PMIX_SUCCESS != (ret = pmix_info_register_framework_params(&component_map))) {
        if (PMIX_ERR_BAD_PARAM == ret) {
            /* output what we got */
            pmix_info_do_params(true, pmix_cmd_line_is_taken(pmix_info_cmd_line, "internal"),
                                &mca_types, &component_map, NULL);
        }
        exit(1);
    }


    /* Execute the desired action(s) */
    want_all = pmix_cmd_line_is_taken(pmix_info_cmd_line, "all");
    if (want_all) {
        pmix_info_out("Package", "package", PMIX_PACKAGE_STRING);
        pmix_info_show_pmix_version(pmix_info_ver_full);
    }
    if (want_all || pmix_cmd_line_is_taken(pmix_info_cmd_line, "path")) {
        pmix_info_do_path(want_all, pmix_info_cmd_line);
        acted = true;
    }
    if (want_all || pmix_cmd_line_is_taken(pmix_info_cmd_line, "arch")) {
        pmix_info_do_arch();
        acted = true;
    }
    if (want_all || pmix_cmd_line_is_taken(pmix_info_cmd_line, "hostname")) {
        pmix_info_do_hostname();
        acted = true;
    }
    if (want_all || pmix_cmd_line_is_taken(pmix_info_cmd_line, "config")) {
        pmix_info_do_config(true);
        acted = true;
    }
    if (want_all || pmix_cmd_line_is_taken(pmix_info_cmd_line, "param") ||
        pmix_cmd_line_is_taken(pmix_info_cmd_line, "params")) {
        pmix_info_do_params(want_all, pmix_cmd_line_is_taken(pmix_info_cmd_line, "internal"),
                            &mca_types, &component_map, pmix_info_cmd_line);
        acted = true;
    }
    if (pmix_cmd_line_is_taken(pmix_info_cmd_line, "type")) {
        pmix_info_do_type(pmix_info_cmd_line);
        acted = true;
    }

    /* If no command line args are specified, show default set */

    if (!acted) {
        pmix_info_out("Package", "package", PMIX_PACKAGE_STRING);
        pmix_info_show_pmix_version(pmix_info_ver_full);
        pmix_info_show_path(pmix_info_path_prefix, pmix_pinstall_dirs.prefix);
        pmix_info_do_arch();
        pmix_info_do_hostname();
        pmix_info_do_config(false);
        pmix_info_show_component_version(&mca_types, &component_map, pmix_info_type_all,
                                         pmix_info_component_all, pmix_info_ver_full,
                                         pmix_info_ver_all);
    }


    /* All done */
    pmix_info_close_components();
    PMIX_RELEASE(pmix_info_cmd_line);
    PMIX_DESTRUCT(&mca_types);
    for (i=0; i < component_map.size; i++) {
        if (NULL != (map = (pmix_info_component_map_t*)pmix_pointer_array_get_item(&component_map, i))) {
            PMIX_RELEASE(map);
        }
    }
    PMIX_DESTRUCT(&component_map);

    pmix_info_finalize();

    return 0;
}
