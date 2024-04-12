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
 * Copyright (c) 2007-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2010-2016 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#ifdef HAVE_NETDB_H
#    include <netdb.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#    include <sys/param.h>
#endif
#include <errno.h>
#include <signal.h>

#include "src/class/pmix_object.h"
#include "src/class/pmix_pointer_array.h"
#include "src/mca/base/pmix_base.h"
#include "src/mca/errmgr/errmgr.h"
#include "src/mca/prteinstalldirs/prteinstalldirs.h"
#include "src/mca/schizo/base/base.h"
#include "src/prted/pmix/pmix_server.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_basename.h"
#include "src/util/pmix_cmd_line.h"
#include "src/util/error.h"
#include "src/util/pmix_path.h"
#include "src/util/proc_info.h"
#include "src/util/pmix_show_help.h"

#include "constants.h"
#include "src/include/prte_frameworks.h"
#include "src/include/version.h"
#include "src/runtime/prte_locks.h"

#include "src/tools/prte_info/pinfo.h"

/*
 * Public variables
 */

bool prte_info_pretty = true;
pmix_cli_result_t prte_info_cmd_line = PMIX_CLI_RESULT_STATIC_INIT;

const char *prte_info_type_all = "all";
const char *prte_info_type_prte = "prte";
const char *prte_info_type_base = "base";

pmix_pointer_array_t mca_types = {{0}};

int main(int argc, char *argv[])
{
    int ret = 0;
    bool acted = false;
    bool want_all = false;
    int i;
    char *str;
    int option_index = 0;   /* getopt_long stores the option index here. */
    char *ptr;
    char *personality;
    prte_schizo_base_module_t *schizo;

    PRTE_HIDE_UNUSED_PARAMS(argc);

    /* protect against problems if someone passes us thru a pipe
     * and then abnormally terminates the pipe early */
    signal(SIGPIPE, SIG_IGN);

    prte_tool_basename = pmix_basename(argv[0]);
    prte_tool_actual = "prte_info";

    /* Initialize the argv parsing stuff */
    if (PRTE_SUCCESS != (ret = prte_init_util(PRTE_PROC_MASTER))) {
        pmix_show_help("help-prte-info.txt", "lib-call-fail", true, "prte_init_util", __FILE__,
                       __LINE__, NULL);
        exit(ret);
    }

    /* open the SCHIZO framework */
    ret = pmix_mca_base_framework_open(&prte_schizo_base_framework,
                                       PMIX_MCA_BASE_OPEN_DEFAULT);
    if (PRTE_SUCCESS != ret) {
        PRTE_ERROR_LOG(ret);
        return ret;
    }

    if (PRTE_SUCCESS != (ret = prte_schizo_base_select())) {
        PRTE_ERROR_LOG(ret);
        return ret;
    }

    /* look for any personality specification */
    personality = NULL;
    for (i = 0; NULL != argv[i]; i++) {
        if (0 == strcmp(argv[i], "--personality")) {
            personality = argv[i + 1];
            break;
        }
    }

    /* detect if we are running as a proxy and select the active
     * schizo module for this tool */
    schizo = prte_schizo_base_detect_proxy(personality);
    if (NULL == schizo) {
        pmix_show_help("help-schizo-base.txt", "no-proxy", true, prte_tool_basename, personality);
        return 1;
    }
    if (NULL == personality) {
        personality = schizo->name;
    }

    /* Register all global MCA Params */
    if (PRTE_SUCCESS != (ret = prte_register_params())) {
        if (PRTE_ERR_SILENT != ret) {
            pmix_show_help("help-prte-runtime", "prte_init:startup:internal-failure", true,
                           "prte register params",
                           PRTE_ERROR_NAME(ret), ret);
        }
        return 1;
    }

    /* parse the input argv to get values, including everyone's MCA params */
    PMIX_CONSTRUCT(&prte_info_cmd_line, pmix_cli_result_t);
    ret = schizo->parse_cli(argv, &prte_info_cmd_line, PMIX_CLI_SILENT);
    if (PRTE_SUCCESS != ret) {
        PMIX_DESTRUCT(&prte_info_cmd_line);
        if (PRTE_OPERATION_SUCCEEDED == ret) {
            return PRTE_SUCCESS;
        }
        if (PRTE_ERR_SILENT != ret) {
            fprintf(stderr, "%s: command line error (%s)\n", prte_tool_basename, prte_strerror(ret));
        }
        return ret;
    }
    // we do NOT accept arguments other than our own
    if (NULL != prte_info_cmd_line.tail) {
        str = PMIX_ARGV_JOIN_COMPAT(prte_info_cmd_line.tail, ' ');
        if (0 != strcmp(str, argv[0])) {
            ptr = pmix_show_help_string("help-pterm.txt", "no-args", false,
                                        prte_tool_basename, str, prte_tool_basename);
            free(str);
            if (NULL != ptr) {
                printf("%s", ptr);
                free(ptr);
            }
            return -1;
        }
        free(str);
    }

    /* setup the mca_types array */
    PMIX_CONSTRUCT(&mca_types, pmix_pointer_array_t);
    pmix_pointer_array_init(&mca_types, 256, INT_MAX, 128);

    /* add a type for prte itself */
    pmix_pointer_array_add(&mca_types, "mca");
    pmix_pointer_array_add(&mca_types, "prte");

    /* add a type for hwloc */
    pmix_pointer_array_add(&mca_types, "hwloc");

    /* let the pmix server register params */
    pmix_server_register_params();
    /* add those in */
    pmix_pointer_array_add(&mca_types, "pmix");

    /* add the rml and routed types since they are no
     * longer in a framework */
    pmix_pointer_array_add(&mca_types, "rml");
    pmix_pointer_array_add(&mca_types, "routed");

    /* push all the types found by autogen */
    for (i = 0; NULL != prte_frameworks[i]; i++) {
        pmix_pointer_array_add(&mca_types, prte_frameworks[i]->framework_name);
    }

    /* Execute the desired action(s) */
    want_all = pmix_cmd_line_is_taken(&prte_info_cmd_line, "all");
    if (want_all) {
        prte_info_do_version(want_all);
        acted = true;
    } else if (pmix_cmd_line_is_taken(&prte_info_cmd_line, "show-version")) {
        prte_info_do_version(false);
        acted = true;
    }
    if (want_all || pmix_cmd_line_is_taken(&prte_info_cmd_line, "path")) {
        prte_info_do_path(want_all);
        acted = true;
    }
    if (want_all || pmix_cmd_line_is_taken(&prte_info_cmd_line, "arch")) {
        prte_info_do_arch();
        acted = true;
    }
    if (want_all || pmix_cmd_line_is_taken(&prte_info_cmd_line, "hostname")) {
        prte_info_do_hostname();
        acted = true;
    }
    if (want_all || pmix_cmd_line_is_taken(&prte_info_cmd_line, "config")) {
        prte_info_do_config(true);
        acted = true;
    }
    if (want_all || pmix_cmd_line_is_taken(&prte_info_cmd_line, "param")) {
        prte_info_do_params(want_all, pmix_cmd_line_is_taken(&prte_info_cmd_line, "internal"));
        acted = true;
    }

    /* If no command line args are specified, show default set */

    if (!acted) {
        prte_info_show_prte_version(prte_info_ver_full);
        prte_info_show_path(prte_info_path_prefix, prte_install_dirs.prefix);
        prte_info_do_arch();
        prte_info_do_hostname();
        prte_info_do_config(false);
        prte_info_components_open();
        for (i = 0; i < mca_types.size; ++i) {
            if (NULL == (str = (char *) pmix_pointer_array_get_item(&mca_types, i))) {
                continue;
            }
            prte_info_show_component_version(str, prte_info_component_all, prte_info_ver_full,
                                             prte_info_type_all);
        }
    }

    /* All done */
    prte_info_components_close();
    PMIX_DESTRUCT(&mca_types);
    pmix_mca_base_close();

    return 0;
}
