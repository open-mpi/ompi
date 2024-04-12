/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2015-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2018      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2018      Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2021      FUJITSU LIMITED.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"

#include <ctype.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#    include <sys/param.h>
#endif
#ifdef HAVE_NETDB_H
#    include <netdb.h>
#endif

#include "src/class/pmix_pointer_array.h"
#include "src/class/pmix_value_array.h"
#include "src/include/constants.h"
#include "src/include/prte_portable_platform.h"
#include "src/include/version.h"
#include "src/mca/prteinstalldirs/prteinstalldirs.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_printf.h"
#include "src/util/pmix_show_help.h"

#include "src/tools/prte_info/pinfo.h"

/*
 * Public variables
 */

const char *prte_info_component_all = "all";
const char *prte_info_param_all = "all";

const char *prte_info_path_prefix = "prefix";
const char *prte_info_path_bindir = "bindir";
const char *prte_info_path_libdir = "libdir";
const char *prte_info_path_incdir = "incdir";
const char *prte_info_path_mandir = "mandir";
const char *prte_info_path_pkglibdir = "pkglibdir";
const char *prte_info_path_sysconfdir = "sysconfdir";
const char *prte_info_path_exec_prefix = "exec_prefix";
const char *prte_info_path_sbindir = "sbindir";
const char *prte_info_path_libexecdir = "libexecdir";
const char *prte_info_path_datarootdir = "datarootdir";
const char *prte_info_path_datadir = "datadir";
const char *prte_info_path_sharedstatedir = "sharedstatedir";
const char *prte_info_path_localstatedir = "localstatedir";
const char *prte_info_path_infodir = "infodir";
const char *prte_info_path_pkgdatadir = "pkgdatadir";
const char *prte_info_path_pkgincludedir = "pkgincludedir";

void prte_info_do_params(bool want_all_in, bool want_internal)
{
    char *type, *str;
    char **args = NULL, **tmp;
    bool found;
    int i, j;
    bool want_all = false;
    pmix_cli_item_t *opt;

    prte_info_components_open();
    opt = pmix_cmd_line_get_param(&prte_info_cmd_line, "param");

    if (want_all_in) {
        want_all = true;
    } else {
        /* See if the special param "all" was given to --param; that
         * superceeds any individual type
         */
        if (NULL != opt) {
            /* split the arguments at the colon */
            args = PMIX_ARGV_SPLIT_COMPAT(opt->values[0], ':');
            if (0 == strcmp(args[0], "all")) {
                want_all = true;
            }
        }
    }

    /* Show the params */
    if (want_all) {
        for (i = 0; i < mca_types.size; ++i) {
            if (NULL == (type = (char *) pmix_pointer_array_get_item(&mca_types, i))) {
                continue;
            }
            prte_info_show_mca_params(type, prte_info_component_all, want_internal);
        }
    } else {
        if (NULL != opt && NULL != args) {
            type = args[0];
            if (NULL != args[1]) {
                tmp = PMIX_ARGV_SPLIT_COMPAT(args[1], ',');

                for (j=0; NULL != tmp[j]; j++) {
                    for (found = false, i = 0; i < mca_types.size; ++i) {
                        str = (char *) pmix_pointer_array_get_item(&mca_types, i);
                        if (NULL == str) {
                            continue;
                        }
                        if (0 == strcmp(str, type)) {
                            found = true;
                            break;
                        }
                    }

                    if (!found) {
                        pmix_show_help("help-prte-info.txt", "not-found", true, type);
                        exit(1);
                    }

                    prte_info_show_mca_params(type, tmp[j], want_internal);
                }
                PMIX_ARGV_FREE_COMPAT(tmp);
            } else {
                prte_info_show_mca_params(type, "*", want_internal);
            }
        }
    }
    if (NULL != args) {
        PMIX_ARGV_FREE_COMPAT(args);
    }
}

static void prte_info_show_mca_group_params(const pmix_mca_base_var_group_t *group,
                                            bool want_internal)
{
    const pmix_mca_base_var_t *var;
    const int *variables;
    int ret, i, j, count;
    const int *groups;
    char **strings;

    variables = PMIX_VALUE_ARRAY_GET_BASE(&group->group_vars, const int);
    count = pmix_value_array_get_size((pmix_value_array_t *) &group->group_vars);

    for (i = 0; i < count; ++i) {
        ret = pmix_mca_base_var_get(variables[i], &var);
        if (PRTE_SUCCESS != ret) {
            continue;
        }

        ret = pmix_mca_base_var_dump(variables[i], &strings,
                                     !prte_info_pretty ? PMIX_MCA_BASE_VAR_DUMP_PARSABLE
                                                       : PMIX_MCA_BASE_VAR_DUMP_READABLE);
        if (PRTE_SUCCESS != ret) {
            continue;
        }

        for (j = 0; strings[j]; ++j) {
            if (0 == j && prte_info_pretty) {
                char *message;

                pmix_asprintf(&message, "MCA %s", group->group_framework);
                prte_info_out(message, message, strings[j]);
                free(message);
            } else {
                prte_info_out("", "", strings[j]);
            }
            free(strings[j]);
        }
        free(strings);
    }

    groups = PMIX_VALUE_ARRAY_GET_BASE(&group->group_subgroups, const int);
    count = pmix_value_array_get_size((pmix_value_array_t *) &group->group_subgroups);

    for (i = 0; i < count; ++i) {
        ret = pmix_mca_base_var_group_get(groups[i], &group);
        if (PRTE_SUCCESS != ret) {
            continue;
        }
        prte_info_show_mca_group_params(group, want_internal);
    }
}

void prte_info_show_mca_params(const char *type, const char *component, bool want_internal)
{
    const pmix_mca_base_var_group_t *group;
    int ret;

    if (0 == strcmp(component, "all")) {
        ret = pmix_mca_base_var_group_find("*", type, NULL);
        if (0 > ret) {
            return;
        }

        (void) pmix_mca_base_var_group_get(ret, &group);

        prte_info_show_mca_group_params(group, want_internal);
    } else {
        ret = pmix_mca_base_var_group_find("*", type, component);
        if (0 > ret) {
            return;
        }

        (void) pmix_mca_base_var_group_get(ret, &group);
        prte_info_show_mca_group_params(group, want_internal);
    }
}

void prte_info_do_path(bool want_all)
{
    int i;
    char *scope;
    pmix_cli_item_t *opt;

    /* Check bozo case */
    opt = pmix_cmd_line_get_param(&prte_info_cmd_line, "path");
    if (NULL != opt) {
        for (i=0; NULL != opt->values[i]; i++) {
            scope = opt->values[i];
            if (0 == strcmp("all", scope)) {
                want_all = true;
                break;
            }
        }
    }

    if (want_all) {
        prte_info_show_path(prte_info_path_prefix, prte_install_dirs.prefix);
        prte_info_show_path(prte_info_path_exec_prefix, prte_install_dirs.exec_prefix);
        prte_info_show_path(prte_info_path_bindir, prte_install_dirs.bindir);
        prte_info_show_path(prte_info_path_sbindir, prte_install_dirs.sbindir);
        prte_info_show_path(prte_info_path_libdir, prte_install_dirs.libdir);
        prte_info_show_path(prte_info_path_incdir, prte_install_dirs.includedir);
        prte_info_show_path(prte_info_path_mandir, prte_install_dirs.mandir);
        prte_info_show_path(prte_info_path_pkglibdir, prte_install_dirs.prtelibdir);
        prte_info_show_path(prte_info_path_libexecdir, prte_install_dirs.libexecdir);
        prte_info_show_path(prte_info_path_datarootdir, prte_install_dirs.datarootdir);
        prte_info_show_path(prte_info_path_datadir, prte_install_dirs.datadir);
        prte_info_show_path(prte_info_path_sysconfdir, prte_install_dirs.sysconfdir);
        prte_info_show_path(prte_info_path_sharedstatedir, prte_install_dirs.sharedstatedir);
        prte_info_show_path(prte_info_path_localstatedir, prte_install_dirs.localstatedir);
        prte_info_show_path(prte_info_path_infodir, prte_install_dirs.infodir);
        prte_info_show_path(prte_info_path_pkgdatadir, prte_install_dirs.prtedatadir);
        prte_info_show_path(prte_info_path_pkglibdir, prte_install_dirs.prtelibdir);
        prte_info_show_path(prte_info_path_pkgincludedir, prte_install_dirs.prteincludedir);
    } else {
        if (NULL != opt) {
            for (i=0; NULL != opt->values[i]; i++) {
                scope = opt->values[i];

                if (0 == strcmp(prte_info_path_prefix, scope)) {
                    prte_info_show_path(prte_info_path_prefix, prte_install_dirs.prefix);
                } else if (0 == strcmp(prte_info_path_bindir, scope)) {
                    prte_info_show_path(prte_info_path_bindir, prte_install_dirs.bindir);
                } else if (0 == strcmp(prte_info_path_libdir, scope)) {
                    prte_info_show_path(prte_info_path_libdir, prte_install_dirs.libdir);
                } else if (0 == strcmp(prte_info_path_incdir, scope)) {
                    prte_info_show_path(prte_info_path_incdir, prte_install_dirs.includedir);
                } else if (0 == strcmp(prte_info_path_mandir, scope)) {
                    prte_info_show_path(prte_info_path_mandir, prte_install_dirs.mandir);
                } else if (0 == strcmp(prte_info_path_pkglibdir, scope)) {
                    prte_info_show_path(prte_info_path_pkglibdir, prte_install_dirs.prtelibdir);
                } else if (0 == strcmp(prte_info_path_sysconfdir, scope)) {
                    prte_info_show_path(prte_info_path_sysconfdir, prte_install_dirs.sysconfdir);
                } else if (0 == strcmp(prte_info_path_exec_prefix, scope)) {
                    prte_info_show_path(prte_info_path_exec_prefix, prte_install_dirs.exec_prefix);
                } else if (0 == strcmp(prte_info_path_sbindir, scope)) {
                    prte_info_show_path(prte_info_path_sbindir, prte_install_dirs.sbindir);
                } else if (0 == strcmp(prte_info_path_libexecdir, scope)) {
                    prte_info_show_path(prte_info_path_libexecdir, prte_install_dirs.libexecdir);
                } else if (0 == strcmp(prte_info_path_datarootdir, scope)) {
                    prte_info_show_path(prte_info_path_datarootdir, prte_install_dirs.datarootdir);
                } else if (0 == strcmp(prte_info_path_datadir, scope)) {
                    prte_info_show_path(prte_info_path_datadir, prte_install_dirs.datadir);
                } else if (0 == strcmp(prte_info_path_sharedstatedir, scope)) {
                    prte_info_show_path(prte_info_path_sharedstatedir,
                                        prte_install_dirs.sharedstatedir);
                } else if (0 == strcmp(prte_info_path_localstatedir, scope)) {
                    prte_info_show_path(prte_info_path_localstatedir, prte_install_dirs.localstatedir);
                } else if (0 == strcmp(prte_info_path_infodir, scope)) {
                    prte_info_show_path(prte_info_path_infodir, prte_install_dirs.infodir);
                } else if (0 == strcmp(prte_info_path_pkgdatadir, scope)) {
                    prte_info_show_path(prte_info_path_pkgdatadir, prte_install_dirs.prtedatadir);
                } else if (0 == strcmp(prte_info_path_pkgincludedir, scope)) {
                    prte_info_show_path(prte_info_path_pkgincludedir, prte_install_dirs.prteincludedir);
                } else {
                    pmix_show_help("help-prte-info.txt", "usage", true, "USAGE");
                    exit(1);
                }
            }
        }
    }
}

void prte_info_show_path(const char *type, const char *value)
{
    char *pretty, *path;

    pretty = strdup(type);
    pretty[0] = toupper(pretty[0]);

    pmix_asprintf(&path, "path:%s", type);
    prte_info_out(pretty, path, value);
    free(pretty);
    free(path);
}

void prte_info_do_arch()
{
    prte_info_out("Configured architecture", "config:arch", PRTE_ARCH);
}

void prte_info_do_hostname()
{
    prte_info_out("Configure host", "config:host", PRTE_CONFIGURE_HOST);
}

/*
 * do_config
 * Accepts:
 *      - want_all: boolean flag; TRUE -> display all options
 *                                FALSE -> display selected options
 *
 * This function displays all the options with which the current
 * installation of prte was configured. There are many options here
 * that are carried forward from PRTE-7 and are not mca parameters
 * in PRTE-10. I have to dig through the invalid options and replace
 * them with PRTE-10 options.
 */
void prte_info_do_config(bool want_all)
{
    char *debug;
    char *have_dl;
    char *prun_prefix_by_default;
    char *symbol_visibility;
    char *manpages;

    /* setup the strings that don't require allocations*/
    debug = PRTE_ENABLE_DEBUG ? "yes" : "no";
    have_dl = PRTE_HAVE_DL_SUPPORT ? "yes" : "no";
    prun_prefix_by_default = PRTE_WANT_PRTE_PREFIX_BY_DEFAULT ? "yes" : "no";
    symbol_visibility = PRTE_C_HAVE_VISIBILITY ? "yes" : "no";
    manpages = "yes";

    /* output values */
    prte_info_out("Configured by", "config:user", PRTE_CONFIGURE_USER);
    prte_info_out("Configured on", "config:timestamp", PRTE_CONFIGURE_DATE);
    prte_info_out("Configure host", "config:host", PRTE_CONFIGURE_HOST);
    prte_info_out("Configure command line", "config:cli", PRTE_CONFIGURE_CLI);

    prte_info_out("Built by", "build:user", PRTE_BUILD_USER);
    prte_info_out("Built on", "build:timestamp", PRTE_BUILD_DATE);
    prte_info_out("Built host", "build:host", PRTE_BUILD_HOST);

    prte_info_out("C compiler", "compiler:c:command", PRTE_CC);
    prte_info_out("C compiler absolute", "compiler:c:absolute", PRTE_CC_ABSOLUTE);
    prte_info_out("C compiler family name", "compiler:c:familyname",
                  PLATFORM_STRINGIFY(PLATFORM_COMPILER_FAMILYNAME));
    prte_info_out("C compiler version", "compiler:c:version",
                  PLATFORM_STRINGIFY(PLATFORM_COMPILER_VERSION_STR));

    if (want_all) {
        prte_info_out_int("C char size", "compiler:c:sizeof:char", sizeof(char));
        prte_info_out_int("C bool size", "compiler:c:sizeof:bool", sizeof(bool));
        prte_info_out_int("C short size", "compiler:c:sizeof:short", sizeof(short));
        prte_info_out_int("C int size", "compiler:c:sizeof:int", sizeof(int));
        prte_info_out_int("C long size", "compiler:c:sizeof:long", sizeof(long));
        prte_info_out_int("C float size", "compiler:c:sizeof:float", sizeof(float));
        prte_info_out_int("C double size", "compiler:c:sizeof:double", sizeof(double));
        prte_info_out_int("C pointer size", "compiler:c:sizeof:pointer", sizeof(void *));
        prte_info_out("C bool align", "compiler:c:align:bool", "skipped");
        prte_info_out_int("C int align", "compiler:c:align:int", ALIGNOF_INT);
        prte_info_out_int("C double align", "compiler:c:align:double", ALIGNOF_DOUBLE);
    }

    prte_info_out("Thread support", "option:threads", "posix");

    if (want_all) {

        prte_info_out("Build CFLAGS", "option:build:cflags", PRTE_BUILD_CFLAGS);
        prte_info_out("Build LDFLAGS", "option:build:ldflags", PRTE_BUILD_LDFLAGS);
        prte_info_out("Build LIBS", "option:build:libs", PRTE_BUILD_LIBS);

    }

    prte_info_out("Internal debug support", "option:debug", debug);
    prte_info_out("dl support", "option:dlopen", have_dl);
    prte_info_out("prun default --prefix", "prun:prefix_by_default", prun_prefix_by_default);
    prte_info_out("Symbol vis. support", "options:visibility", symbol_visibility);
    prte_info_out("Manpages built", "options:man-pages", manpages);
}
