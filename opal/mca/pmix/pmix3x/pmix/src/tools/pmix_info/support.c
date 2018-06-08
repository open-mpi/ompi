/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2016 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011-2012 University of Houston. All rights reserved.
 * Copyright (c) 2016-2018 Intel, Inc. All rights reserved.
 * Copyright (c) 2017      IBM Corporation.  All rights reserved.
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "pmix_config.h"

#include <string.h>
#include <ctype.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <errno.h>

#include "src/class/pmix_list.h"
#include "src/class/pmix_pointer_array.h"
#include "src/runtime/pmix_rte.h"
#include "src/util/output.h"
#include "src/util/cmd_line.h"
#include "src/util/error.h"
#include "src/util/argv.h"
#include "src/util/show_help.h"

#include "src/include/frameworks.h"
#include "src/include/pmix_portable_platform.h"

#include "src/mca/pinstalldirs/pinstalldirs.h"
#include "pinfo.h"
#include "support.h"
#include "src/mca/base/pmix_mca_base_component_repository.h"

const char *pmix_info_path_prefix = "prefix";
const char *pmix_info_path_bindir = "bindir";
const char *pmix_info_path_libdir = "libdir";
const char *pmix_info_path_incdir = "incdir";
const char *pmix_info_path_mandir = "mandir";
const char *pmix_info_path_pkglibdir = "pkglibdir";
const char *pmix_info_path_sysconfdir = "sysconfdir";
const char *pmix_info_path_exec_prefix = "exec_prefix";
const char *pmix_info_path_sbindir = "sbindir";
const char *pmix_info_path_libexecdir = "libexecdir";
const char *pmix_info_path_datarootdir = "datarootdir";
const char *pmix_info_path_datadir = "datadir";
const char *pmix_info_path_sharedstatedir = "sharedstatedir";
const char *pmix_info_path_localstatedir = "localstatedir";
const char *pmix_info_path_infodir = "infodir";
const char *pmix_info_path_pkgdatadir = "pkgdatadir";
const char *pmix_info_path_pkgincludedir = "pkgincludedir";

bool pmix_info_pretty = true;
pmix_mca_base_register_flag_t pmix_info_register_flags = PMIX_MCA_BASE_REGISTER_ALL;

const char *pmix_info_type_all = "all";
const char *pmix_info_type_pmix = "pmix";
const char *pmix_info_component_all = "all";
const char *pmix_info_param_all = "all";

const char *pmix_info_ver_full = "full";
const char *pmix_info_ver_major = "major";
const char *pmix_info_ver_minor = "minor";
const char *pmix_info_ver_release = "release";
const char *pmix_info_ver_greek = "greek";
const char *pmix_info_ver_repo = "repo";

const char *pmix_info_ver_all = "all";
const char *pmix_info_ver_mca = "mca";
const char *pmix_info_ver_type = "type";
const char *pmix_info_ver_component = "component";

static int pmix_info_registered = 0;

static void component_map_construct(pmix_info_component_map_t *map)
{
    map->type = NULL;
}
static void component_map_destruct(pmix_info_component_map_t *map)
{
    if (NULL != map->type) {
        free(map->type);
    }
    /* the type close functions will release the
     * list of components
     */
}
PMIX_CLASS_INSTANCE(pmix_info_component_map_t,
                    pmix_list_item_t,
                    component_map_construct,
                    component_map_destruct);

static void pmix_info_show_failed_component(const pmix_mca_base_component_repository_item_t* ri,
                                            const char *error_msg);

int pmix_info_init(int argc, char **argv,
                   pmix_cmd_line_t *pmix_info_cmd_line)
{
    int ret;
    bool want_help = false;
    bool cmd_error = false;
    char **app_env = NULL, **global_env = NULL;

    /* add the cmd line options */
    pmix_cmd_line_make_opt3(pmix_info_cmd_line, 'V', NULL, "version", 0,
                            "Show version of Open MPI");
    pmix_cmd_line_make_opt3(pmix_info_cmd_line, '\0', NULL, "param", 2,
                            "Show MCA parameters.  The first parameter is the framework (or the keyword \"all\"); the second parameter is the specific component name (or the keyword \"all\").");
    pmix_cmd_line_make_opt3(pmix_info_cmd_line, '\0', NULL, "params", 2,
                            "Synonym for --param");
    pmix_cmd_line_make_opt3(pmix_info_cmd_line, '\0', NULL, "internal", 0,
                            "Show internal MCA parameters (not meant to be modified by users)");
    pmix_cmd_line_make_opt3(pmix_info_cmd_line, '\0', NULL, "path", 1,
                            "Show paths that Open MPI was configured with.  Accepts the following parameters: prefix, bindir, libdir, incdir, mandir, pkglibdir, sysconfdir, all");
    pmix_cmd_line_make_opt3(pmix_info_cmd_line, '\0', NULL, "arch", 0,
                            "Show architecture Open MPI was compiled on");
    pmix_cmd_line_make_opt3(pmix_info_cmd_line, 'c', NULL, "config", 0,
                            "Show configuration options");
    pmix_cmd_line_make_opt3(pmix_info_cmd_line, 't', NULL, "type", 1,
                            "Show internal MCA parameters with the type specified in parameter.");
    pmix_cmd_line_make_opt3(pmix_info_cmd_line, 'h', NULL, "help", 0,
                            "Show this help message");
    pmix_cmd_line_make_opt3(pmix_info_cmd_line, '\0', NULL, "pretty-print", 0,
                            "When used in conjunction with other parameters, the output is displayed in 'pretty-print' format (default)");
    pmix_cmd_line_make_opt3(pmix_info_cmd_line, '\0', NULL, "parsable", 0,
                            "When used in conjunction with other parameters, the output is displayed in a machine-parsable format");
    pmix_cmd_line_make_opt3(pmix_info_cmd_line, '\0', NULL, "parseable", 0,
                            "Synonym for --parsable");
    pmix_cmd_line_make_opt3(pmix_info_cmd_line, '\0', NULL, "hostname", 0,
                            "Show the hostname that Open MPI was configured and built on");
    pmix_cmd_line_make_opt3(pmix_info_cmd_line, 'a', NULL, "all", 0,
                            "Show all configuration options and MCA parameters");
    pmix_cmd_line_make_opt3(pmix_info_cmd_line, 'l', NULL, "level", 1,
                            "Show only variables with at most this level (1-9)");
    pmix_cmd_line_make_opt3(pmix_info_cmd_line, 's', NULL, "selected-only", 0,
                            "Show only variables from selected components");
    pmix_cmd_line_make_opt3(pmix_info_cmd_line, '\0', NULL, "show-failed", 0,
                            "Show the components that failed to load along with the reason why they failed.");

    if( PMIX_SUCCESS != pmix_mca_base_open() ) {
        pmix_show_help("help-pinfo.txt", "lib-call-fail", true, "mca_base_open", __FILE__, __LINE__ );
        PMIX_RELEASE(pmix_info_cmd_line);
        exit(1);
    }
    pmix_mca_base_cmd_line_setup(pmix_info_cmd_line);

    /* Do the parsing */

    ret = pmix_cmd_line_parse(pmix_info_cmd_line, false, false, argc, argv);
    if (PMIX_SUCCESS != ret) {
        if (PMIX_ERR_SILENT != ret) {
            fprintf(stderr, "%s: command line error (%s)\n", argv[0],
                    PMIx_Error_string(ret));
        }
        cmd_error = true;
    }
    if (!cmd_error &&
        (pmix_cmd_line_is_taken(pmix_info_cmd_line, "help") ||
         pmix_cmd_line_is_taken(pmix_info_cmd_line, "h"))) {
        char *str, *usage;

        want_help = true;
        usage = pmix_cmd_line_get_usage_msg(pmix_info_cmd_line);
        str = pmix_show_help_string("help-pmix-info.txt", "usage", true,
                                    usage);
        if (NULL != str) {
            printf("%s", str);
            free(str);
        }
        free(usage);
    }


    /* If we had a cmd line parse error, or we showed the help
       message, it's time to exit. */
    if (cmd_error || want_help) {
        pmix_mca_base_close();
        PMIX_RELEASE(pmix_info_cmd_line);
        exit(cmd_error ? 1 : 0);
    }

    pmix_mca_base_cmd_line_process_args(pmix_info_cmd_line, &app_env, &global_env);


    /* set the flags */
    if (pmix_cmd_line_is_taken(pmix_info_cmd_line, "pretty-print")) {
        pmix_info_pretty = true;
    } else if (pmix_cmd_line_is_taken(pmix_info_cmd_line, "parsable") || pmix_cmd_line_is_taken(pmix_info_cmd_line, "parseable")) {
        pmix_info_pretty = false;
    }

    if (pmix_cmd_line_is_taken(pmix_info_cmd_line, "selected-only")) {
        /* register only selected components */
        pmix_info_register_flags = PMIX_MCA_BASE_REGISTER_DEFAULT;
    }

    if (pmix_cmd_line_is_taken(pmix_info_cmd_line, "show-failed")) {
        pmix_mca_base_component_track_load_errors = true;
    }

    return PMIX_SUCCESS;
}

void pmix_info_finalize(void)
{
    pmix_mca_base_close();
}

static int info_register_framework (pmix_mca_base_framework_t *framework, pmix_pointer_array_t *component_map)
{
    pmix_info_component_map_t *map;
    int rc;
    rc = pmix_mca_base_framework_register(framework, pmix_info_register_flags);
    if (PMIX_SUCCESS != rc && PMIX_ERR_BAD_PARAM != rc) {
        return rc;
    }

    if (NULL != component_map) {
        map = PMIX_NEW(pmix_info_component_map_t);
        map->type = strdup(framework->framework_name);
        map->components = &framework->framework_components;
        map->failed_components = &framework->framework_failed_components;
        pmix_pointer_array_add(component_map, map);
    }

    return rc;
}

int pmix_info_register_project_frameworks (const char *project_name, pmix_mca_base_framework_t **frameworks,
                                           pmix_pointer_array_t *component_map)
{
    int i, rc=PMIX_SUCCESS;

    for (i=0; NULL != frameworks[i]; i++) {
        if (PMIX_SUCCESS != (rc = info_register_framework(frameworks[i], component_map))) {
            if (PMIX_ERR_BAD_PARAM == rc) {
                fprintf(stderr, "\nA \"bad parameter\" error was encountered when opening the %s %s framework\n",
                        project_name, frameworks[i]->framework_name);
                fprintf(stderr, "The output received from that framework includes the following parameters:\n\n");
            } else if (PMIX_ERR_NOT_AVAILABLE != rc) {
                fprintf(stderr, "%s_info_register: %s failed\n", project_name, frameworks[i]->framework_name);
                rc = PMIX_ERROR;
            } else {
                continue;
            }

            break;
        }
    }

    return rc;
}

void pmix_info_register_types(pmix_pointer_array_t *mca_types)
{
    int i;

    /* add the top-level types */
    pmix_pointer_array_add(mca_types, "mca");
    pmix_pointer_array_add(mca_types, "pmix");

    /* push all the types found by autogen */
    for (i=0; NULL != pmix_frameworks[i]; i++) {
        pmix_pointer_array_add(mca_types, pmix_frameworks[i]->framework_name);
    }
}

int pmix_info_register_framework_params(pmix_pointer_array_t *component_map)
{
    int rc;

    if (pmix_info_registered++) {
        return PMIX_SUCCESS;
    }

    /* Register mca/base parameters */
    if( PMIX_SUCCESS != pmix_mca_base_open() ) {
        pmix_show_help("help-pmix_info.txt", "lib-call-fail", true, "mca_base_open", __FILE__, __LINE__ );
        return PMIX_ERROR;
    }

    /* Register the PMIX layer's MCA parameters */
    if (PMIX_SUCCESS != (rc = pmix_register_params())) {
        fprintf(stderr, "pmix_info_register: pmix_register_params failed\n");
        return rc;
    }

    return pmix_info_register_project_frameworks("pmix", pmix_frameworks, component_map);
}


void pmix_info_close_components(void)
{
    int i;

    assert(pmix_info_registered);
    if (--pmix_info_registered) {
        return;
    }

    for (i=0; NULL != pmix_frameworks[i]; i++) {
        (void) pmix_mca_base_framework_close(pmix_frameworks[i]);
    }

    /* release our reference to MCA */
    pmix_mca_base_close ();
}


void pmix_info_show_path(const char *type, const char *value)
{
    char *pretty, *path;

    pretty = strdup(type);
    pretty[0] = toupper(pretty[0]);

    if (0 > asprintf(&path, "path:%s", type)) {
        free(pretty);
        return;
    }
    pmix_info_out(pretty, path, value);
    free(pretty);
    free(path);
}

void pmix_info_do_path(bool want_all, pmix_cmd_line_t *cmd_line)
{
    int i, count;
    char *scope;

    /* Check bozo case */
    count = pmix_cmd_line_get_ninsts(cmd_line, "path");
    for (i = 0; i < count; ++i) {
        scope = pmix_cmd_line_get_param(cmd_line, "path", i, 0);
        if (0 == strcmp("all", scope)) {
            want_all = true;
            break;
        }
    }

    if (want_all) {
        pmix_info_show_path(pmix_info_path_prefix, pmix_pinstall_dirs.prefix);
        pmix_info_show_path(pmix_info_path_exec_prefix, pmix_pinstall_dirs.exec_prefix);
        pmix_info_show_path(pmix_info_path_bindir, pmix_pinstall_dirs.bindir);
        pmix_info_show_path(pmix_info_path_sbindir, pmix_pinstall_dirs.sbindir);
        pmix_info_show_path(pmix_info_path_libdir, pmix_pinstall_dirs.libdir);
        pmix_info_show_path(pmix_info_path_incdir, pmix_pinstall_dirs.includedir);
        pmix_info_show_path(pmix_info_path_mandir, pmix_pinstall_dirs.mandir);
        pmix_info_show_path(pmix_info_path_pkglibdir, pmix_pinstall_dirs.pmixlibdir);
        pmix_info_show_path(pmix_info_path_libexecdir, pmix_pinstall_dirs.libexecdir);
        pmix_info_show_path(pmix_info_path_datarootdir, pmix_pinstall_dirs.datarootdir);
        pmix_info_show_path(pmix_info_path_datadir, pmix_pinstall_dirs.datadir);
        pmix_info_show_path(pmix_info_path_sysconfdir, pmix_pinstall_dirs.sysconfdir);
        pmix_info_show_path(pmix_info_path_sharedstatedir, pmix_pinstall_dirs.sharedstatedir);
        pmix_info_show_path(pmix_info_path_localstatedir, pmix_pinstall_dirs.localstatedir);
        pmix_info_show_path(pmix_info_path_infodir, pmix_pinstall_dirs.infodir);
        pmix_info_show_path(pmix_info_path_pkgdatadir, pmix_pinstall_dirs.pmixdatadir);
        pmix_info_show_path(pmix_info_path_pkglibdir, pmix_pinstall_dirs.pmixlibdir);
        pmix_info_show_path(pmix_info_path_pkgincludedir, pmix_pinstall_dirs.pmixincludedir);
    } else {
        count = pmix_cmd_line_get_ninsts(cmd_line, "path");
        for (i = 0; i < count; ++i) {
            scope = pmix_cmd_line_get_param(cmd_line, "path", i, 0);

            if (0 == strcmp(pmix_info_path_prefix, scope)) {
                pmix_info_show_path(pmix_info_path_prefix, pmix_pinstall_dirs.prefix);
            } else if (0 == strcmp(pmix_info_path_bindir, scope)) {
                pmix_info_show_path(pmix_info_path_bindir, pmix_pinstall_dirs.bindir);
            } else if (0 == strcmp(pmix_info_path_libdir, scope)) {
                pmix_info_show_path(pmix_info_path_libdir, pmix_pinstall_dirs.libdir);
            } else if (0 == strcmp(pmix_info_path_incdir, scope)) {
                pmix_info_show_path(pmix_info_path_incdir, pmix_pinstall_dirs.includedir);
            } else if (0 == strcmp(pmix_info_path_mandir, scope)) {
                pmix_info_show_path(pmix_info_path_mandir, pmix_pinstall_dirs.mandir);
            } else if (0 == strcmp(pmix_info_path_pkglibdir, scope)) {
                pmix_info_show_path(pmix_info_path_pkglibdir, pmix_pinstall_dirs.pmixlibdir);
            } else if (0 == strcmp(pmix_info_path_sysconfdir, scope)) {
                pmix_info_show_path(pmix_info_path_sysconfdir, pmix_pinstall_dirs.sysconfdir);
            } else if (0 == strcmp(pmix_info_path_exec_prefix, scope)) {
                pmix_info_show_path(pmix_info_path_exec_prefix, pmix_pinstall_dirs.exec_prefix);
            } else if (0 == strcmp(pmix_info_path_sbindir, scope)) {
                pmix_info_show_path(pmix_info_path_sbindir, pmix_pinstall_dirs.sbindir);
            } else if (0 == strcmp(pmix_info_path_libexecdir, scope)) {
                pmix_info_show_path(pmix_info_path_libexecdir, pmix_pinstall_dirs.libexecdir);
            } else if (0 == strcmp(pmix_info_path_datarootdir, scope)) {
                pmix_info_show_path(pmix_info_path_datarootdir, pmix_pinstall_dirs.datarootdir);
            } else if (0 == strcmp(pmix_info_path_datadir, scope)) {
                pmix_info_show_path(pmix_info_path_datadir, pmix_pinstall_dirs.datadir);
            } else if (0 == strcmp(pmix_info_path_sharedstatedir, scope)) {
                pmix_info_show_path(pmix_info_path_sharedstatedir, pmix_pinstall_dirs.sharedstatedir);
            } else if (0 == strcmp(pmix_info_path_localstatedir, scope)) {
                pmix_info_show_path(pmix_info_path_localstatedir, pmix_pinstall_dirs.localstatedir);
            } else if (0 == strcmp(pmix_info_path_infodir, scope)) {
                pmix_info_show_path(pmix_info_path_infodir, pmix_pinstall_dirs.infodir);
            } else if (0 == strcmp(pmix_info_path_pkgdatadir, scope)) {
                pmix_info_show_path(pmix_info_path_pkgdatadir, pmix_pinstall_dirs.pmixdatadir);
            } else if (0 == strcmp(pmix_info_path_pkgincludedir, scope)) {
                pmix_info_show_path(pmix_info_path_pkgincludedir, pmix_pinstall_dirs.pmixincludedir);
            } else {
                char *usage = pmix_cmd_line_get_usage_msg(cmd_line);
                pmix_show_help("help-pmix_info.txt", "usage", true, usage);
                free(usage);
                exit(1);
            }
        }
    }
}

void pmix_info_do_params(bool want_all_in, bool want_internal,
                         pmix_pointer_array_t *mca_types,
                         pmix_pointer_array_t *component_map,
                         pmix_cmd_line_t *pmix_info_cmd_line)
{
    pmix_mca_base_var_info_lvl_t max_level = PMIX_INFO_LVL_1;
    int count;
    char *type, *component, *str;
    bool found;
    int i;
    bool want_all = false;
    char *p;

    if (pmix_cmd_line_is_taken(pmix_info_cmd_line, "param")) {
        p = "param";
    } else if (pmix_cmd_line_is_taken(pmix_info_cmd_line, "params")) {
        p = "params";
    } else {
        p = "foo";  /* should never happen, but protect against segfault */
    }

    if (NULL != (str = pmix_cmd_line_get_param (pmix_info_cmd_line, "level", 0, 0))) {
        char *tmp;

        errno = 0;
        max_level = strtol (str, &tmp, 10) + PMIX_INFO_LVL_1 - 1;
        if (0 != errno || '\0' != tmp[0] || max_level < PMIX_INFO_LVL_1 || max_level > PMIX_INFO_LVL_9) {
            char *usage = pmix_cmd_line_get_usage_msg(pmix_info_cmd_line);
            pmix_show_help("help-pmix_info.txt", "invalid-level", true, str);
            free(usage);
            exit(1);
        }
    } else if (want_all_in) {
        /* if not specified default to level 9 if all components are requested */
        max_level = PMIX_INFO_LVL_9;
    }

    if (want_all_in) {
        want_all = true;
    } else {
        /* See if the special param "all" was given to --param; that
         * supercedes any individual type
         */
        count = pmix_cmd_line_get_ninsts(pmix_info_cmd_line, p);
        for (i = 0; i < count; ++i) {
            type = pmix_cmd_line_get_param(pmix_info_cmd_line, p, (int)i, 0);
            if (0 == strcmp(pmix_info_type_all, type)) {
                want_all = true;
                break;
            }
        }
    }

    /* Show the params */

    if (want_all) {
        pmix_info_show_component_version(mca_types, component_map, pmix_info_type_all,
                                         pmix_info_component_all, pmix_info_ver_full,
                                         pmix_info_ver_all);
        for (i = 0; i < mca_types->size; ++i) {
            if (NULL == (type = (char *)pmix_pointer_array_get_item(mca_types, i))) {
                continue;
            }
            pmix_info_show_mca_params(type, pmix_info_component_all, max_level, want_internal);
        }
    } else {
        for (i = 0; i < count; ++i) {
            type = pmix_cmd_line_get_param(pmix_info_cmd_line, p, (int)i, 0);
            component = pmix_cmd_line_get_param(pmix_info_cmd_line, p, (int)i, 1);

            for (found = false, i = 0; i < mca_types->size; ++i) {
                if (NULL == (str = (char *)pmix_pointer_array_get_item(mca_types, i))) {
                    continue;
                }
                if (0 == strcmp(str, type)) {
                    found = true;
                    break;
                }
            }

            if (!found) {
                char *usage = pmix_cmd_line_get_usage_msg(pmix_info_cmd_line);
                pmix_show_help("help-pmix_info.txt", "not-found", true, type);
                free(usage);
                exit(1);
            }

            pmix_info_show_component_version(mca_types, component_map, type,
                                             component, pmix_info_ver_full,
                                             pmix_info_ver_all);
            pmix_info_show_mca_params(type, component, max_level, want_internal);
        }
    }
}

void pmix_info_err_params(pmix_pointer_array_t *component_map)
{
    pmix_info_component_map_t *map=NULL, *mptr;
    int i;

    /* all we want to do is display the LAST entry in the
     * component_map array as this is the one that generated the error
     */
    for (i=0; i < component_map->size; i++) {
        if (NULL == (mptr = (pmix_info_component_map_t*)pmix_pointer_array_get_item(component_map, i))) {
            continue;
        }
        map = mptr;
    }
    if (NULL == map) {
        fprintf(stderr, "pmix_info_err_params: map not found\n");
        return;
    }
    pmix_info_show_mca_params(map->type, pmix_info_component_all, PMIX_INFO_LVL_9, true);
    fprintf(stderr, "\n");
    return;
}

void pmix_info_do_type(pmix_cmd_line_t *pmix_info_cmd_line)
{
    pmix_mca_base_var_info_lvl_t max_level = PMIX_INFO_LVL_1;
    int count;
    char *type, *str;
    int i, j, k, len, ret;
    char *p;
    const pmix_mca_base_var_t *var;
    char** strings, *message;
    const pmix_mca_base_var_group_t *group;
    p = "type";

    if (NULL != (str = pmix_cmd_line_get_param (pmix_info_cmd_line, "level", 0, 0))) {
        char *tmp;

        errno = 0;
        max_level = strtol (str, &tmp, 10) + PMIX_INFO_LVL_1 - 1;
        if (0 != errno || '\0' != tmp[0] || max_level < PMIX_INFO_LVL_1 || max_level > PMIX_INFO_LVL_9) {
            char *usage = pmix_cmd_line_get_usage_msg(pmix_info_cmd_line);
            pmix_show_help("help-pmix_info.txt", "invalid-level", true, str);
            free(usage);
            exit(1);
        }
    }

    count = pmix_cmd_line_get_ninsts(pmix_info_cmd_line, p);
    len = pmix_mca_base_var_get_count ();

    for (k = 0; k < count; ++k) {
        type = pmix_cmd_line_get_param(pmix_info_cmd_line, p, k, 0);
        for (i = 0; i < len; ++i) {
            ret = pmix_mca_base_var_get (i, &var);
            if (PMIX_SUCCESS != ret) {
                continue;
            }
            if (0 == strcmp(type, pmix_var_type_names[var->mbv_type]) && (var->mbv_info_lvl <= max_level)) {
                ret = pmix_mca_base_var_dump(var->mbv_index, &strings, !pmix_info_pretty ? PMIX_MCA_BASE_VAR_DUMP_PARSABLE : PMIX_MCA_BASE_VAR_DUMP_READABLE);
                if (PMIX_SUCCESS != ret) {
                    continue;
                }
                (void) pmix_mca_base_var_group_get(var->mbv_group_index, &group);
                for (j = 0 ; strings[j] ; ++j) {
                    if (0 == j && pmix_info_pretty) {
                        if (0 > asprintf (&message, "MCA %s", group->group_framework)) {
                            continue;
                        }
                        pmix_info_out(message, message, strings[j]);
                        free(message);
                    } else {
                        pmix_info_out("", "", strings[j]);
                    }
                    free(strings[j]);
                }
                free(strings);
            }
        }
    }
}

static void pmix_info_show_mca_group_params(const pmix_mca_base_var_group_t *group, pmix_mca_base_var_info_lvl_t max_level, bool want_internal)
{
    const int *variables, *groups;
    const char *group_component;
    const pmix_mca_base_var_t *var;
    char **strings, *message;
    bool requested = true;
    int ret, i, j, count;

    variables = PMIX_VALUE_ARRAY_GET_BASE(&group->group_vars, const int);
    count = pmix_value_array_get_size((pmix_value_array_t *)&group->group_vars);

    /* the default component name is "base". depending on how the
     * group was registered the group may or not have this set.  */
    group_component = group->group_component ? group->group_component : "base";

    /* check if this group may be disabled due to a selection variable */
    if (0 != strcmp (group_component, "base")) {
        int var_id;

        /* read the selection parameter */
        var_id = pmix_mca_base_var_find (group->group_project, group->group_framework, NULL, NULL);
        if (0 <= var_id) {
            const pmix_mca_base_var_storage_t *value=NULL;
            char **requested_components;
            bool include_mode;

            pmix_mca_base_var_get_value (var_id, &value, NULL, NULL);
            if (NULL != value && NULL != value->stringval && '\0' != value->stringval[0]) {
                pmix_mca_base_component_parse_requested (value->stringval, &include_mode, &requested_components);

                for (i = 0, requested = !include_mode ; requested_components[i] ; ++i) {
                    if (0 == strcmp (requested_components[i], group_component)) {
                        requested = include_mode;
                        break;
                    }
                }

                pmix_argv_free (requested_components);
            }
        }
    }

    const pmix_mca_base_var_group_t *curr_group = NULL;
    char *component_msg = NULL;
    if (0 > asprintf(&component_msg, " %s", group_component)) {
        return;
    }

    for (i = 0 ; i < count ; ++i) {
        ret = pmix_mca_base_var_get(variables[i], &var);
        if (PMIX_SUCCESS != ret || ((var->mbv_flags & PMIX_MCA_BASE_VAR_FLAG_INTERNAL) &&
                                    !want_internal) ||
            max_level < var->mbv_info_lvl) {
            continue;
        }

        if (pmix_info_pretty && curr_group != group) {
            if (0 > asprintf(&message, "MCA%s %s%s", requested ? "" : " (-)",
                             group->group_framework,
                             component_msg ? component_msg : "")) {
                continue;
            }
            pmix_info_out(message, message, "---------------------------------------------------");
            free(message);
            curr_group = group;
        }

        ret = pmix_mca_base_var_dump(variables[i], &strings, !pmix_info_pretty ? PMIX_MCA_BASE_VAR_DUMP_PARSABLE : PMIX_MCA_BASE_VAR_DUMP_READABLE);
        if (PMIX_SUCCESS != ret) {
            continue;
        }

        for (j = 0 ; strings[j] ; ++j) {
            if (0 == j && pmix_info_pretty) {
                if (0 > asprintf (&message, "MCA%s %s%s", requested ? "" : " (-)",
                                  group->group_framework,
                                  component_msg ? component_msg : "")) {
                    continue;
                }
                pmix_info_out(message, message, strings[j]);
                free(message);
            } else {
                pmix_info_out("", "", strings[j]);
            }
            free(strings[j]);
        }
        if (!pmix_info_pretty) {
            /* generate an entry indicating whether this variable is disabled or not. if the
             * format in mca_base_var/pvar.c changes this needs to be changed as well */
            if (0 > asprintf (&message, "mca:%s:%s:param:%s:disabled:%s", group->group_framework,
                              group_component, var->mbv_full_name, requested ? "false" : "true")) {
                continue;
            }
            pmix_info_out("", "", message);
            free (message);
        }
        free(strings);
    }

    groups = PMIX_VALUE_ARRAY_GET_BASE(&group->group_subgroups, const int);
    count = pmix_value_array_get_size((pmix_value_array_t *)&group->group_subgroups);

    for (i = 0 ; i < count ; ++i) {
        ret = pmix_mca_base_var_group_get(groups[i], &group);
        if (PMIX_SUCCESS != ret) {
            continue;
        }
        pmix_info_show_mca_group_params(group, max_level, want_internal);
    }
    free(component_msg);
}

void pmix_info_show_mca_params(const char *type, const char *component,
                               pmix_mca_base_var_info_lvl_t max_level, bool want_internal)
{
    const pmix_mca_base_var_group_t *group;
    int ret;

    if (0 == strcmp (component, "all")) {
        ret = pmix_mca_base_var_group_find("*", type, NULL);
        if (0 > ret) {
            return;
        }

        (void) pmix_mca_base_var_group_get(ret, &group);

        pmix_info_show_mca_group_params(group, max_level, want_internal);
    } else {
        ret = pmix_mca_base_var_group_find("*", type, component);
        if (0 > ret) {
            return;
        }

        (void) pmix_mca_base_var_group_get(ret, &group);
        pmix_info_show_mca_group_params(group, max_level, want_internal);
    }
}



void pmix_info_do_arch()
{
    pmix_info_out("Configured architecture", "config:arch", PMIX_ARCH);
}


void pmix_info_do_hostname()
{
    pmix_info_out("Configure host", "config:host", PMIX_CONFIGURE_HOST);
}


static char *escape_quotes(const char *value)
{
    const char *src;
    int num_quotes = 0;
    for (src = value; src != NULL && *src != '\0'; ++src) {
        if ('"' == *src) {
            ++num_quotes;
        }
    }

    // If there are no quotes in the string, there's nothing to do
    if (0 == num_quotes) {
        return NULL;
    }

    // If we have quotes, make a new string.  Copy over the old
    // string, escaping the quotes along the way.  This is simple and
    // clear to read; it's not particularly efficient (performance is
    // definitely not important here).
    char *quoted_value;
    quoted_value = calloc(1, strlen(value) + num_quotes + 1);
    if (NULL == quoted_value) {
        return NULL;
    }

    char *dest;
    for (src = value, dest = quoted_value; *src != '\0'; ++src, ++dest) {
        if ('"' == *src) {
            *dest++ = '\\';
        }
        *dest = *src;
    }

    return quoted_value;
}


/*
 * Private variables - set some reasonable screen size defaults
 */

static int centerpoint = 24;
static int screen_width = 78;

/*
 * Prints the passed message in a pretty or parsable format.
 */
void pmix_info_out(const char *pretty_message, const char *plain_message, const char *value)
{
    size_t len, max_value_width, value_offset;
    char *spaces = NULL;
    char *filler = NULL;
    char *pos, *v, savev, *v_to_free;

#ifdef HAVE_ISATTY
    /* If we have isatty(), if this is not a tty, then disable
     * wrapping for grep-friendly behavior
     */
    if (0 == isatty(STDOUT_FILENO)) {
        screen_width = INT_MAX;
    }
#endif

#ifdef TIOCGWINSZ
    if (screen_width < INT_MAX) {
        struct winsize size;
        if (ioctl(STDOUT_FILENO, TIOCGWINSZ, (char*) &size) >= 0) {
            screen_width = size.ws_col;
        }
    }
#endif

    /* Sanity check (allow NULL to mean "") */
    if (NULL == value) {
        value = "";
    }

    /* Strip leading and trailing whitespace from the string value */
    value_offset = strspn(value, " ");

    v = v_to_free = strdup(value + value_offset);
    len = strlen(v);

    if (len > 0) {
        while (len > 0 && isspace(v[len-1])) len--;
        v[len] = '\0';
    }

    if (pmix_info_pretty && NULL != pretty_message) {
        if (centerpoint > (int)strlen(pretty_message)) {
            if (0 > asprintf(&spaces, "%*s", centerpoint -
                             (int)strlen(pretty_message), " ")) {
                if (NULL != v_to_free) {
                    free(v_to_free);
                }
                return;
            }
        } else {
            spaces = strdup("");
#if PMIX_ENABLE_DEBUG
            if (centerpoint < (int)strlen(pretty_message)) {
                pmix_show_help("help-pmix_info.txt",
                               "developer warning: field too long", false,
                               pretty_message, centerpoint);
            }
#endif
        }
        max_value_width = screen_width - strlen(spaces) - strlen(pretty_message) - 2;
        if (0 < strlen(pretty_message)) {
            if (0 > asprintf(&filler, "%s%s: ", spaces, pretty_message)) {
                if (NULL != v_to_free) {
                    free(v_to_free);
                }
                return;
            }
        } else {
            if (0 > asprintf(&filler, "%s  ", spaces)) {
                if (NULL != v_to_free) {
                    free(v_to_free);
                }
                return;
            }
        }
        free(spaces);
        spaces = NULL;

        while (true) {
            if (strlen(v) < max_value_width) {
                printf("%s%s\n", filler, v);
                break;
            } else {
                if (0 > asprintf(&spaces, "%*s", centerpoint + 2, " ")) {
                    if (NULL != v_to_free) {
                        free(v_to_free);
                    }
                    return;
                }

                /* Work backwards to find the first space before
                 * max_value_width
                 */
                savev = v[max_value_width];
                v[max_value_width] = '\0';
                pos = (char*)strrchr(v, (int)' ');
                v[max_value_width] = savev;
                if (NULL == pos) {
                    /* No space found < max_value_width.  Look for the first
                     * space after max_value_width.
                     */
                    pos = strchr(&v[max_value_width], ' ');

                    if (NULL == pos) {

                        /* There's just no spaces.  So just print it and be done. */

                        printf("%s%s\n", filler, v);
                        break;
                    } else {
                        *pos = '\0';
                        printf("%s%s\n", filler, v);
                        v = pos + 1;
                    }
                } else {
                    *pos = '\0';
                    printf("%s%s\n", filler, v);
                    v = pos + 1;
                }

                /* Reset for the next iteration */
                free(filler);
                filler = strdup(spaces);
                free(spaces);
                spaces = NULL;
            }
        }
        if (NULL != filler) {
            free(filler);
        }
        if (NULL != spaces) {
            free(spaces);
        }
    } else {
        if (NULL != plain_message && 0 < strlen(plain_message)) {
            // Escape any double quotes in the value.
            char *quoted_value;
            quoted_value = escape_quotes(value);
            if (NULL != quoted_value) {
                value = quoted_value;
            }

            char *colon = strchr(value, ':');
            if (NULL != colon) {
                printf("%s:\"%s\"\n", plain_message, value);
            } else {
                printf("%s:%s\n", plain_message, value);
            }

            if (NULL != quoted_value) {
                free(quoted_value);
            }
        } else {
            printf("%s\n", value);
        }
    }
    if (NULL != v_to_free) {
        free(v_to_free);
    }
}

/*
 * Prints the passed integer in a pretty or parsable format.
 */
void pmix_info_out_int(const char *pretty_message,
                       const char *plain_message,
                       int value)
{
    char *valstr;

    if (0 > asprintf(&valstr, "%d", (int)value)) {
        return;
    }
    pmix_info_out(pretty_message, plain_message, valstr);
    free(valstr);
}

/*
 * Show all the components of a specific type/component combo (component may be
 * a wildcard)
 */
void pmix_info_show_component_version(pmix_pointer_array_t *mca_types,
                                      pmix_pointer_array_t *component_map,
                                      const char *type_name,
                                      const char *component_name,
                                      const char *scope, const char *ver_type)
{
    bool want_all_components = false;
    bool want_all_types = false;
    bool found;
    pmix_mca_base_component_list_item_t *cli;
    pmix_mca_base_failed_component_t *cli_failed;
    int j;
    char *pos;
    pmix_info_component_map_t *map;

    /* see if all components wanted */
    if (0 == strcmp(pmix_info_component_all, component_name)) {
        want_all_components = true;
    }

    /* see if all types wanted */
    if (0 != strcmp(pmix_info_type_all, type_name)) {
        /* Check to see if the type is valid */

        for (found = false, j = 0; j < mca_types->size; ++j) {
            if (NULL == (pos = (char*)pmix_pointer_array_get_item(mca_types, j))) {
                continue;
            }
            if (0 == strcmp(pos, type_name)) {
                found = true;
                break;
            }
        }

        if (!found) {
            return;
        }
    } else {
        want_all_types = true;
    }

    /* Now that we have a valid type, find the right components */
    for (j=0; j < component_map->size; j++) {
        if (NULL == (map = (pmix_info_component_map_t*)pmix_pointer_array_get_item(component_map, j))) {
            continue;
        }
        if ((want_all_types || 0 == strcmp(type_name, map->type)) && map->components) {
            /* found it! */
            PMIX_LIST_FOREACH(cli, map->components, pmix_mca_base_component_list_item_t) {
                const pmix_mca_base_component_t *component = cli->cli_component;
                if (want_all_components ||
                    0 == strcmp(component->pmix_mca_component_name, component_name)) {
                    pmix_info_show_mca_version(component, scope, ver_type);
                }
            }

            /* found it! */
            PMIX_LIST_FOREACH(cli_failed, map->failed_components, pmix_mca_base_failed_component_t) {
                pmix_mca_base_component_repository_item_t *ri = cli_failed->comp;
                if (want_all_components ||
                    0 == strcmp(component_name, ri->ri_name) ) {
                    pmix_info_show_failed_component(ri, cli_failed->error_msg);
                }
            }

            if (!want_all_types) {
                break;
            }
        }
    }
}


static void pmix_info_show_failed_component(const pmix_mca_base_component_repository_item_t* ri,
                                            const char *error_msg)
{
    char *message, *content;

    if (pmix_info_pretty) {
        if (0 > asprintf(&message, "MCA %s", ri->ri_type)) {
            return;
        }
        if (0 > asprintf(&content, "%s (failed to load) %s", ri->ri_name, error_msg)) {
            free(message);
            return;
        }

        pmix_info_out(message, NULL, content);

        free(message);
        free(content);
    } else {
        if (0 > asprintf(&message, "mca:%s:%s:failed", ri->ri_type, ri->ri_name)) {
            return;
        }
        if (0 > asprintf(&content, "%s", error_msg)) {
            free(message);
            return;
        }

        pmix_info_out(NULL, message, content);

        free(message);
        free(content);
    }
}

/*
 * Given a component, display its relevant version(s)
 */
void pmix_info_show_mca_version(const pmix_mca_base_component_t* component,
                                const char *scope, const char *ver_type)
{
    bool printed;
    bool want_mca = false;
    bool want_type = false;
    bool want_component = false;
    char *message = NULL, *content = NULL;
    char *mca_version;
    char *api_version;
    char *component_version;
    char *tmp;

    if (0 == strcmp(ver_type, pmix_info_ver_all) ||
        0 == strcmp(ver_type, pmix_info_ver_mca)) {
        want_mca = true;
    }

    if (0 == strcmp(ver_type, pmix_info_ver_all) ||
        0 == strcmp(ver_type, pmix_info_ver_type)) {
        want_type = true;
    }

    if (0 == strcmp(ver_type, pmix_info_ver_all) ||
        0 == strcmp(ver_type, pmix_info_ver_component)) {
        want_component = true;
    }

    mca_version = pmix_info_make_version_str(scope, component->pmix_mca_major_version,
                                             component->pmix_mca_minor_version,
                                             component->pmix_mca_release_version, "",
                                             "");
    api_version = pmix_info_make_version_str(scope, component->pmix_mca_type_major_version,
                                             component->pmix_mca_type_minor_version,
                                             component->pmix_mca_type_release_version, "",
                                             "");
    component_version = pmix_info_make_version_str(scope, component->pmix_mca_component_major_version,
                                                   component->pmix_mca_component_minor_version,
                                                   component->pmix_mca_component_release_version,
                                                   "", "");
    if (pmix_info_pretty) {
        if (0 > asprintf(&message, "MCA %s", component->pmix_mca_type_name)) {
            goto exit;
        }
        printed = false;
        if (0 > asprintf(&content, "%s (", component->pmix_mca_component_name)) {
            goto exit;
        }

        if (want_mca) {
            if (0 > asprintf(&tmp, "%sMCA v%s", content, mca_version)) {
                goto exit;
            }
            content = tmp;
            printed = true;
        }

        if (want_type) {
            if (printed) {
                if (0 > asprintf(&tmp, "%s, ", content)) {
                    goto exit;
                }
                free(content);
                content = tmp;
            }
            if (0 > asprintf(&tmp, "%sAPI v%s", content, api_version)) {
                goto exit;
            }
            free(content);
            content = tmp;
            printed = true;
        }

        if (want_component) {
            if (printed) {
                if (0 > asprintf(&tmp, "%s, ", content)) {
                    goto exit;
                }
                free(content);
                content = tmp;
            }
            if (0 > asprintf(&tmp, "%sComponent v%s", content, component_version)) {
                goto exit;
            }
            free(content);
            content = tmp;
            printed = true;
        }
        if (NULL != content) {
            if (0 > asprintf(&tmp, "%s)", content)) {
                goto exit;
            }
        } else {
            tmp = NULL;
        }

        pmix_info_out(message, NULL, tmp);
        if (NULL != tmp) {
            free(tmp);
        }

    } else {
        if (0 > asprintf(&message, "mca:%s:%s:version", component->pmix_mca_type_name, component->pmix_mca_component_name)) {
            goto exit;
        }
        if (want_mca) {
            if (0 > asprintf(&tmp, "mca:%s", mca_version)) {
                goto exit;
            }
            pmix_info_out(NULL, message, tmp);
            free(tmp);
        }
        if (want_type) {
            if (0 > asprintf(&tmp, "api:%s", api_version)) {
                goto exit;
            }
            pmix_info_out(NULL, message, tmp);
            free(tmp);
        }
        if (want_component) {
            if (0 > asprintf(&tmp, "component:%s", component_version)) {
                goto exit;
            }
            pmix_info_out(NULL, message, tmp);
            free(tmp);
        }
    }

exit:
    if (NULL != mca_version) {
        free(mca_version);
    }
    if (NULL != api_version) {
        free(api_version);
    }
    if (NULL != component_version) {
        free(component_version);
    }
    if (NULL != message) {
        free(message);
    }
    if (NULL != content) {
        free(content);
    }
}


char *pmix_info_make_version_str(const char *scope,
                                 int major, int minor, int release,
                                 const char *greek,
                                 const char *repo)
{
    char *str = NULL, *tmp;
    char temp[BUFSIZ];

    temp[BUFSIZ - 1] = '\0';
    if (0 == strcmp(scope, pmix_info_ver_full) ||
        0 == strcmp(scope, pmix_info_ver_all)) {
        snprintf(temp, BUFSIZ - 1, "%d.%d.%d", major, minor, release);
        str = strdup(temp);
        if (NULL != greek) {
            if (0 > asprintf(&tmp, "%s%s", str, greek)) {
                free(str);
                return NULL;
            }
            free(str);
            str = tmp;
        }
    } else if (0 == strcmp(scope, pmix_info_ver_major)) {
        snprintf(temp, BUFSIZ - 1, "%d", major);
    } else if (0 == strcmp(scope, pmix_info_ver_minor)) {
        snprintf(temp, BUFSIZ - 1, "%d", minor);
    } else if (0 == strcmp(scope, pmix_info_ver_release)) {
        snprintf(temp, BUFSIZ - 1, "%d", release);
    } else if (0 == strcmp(scope, pmix_info_ver_greek)) {
        str = strdup(greek);
    } else if (0 == strcmp(scope, pmix_info_ver_repo)) {
        str = strdup(repo);
    }

    if (NULL == str) {
        str = strdup(temp);
    }

    return str;
}

void pmix_info_show_pmix_version(const char *scope)
{
    char *tmp, *tmp2;

    if (0 > asprintf(&tmp, "%s:version:full", pmix_info_type_pmix)) {
        return;
    }
    tmp2 = pmix_info_make_version_str(scope,
                                      PMIX_MAJOR_VERSION, PMIX_MINOR_VERSION,
                                      PMIX_RELEASE_VERSION,
                                      PMIX_GREEK_VERSION,
                                      PMIX_REPO_REV);
    pmix_info_out("PMIX", tmp, tmp2);
    free(tmp);
    free(tmp2);
    if (0 > asprintf(&tmp, "%s:version:repo", pmix_info_type_pmix)) {
        return;
    }
    pmix_info_out("PMIX repo revision", tmp, PMIX_REPO_REV);
    free(tmp);
    if (0 > asprintf(&tmp, "%s:version:release_date", pmix_info_type_pmix)) {
        return;
    }
    pmix_info_out("PMIX release date", tmp, PMIX_RELEASE_DATE);
    free(tmp);
}

/*
 * do_config
 * Accepts:
 *  - want_all: boolean flag; TRUE -> display all options
 *                FALSE -> display selected options
 *
 * This function displays all the options with which the current
 * installation of pmix was configured. There are many options here
 * that are carried forward from OMPI-7 and are not mca parameters
 * in OMPI-10. I have to dig through the invalid options and replace
 * them with OMPI-10 options.
 */
void pmix_info_do_config(bool want_all)
{
    char *debug;
    char *have_dl;
    char *symbol_visibility;

    /* setup the strings that don't require allocations*/
    debug = PMIX_ENABLE_DEBUG ? "yes" : "no";
    have_dl = PMIX_HAVE_PDL_SUPPORT ? "yes" : "no";
    symbol_visibility = PMIX_HAVE_VISIBILITY ? "yes" : "no";

    /* output values */
    pmix_info_out("Configured by", "config:user", PMIX_CONFIGURE_USER);
    pmix_info_out("Configured on", "config:timestamp", PMIX_CONFIGURE_DATE);
    pmix_info_out("Configure host", "config:host", PMIX_CONFIGURE_HOST);
    pmix_info_out("Configure command line", "config:cli", PMIX_CONFIGURE_CLI);

    pmix_info_out("Built by", "build:user", PMIX_BUILD_USER);
    pmix_info_out("Built on", "build:timestamp", PMIX_BUILD_DATE);
    pmix_info_out("Built host", "build:host", PMIX_BUILD_HOST);

    pmix_info_out("C compiler", "compiler:c:command", PMIX_CC);
    pmix_info_out("C compiler absolute", "compiler:c:absolute", PMIX_CC_ABSOLUTE);
    pmix_info_out("C compiler family name", "compiler:c:familyname", _STRINGIFY(PMIX_BUILD_PLATFORM_COMPILER_FAMILYNAME));
    pmix_info_out("C compiler version", "compiler:c:version", _STRINGIFY(PMIX_BUILD_PLATFORM_COMPILER_VERSION_STR));

    if (want_all) {
        pmix_info_out_int("C char size", "compiler:c:sizeof:char", sizeof(char));
        pmix_info_out_int("C bool size", "compiler:c:sizeof:bool", sizeof(bool));
        pmix_info_out_int("C short size", "compiler:c:sizeof:short", sizeof(short));
        pmix_info_out_int("C int size", "compiler:c:sizeof:int", sizeof(int));
        pmix_info_out_int("C long size", "compiler:c:sizeof:long", sizeof(long));
        pmix_info_out_int("C float size", "compiler:c:sizeof:float", sizeof(float));
        pmix_info_out_int("C double size", "compiler:c:sizeof:double", sizeof(double));
        pmix_info_out_int("C pointer size", "compiler:c:sizeof:pointer", sizeof(void *));
        pmix_info_out_int("C char align", "compiler:c:align:char", PMIX_ALIGNMENT_CHAR);
        pmix_info_out("C bool align", "compiler:c:align:bool", "skipped");
        pmix_info_out_int("C int align", "compiler:c:align:int", PMIX_ALIGNMENT_INT);
        pmix_info_out_int("C float align", "compiler:c:align:float", PMIX_ALIGNMENT_FLOAT);
        pmix_info_out_int("C double align", "compiler:c:align:double", PMIX_ALIGNMENT_DOUBLE);
    }

    if (want_all) {
        pmix_info_out("Build CFLAGS", "option:build:cflags", PMIX_BUILD_CFLAGS);
        pmix_info_out("Build LDFLAGS", "option:build:ldflags", PMIX_BUILD_LDFLAGS);
        pmix_info_out("Build LIBS", "option:build:libs", PMIX_BUILD_LIBS);
    }

    pmix_info_out("Internal debug support", "option:debug", debug);
    pmix_info_out("dl support", "option:dlopen", have_dl);
    pmix_info_out("Symbol vis. support", "options:visibility", symbol_visibility);
}
