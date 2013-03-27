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
 * Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2012 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2011-2012 University of Houston. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include <string.h>
#include <ctype.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/class/opal_list.h"
#include "opal/class/opal_pointer_array.h"

#include "opal/util/output.h"
#include "opal/util/cmd_line.h"
#include "opal/util/error.h"
#include "opal/util/argv.h"
#include "opal/util/show_help.h"
#include "opal/runtime/opal.h"
#include "opal/dss/dss.h"

#include "opal/include/opal/frameworks.h"

#include "opal/mca/installdirs/installdirs.h"

#include "opal/runtime/opal_info_support.h"

const char *opal_info_path_prefix = "prefix";
const char *opal_info_path_bindir = "bindir";
const char *opal_info_path_libdir = "libdir";
const char *opal_info_path_incdir = "incdir";
const char *opal_info_path_mandir = "mandir";
const char *opal_info_path_pkglibdir = "pkglibdir";
const char *opal_info_path_sysconfdir = "sysconfdir";
const char *opal_info_path_exec_prefix = "exec_prefix";
const char *opal_info_path_sbindir = "sbindir";
const char *opal_info_path_libexecdir = "libexecdir";
const char *opal_info_path_datarootdir = "datarootdir";
const char *opal_info_path_datadir = "datadir";
const char *opal_info_path_sharedstatedir = "sharedstatedir";
const char *opal_info_path_localstatedir = "localstatedir";
const char *opal_info_path_infodir = "infodir";
const char *opal_info_path_pkgdatadir = "pkgdatadir";
const char *opal_info_path_pkgincludedir = "pkgincludedir";

bool opal_info_pretty = true;

const char *opal_info_type_all = "all";
const char *opal_info_type_opal = "opal";
const char *opal_info_component_all = "all";
const char *opal_info_param_all = "all";

const char *opal_info_ver_full = "full";
const char *opal_info_ver_major = "major";
const char *opal_info_ver_minor = "minor";
const char *opal_info_ver_release = "release";
const char *opal_info_ver_greek = "greek";
const char *opal_info_ver_repo = "repo";

const char *opal_info_ver_all = "all";
const char *opal_info_ver_mca = "mca";
const char *opal_info_ver_type = "type";
const char *opal_info_ver_component = "component";


static void component_map_construct(opal_info_component_map_t *map)
{
    map->type = NULL;
}
static void component_map_destruct(opal_info_component_map_t *map)
{
    if (NULL != map->type) {
        free(map->type);
    }
    /* the type close functions will release the
     * list of components
     */
}
OBJ_CLASS_INSTANCE(opal_info_component_map_t,
                   opal_list_item_t,
                   component_map_construct,
                   component_map_destruct);

int opal_info_init(int argc, char **argv,
                   opal_cmd_line_t *opal_info_cmd_line)
{
    int ret;
    bool want_help = false;
    bool cmd_error = false;
    char **app_env = NULL, **global_env = NULL;

    /* Initialize the argv parsing handle */
    if (OPAL_SUCCESS != (ret = opal_init_util(&argc, &argv))) {
        opal_show_help("help-opal_info.txt", "lib-call-fail", true, 
                       "opal_init_util", __FILE__, __LINE__, NULL);
        exit(ret);
    }
    
    /* add the cmd line options */
    opal_cmd_line_make_opt3(opal_info_cmd_line, 'v', NULL, "version", 2, 
                            "Show version of Open MPI or a component.  The first parameter can be a keyword [\"ompi\" | \"orte\" | \"opal\" | \"all\"], a framework name (indicating all components in a framework), or a framework:component string (indicating a specific component).  The second parameter can be one of [full | major | minor | release | greek | svn]");
    opal_cmd_line_make_opt3(opal_info_cmd_line, '\0', NULL, "param", 2, 
                            "Show MCA parameters.  The first parameter is the framework (or the keyword \"all\"); the second parameter is the specific component name (or the keyword \"all\").");
    opal_cmd_line_make_opt3(opal_info_cmd_line, '\0', NULL, "params", 2, 
                            "Synonym for --param");
    opal_cmd_line_make_opt3(opal_info_cmd_line, '\0', NULL, "internal", 0, 
                            "Show internal MCA parameters (not meant to be modified by users)");
    opal_cmd_line_make_opt3(opal_info_cmd_line, '\0', NULL, "path", 1, 
                            "Show paths that Open MPI was configured with.  Accepts the following parameters: prefix, bindir, libdir, incdir, mandir, pkglibdir, sysconfdir, all");
    opal_cmd_line_make_opt3(opal_info_cmd_line, '\0', NULL, "arch", 0, 
                            "Show architecture Open MPI was compiled on");
    opal_cmd_line_make_opt3(opal_info_cmd_line, 'c', NULL, "config", 0, 
                            "Show configuration options");
    opal_cmd_line_make_opt3(opal_info_cmd_line, 'h', NULL, "help", 0, 
                            "Show this help message");
    opal_cmd_line_make_opt3(opal_info_cmd_line, '\0', NULL, "pretty-print", 0, 
                            "When used in conjunction with other parameters, the output is displayed in 'pretty-print' format (default)");
    opal_cmd_line_make_opt3(opal_info_cmd_line, '\0', NULL, "parsable", 0, 
                            "When used in conjunction with other parameters, the output is displayed in a machine-parsable format");
    opal_cmd_line_make_opt3(opal_info_cmd_line, '\0', NULL, "parseable", 0, 
                            "Synonym for --parsable");
    opal_cmd_line_make_opt3(opal_info_cmd_line, '\0', NULL, "hostname", 0, 
                            "Show the hostname that Open MPI was configured and built on");
    opal_cmd_line_make_opt3(opal_info_cmd_line, 'a', NULL, "all", 0, 
                            "Show all configuration options and MCA parameters");
    
    /* set our threading level */
    opal_set_using_threads(false);
    
    /* Get MCA parameters, if any */
    if( OPAL_SUCCESS != mca_base_open() ) {
        opal_show_help("help-opal_info.txt", "lib-call-fail", true, "mca_base_open", __FILE__, __LINE__ );
        opal_finalize_util();
        return OPAL_ERROR;
    }
    mca_base_cmd_line_setup(opal_info_cmd_line);
    
    /* Initialize the opal_output system */
    if (!opal_output_init()) {
        return OPAL_ERROR;
    }
    
    /* Do the parsing */
    ret = opal_cmd_line_parse(opal_info_cmd_line, false, argc, argv);
    if (OPAL_SUCCESS != ret) {
        cmd_error = true;
        if (OPAL_ERR_SILENT != ret) {
            fprintf(stderr, "%s: command line error (%s)\n", argv[0],
                    opal_strerror(ret));
        }
    }
    if (!cmd_error && 
        (opal_cmd_line_is_taken(opal_info_cmd_line, "help") || 
         opal_cmd_line_is_taken(opal_info_cmd_line, "h"))) {
        char *str, *usage;

        want_help = true;
        usage = opal_cmd_line_get_usage_msg(opal_info_cmd_line);
        str = opal_show_help_string("help-opal_info.txt", "usage", 
                                    true, usage);
        if (NULL != str) {
            printf("%s", str);
            free(str);
        }
        free(usage);
    }

    /* If we had a cmd line parse error, or we showed the help
       message, it's time to exit. */
    if (cmd_error || want_help) {
        mca_base_close();
        OBJ_RELEASE(opal_info_cmd_line);
        opal_finalize_util();
        exit(cmd_error ? 1 : 0);
    }
    
    mca_base_cmd_line_process_args(opal_info_cmd_line, &app_env, &global_env);
    

    /* set the flags */
    if (opal_cmd_line_is_taken(opal_info_cmd_line, "pretty-print")) {
        opal_info_pretty = true;
    } else if (opal_cmd_line_is_taken(opal_info_cmd_line, "parsable") || opal_cmd_line_is_taken(opal_info_cmd_line, "parseable")) {
        opal_info_pretty = false;
    }
    
    return OPAL_SUCCESS;
}

void opal_info_finalize(void)
{
    mca_base_close();
    opal_finalize_util();
}

static int info_register_framework (mca_base_framework_t *framework, opal_pointer_array_t *component_map) {
    opal_info_component_map_t *map;
    int rc;

    if ((OPAL_SUCCESS != (rc = mca_base_framework_register(framework, MCA_BASE_REGISTER_ALL))) ||
        OPAL_ERR_BAD_PARAM != rc) {
        return rc;
    }

    map = OBJ_NEW(opal_info_component_map_t);
    map->type = strdup(framework->framework_name);
    map->components = &framework->framework_components;
    opal_pointer_array_add(component_map, map);

    return rc;
}

void opal_info_register_types(opal_pointer_array_t *mca_types)
{
    opal_pointer_array_add(mca_types, "backtrace");
    opal_pointer_array_add(mca_types, "db");
#if OPAL_ENABLE_FT_CR == 1
    opal_cr_set_enabled(true);
    opal_pointer_array_add(mca_types, "compress");
    opal_pointer_array_add(mca_types, "crs");
#endif
    opal_pointer_array_add(mca_types, "event");
    opal_pointer_array_add(mca_types, "filter");
    opal_pointer_array_add(mca_types, "hwloc");
    opal_pointer_array_add(mca_types, "if");
    opal_pointer_array_add(mca_types, "installdirs");
    opal_pointer_array_add(mca_types, "mca");
    opal_pointer_array_add(mca_types, "memchecker");
    opal_pointer_array_add(mca_types, "memory");
    opal_pointer_array_add(mca_types, "memcpy");
    opal_pointer_array_add(mca_types, "opal");
    opal_pointer_array_add(mca_types, "shmem");
    opal_pointer_array_add(mca_types, "timer");

}

int opal_info_register_components(opal_pointer_array_t *mca_types,
                                  opal_pointer_array_t *component_map)
{
    opal_info_component_map_t *map;
    char *env, *str;
    int i, rc;
    char *target, *save, *type;
    char **env_save=NULL;

    /* Clear out the environment.  Use strdup() to orphan the resulting
     * strings because items are placed in the environment by reference,
     * not by value.
     */
    for (i = 0; i < mca_types->size; ++i) {
        if (NULL == (type = (char*)opal_pointer_array_get_item(mca_types, i))) {
            continue;
        }
        asprintf(&env, "OMPI_MCA_%s", type);
        if (NULL != (save = getenv(env))) {
            /* save this param so it can later be restored */
            asprintf(&str, "%s=%s", env, save);
            opal_argv_append_nosize(&env_save, str);
            free(str);
            /* can't manipulate it directly, so make a copy first */
            asprintf(&target, "%s=", env);
            putenv(target);
        }
        free(env);
    }

    /* some components require the event library be active, so activate it */
    if (OPAL_SUCCESS != (rc = mca_base_framework_open(&opal_event_base_framework, 0))) {
        str = "event open";
        goto breakout;
    }

    map = OBJ_NEW(opal_info_component_map_t);
    map->type = strdup(opal_event_base_framework.framework_name);
    map->components = &opal_event_base_framework.framework_components;
    opal_pointer_array_add(component_map, map);
    
    /* Open the DSS */
    if (OPAL_SUCCESS != (rc = opal_dss_open())) {
        if (OPAL_ERR_BAD_PARAM == rc)  {
            str = "opal_dss";
            goto breakout;
        }
        str = "dss_open";
        goto error;
    }
    
    /* Register the OPAL layer's MCA parameters */
    if (OPAL_SUCCESS != (rc = opal_register_params())) {
        str = "opal_register_params";
        if (OPAL_ERR_BAD_PARAM == rc)  {
            goto breakout;
        }
        goto error;
    }

    /* OPAL frameworks */

    if (OPAL_SUCCESS != (rc = info_register_framework (&opal_backtrace_base_framework, component_map))) {
        fprintf (stderr, "rc = %d\n", rc);
        str = "backtrace register";
        goto breakout;
    }

    if (OPAL_SUCCESS != (rc = info_register_framework (&opal_db_base_framework, component_map))) {
        str = "db open";
        goto breakout;
    }

#if OPAL_ENABLE_FT_CR == 1
    if (OPAL_SUCCESS != (rc = info_register_framework (&opal_compress_base_framework, component_map))) {
        str = "compress register";
        goto breakout;
    }

    if (OPAL_SUCCESS != (rc = info_register_framework (&opal_crs_base_framework, component_map))) {
        str = "crs open";
        goto breakout;
    }
#endif

#if OPAL_HAVE_HWLOC
    if (OPAL_SUCCESS != (rc = info_register_framework (&opal_hwloc_base_framework, component_map))) {
        str = "hwloc open";
        goto breakout;
    }
#endif

    if (OPAL_SUCCESS != (rc = info_register_framework (&opal_if_base_framework, component_map))) {
        str = "if open";
        goto breakout;
    }

    /* OPAL's installdirs base open has already been called as part of
     * opal_init_util() back in main().
     */
    map = OBJ_NEW(opal_info_component_map_t);
    map->type = strdup(opal_installdirs_base_framework.framework_name);
    map->components = &opal_installdirs_base_framework.framework_components;
    opal_pointer_array_add(component_map, map);

    if (OPAL_SUCCESS != (rc = info_register_framework (&opal_memory_base_framework, component_map))) {
        str = "memory";
        goto breakout;
    }

    if (OPAL_SUCCESS != (rc = info_register_framework (&opal_memcpy_base_framework, component_map))) {
        str = "memcpy";
        goto breakout;
    }

    if (OPAL_SUCCESS != (rc = info_register_framework (&opal_memchecker_base_framework, component_map))) {
        str = "memchecker open";
        goto breakout;
    }

    if (OPAL_SUCCESS != (rc = info_register_framework (&opal_shmem_base_framework, component_map))) {
        str = "shmem";
        goto breakout;
    }

    if (OPAL_SUCCESS != (rc = info_register_framework (&opal_timer_base_framework, component_map))) {
        str = "timer";
    }

 breakout:
    if (OPAL_ERR_BAD_PARAM == rc || OPAL_SUCCESS == rc) {
        /* Restore the environment to what it was before we started so that
         * if users setenv OMPI_MCA_<framework name> to some value, they'll
         * see that value when it is shown via --param output.
         */
    
        if (NULL != env_save) {
            for (i = 0; i < opal_argv_count(env_save); ++i) {
                putenv(env_save[i]);
            }
        }
    
        if (OPAL_ERR_BAD_PARAM == rc) {
            fprintf(stderr, "\nA \"bad parameter\" error was encountered when opening the OPAL %s framework.\n", str);
            fprintf(stderr, "The output received from that framework includes the following parameters:\n\n");
        }

        return rc;
    }

 error:
    fprintf(stderr, "opal_info_register: %s failed\n", str);
    return OPAL_ERROR;
}


void opal_info_close_components(void)
{
    (void) mca_base_framework_close(&opal_backtrace_base_framework);
    (void) mca_base_framework_close(&opal_memcpy_base_framework);
    (void) mca_base_framework_close(&opal_memory_base_framework);
    (void) mca_base_framework_close(&opal_memchecker_base_framework);
    (void) mca_base_framework_close(&opal_timer_base_framework);
#if OPAL_HAVE_HWLOC
    (void) mca_base_framework_close(&opal_hwloc_base_framework);
#endif
#if OPAL_ENABLE_FT_CR == 1
    (void) mca_base_framework_close(&opal_crs_base_framework);
#endif
    (void) opal_dss_close();
    (void) mca_base_framework_close(&opal_event_base_framework);
        
    /* Do not call OPAL's installdirs close; it will be handled in
     * opal_finalize_util().  Ditto for opal_if.
     */
}


void opal_info_show_path(const char *type, const char *value)
{
    char *pretty, *path;
    
    pretty = strdup(type);
    pretty[0] = toupper(pretty[0]);
    
    asprintf(&path, "path:%s", type);
    opal_info_out(pretty, path, value);
    free(pretty);
    free(path);
}

void opal_info_do_path(bool want_all, opal_cmd_line_t *cmd_line)
{
    int i, count;
    char *scope;
    
    /* Check bozo case */
    count = opal_cmd_line_get_ninsts(cmd_line, "path");
    for (i = 0; i < count; ++i) {
        scope = opal_cmd_line_get_param(cmd_line, "path", i, 0);
        if (0 == strcmp("all", scope)) {
            want_all = true;
            break;
        }
    }
    
    if (want_all) {
        opal_info_show_path(opal_info_path_prefix, opal_install_dirs.prefix);
        opal_info_show_path(opal_info_path_exec_prefix, opal_install_dirs.exec_prefix);
        opal_info_show_path(opal_info_path_bindir, opal_install_dirs.bindir);
        opal_info_show_path(opal_info_path_sbindir, opal_install_dirs.sbindir);
        opal_info_show_path(opal_info_path_libdir, opal_install_dirs.libdir);
        opal_info_show_path(opal_info_path_incdir, opal_install_dirs.includedir);
        opal_info_show_path(opal_info_path_mandir, opal_install_dirs.mandir);
        opal_info_show_path(opal_info_path_pkglibdir, opal_install_dirs.pkglibdir);
        opal_info_show_path(opal_info_path_libexecdir, opal_install_dirs.libexecdir);
        opal_info_show_path(opal_info_path_datarootdir, opal_install_dirs.datarootdir);
        opal_info_show_path(opal_info_path_datadir, opal_install_dirs.datadir);
        opal_info_show_path(opal_info_path_sysconfdir, opal_install_dirs.sysconfdir);
        opal_info_show_path(opal_info_path_sharedstatedir, opal_install_dirs.sharedstatedir);
        opal_info_show_path(opal_info_path_localstatedir, opal_install_dirs.localstatedir);
        opal_info_show_path(opal_info_path_infodir, opal_install_dirs.infodir);
        opal_info_show_path(opal_info_path_pkgdatadir, opal_install_dirs.pkgdatadir);
        opal_info_show_path(opal_info_path_pkglibdir, opal_install_dirs.pkglibdir);
        opal_info_show_path(opal_info_path_pkgincludedir, opal_install_dirs.pkgincludedir);
    } else {
        count = opal_cmd_line_get_ninsts(cmd_line, "path");
        for (i = 0; i < count; ++i) {
            scope = opal_cmd_line_get_param(cmd_line, "path", i, 0);
            
            if (0 == strcmp(opal_info_path_prefix, scope)) {
                opal_info_show_path(opal_info_path_prefix, opal_install_dirs.prefix);
            } else if (0 == strcmp(opal_info_path_bindir, scope)) {
                opal_info_show_path(opal_info_path_bindir, opal_install_dirs.bindir);
            } else if (0 == strcmp(opal_info_path_libdir, scope)) {
                opal_info_show_path(opal_info_path_libdir, opal_install_dirs.libdir);
            } else if (0 == strcmp(opal_info_path_incdir, scope)) {
                opal_info_show_path(opal_info_path_incdir, opal_install_dirs.includedir);
            } else if (0 == strcmp(opal_info_path_mandir, scope)) {
                opal_info_show_path(opal_info_path_mandir, opal_install_dirs.mandir);
            } else if (0 == strcmp(opal_info_path_pkglibdir, scope)) {
                opal_info_show_path(opal_info_path_pkglibdir, opal_install_dirs.pkglibdir);
            } else if (0 == strcmp(opal_info_path_sysconfdir, scope)) {
                opal_info_show_path(opal_info_path_sysconfdir, opal_install_dirs.sysconfdir);
            } else if (0 == strcmp(opal_info_path_exec_prefix, scope)) {
                opal_info_show_path(opal_info_path_exec_prefix, opal_install_dirs.exec_prefix);
            } else if (0 == strcmp(opal_info_path_sbindir, scope)) {
                opal_info_show_path(opal_info_path_sbindir, opal_install_dirs.sbindir);
            } else if (0 == strcmp(opal_info_path_libexecdir, scope)) {
                opal_info_show_path(opal_info_path_libexecdir, opal_install_dirs.libexecdir);
            } else if (0 == strcmp(opal_info_path_datarootdir, scope)) {
                opal_info_show_path(opal_info_path_datarootdir, opal_install_dirs.datarootdir);
            } else if (0 == strcmp(opal_info_path_datadir, scope)) {
                opal_info_show_path(opal_info_path_datadir, opal_install_dirs.datadir);
            } else if (0 == strcmp(opal_info_path_sharedstatedir, scope)) {
                opal_info_show_path(opal_info_path_sharedstatedir, opal_install_dirs.sharedstatedir);
            } else if (0 == strcmp(opal_info_path_localstatedir, scope)) {
                opal_info_show_path(opal_info_path_localstatedir, opal_install_dirs.localstatedir);
            } else if (0 == strcmp(opal_info_path_infodir, scope)) {
                opal_info_show_path(opal_info_path_infodir, opal_install_dirs.infodir);
            } else if (0 == strcmp(opal_info_path_pkgdatadir, scope)) {
                opal_info_show_path(opal_info_path_pkgdatadir, opal_install_dirs.pkgdatadir);
            } else if (0 == strcmp(opal_info_path_pkgincludedir, scope)) {
                opal_info_show_path(opal_info_path_pkgincludedir, opal_install_dirs.pkgincludedir);
            } else {
                char *usage = opal_cmd_line_get_usage_msg(cmd_line);
                opal_show_help("help-opal_info.txt", "usage", true, usage);
                free(usage);
                exit(1);
            }
        }
    }
}

void opal_info_do_params(bool want_all_in, bool want_internal,
                         opal_pointer_array_t *mca_types,
                         opal_cmd_line_t *opal_info_cmd_line)
{
    int count;
    char *type, *component, *str;
    bool found;
    int i;
    bool want_all = false;
    char *p;

    if (opal_cmd_line_is_taken(opal_info_cmd_line, "param")) {
        p = "param";
    } else if (opal_cmd_line_is_taken(opal_info_cmd_line, "params")) {
        p = "params";
    } else {
        p = "foo";  /* should never happen, but protect against segfault */
    }

    if (want_all_in) {
        want_all = true;
    } else {
        /* See if the special param "all" was given to --param; that
         * supercedes any individual type
         */
        count = opal_cmd_line_get_ninsts(opal_info_cmd_line, p);
        for (i = 0; i < count; ++i) {
            type = opal_cmd_line_get_param(opal_info_cmd_line, p, (int)i, 0);
            if (0 == strcmp(opal_info_type_all, type)) {
                want_all = true;
                break;
            }
        }
    }
    
    /* Show the params */
    
    if (want_all) {
        for (i = 0; i < mca_types->size; ++i) {
            if (NULL == (type = (char *)opal_pointer_array_get_item(mca_types, i))) {
                continue;
            }
            opal_info_show_mca_params(type, opal_info_component_all, want_internal);
        }
    } else {
        for (i = 0; i < count; ++i) {
            type = opal_cmd_line_get_param(opal_info_cmd_line, p, (int)i, 0);
            component = opal_cmd_line_get_param(opal_info_cmd_line, p, (int)i, 1);
            
            for (found = false, i = 0; i < mca_types->size; ++i) {
                if (NULL == (str = (char *)opal_pointer_array_get_item(mca_types, i))) {
                    continue;
                }
                if (0 == strcmp(str, type)) {
                    found = true;
                    break;
                }
            }
            
            if (!found) {
                char *usage = opal_cmd_line_get_usage_msg(opal_info_cmd_line);
                opal_show_help("help-opal_info.txt", "not-found", true, type);
                free(usage);
                exit(1);
            }
            
            opal_info_show_mca_params(type, component, want_internal);
        }
    }
}

void opal_info_err_params(opal_pointer_array_t *component_map)
{
    opal_info_component_map_t *map=NULL, *mptr;
    int i;

    /* all we want to do is display the LAST entry in the
     * component_map array as this is the one that generated the error
     */
    for (i=0; i < component_map->size; i++) {
        if (NULL == (mptr = (opal_info_component_map_t*)opal_pointer_array_get_item(component_map, i))) {
            continue;
        }
        map = mptr;
    }
    if (NULL == map) {
        fprintf(stderr, "opal_info_err_params: map not found\n");
        return;
    }
    opal_info_show_mca_params(map->type, opal_info_component_all, true);
    fprintf(stderr, "\n");
    return;
}


static void opal_info_show_mca_group_params(const mca_base_var_group_t *group, bool want_internal)
{
    const mca_base_var_t *var;
    const int *variables;
    int ret, i, j, count;
    const int *groups;
    char **strings;

    variables = OPAL_VALUE_ARRAY_GET_BASE(&group->group_vars, const int);
    count = opal_value_array_get_size((opal_value_array_t *)&group->group_vars);

    for (i = 0 ; i < count ; ++i) {
        ret = mca_base_var_get(variables[i], &var);
        if (OPAL_SUCCESS != ret || ((var->mbv_flags & MCA_BASE_VAR_FLAG_INTERNAL) &&
                                    !want_internal)) {
            continue;
        }

        ret = mca_base_var_dump(variables[i], &strings, !opal_info_pretty ? MCA_BASE_VAR_DUMP_PARSABLE : MCA_BASE_VAR_DUMP_READABLE);
        if (OPAL_SUCCESS != ret) {
            continue;
        }

        for (j = 0 ; strings[j] ; ++j) {
            if (0 == j && opal_info_pretty) {
                char *message;

                asprintf (&message, "MCA %s", group->group_framework);
                opal_info_out(message, message, strings[j]);
                free(message);
            } else {
                opal_info_out("", "", strings[j]);
            }
            free(strings[j]);
        }
        free(strings);
    }

    groups = OPAL_VALUE_ARRAY_GET_BASE(&group->group_subgroups, const int);
    count = opal_value_array_get_size((opal_value_array_t *)&group->group_subgroups);

    for (i = 0 ; i < count ; ++i) {
        ret = mca_base_var_group_get(groups[i], &group);
        if (OPAL_SUCCESS != ret) {
            continue;
        }
        opal_info_show_mca_group_params(group, want_internal);
    }
}

void opal_info_show_mca_params(const char *type, const char *component, 
                               bool want_internal)
{
    const mca_base_var_group_t *group;
    int ret;

    if (0 == strcmp (component, "all")) {
        ret = mca_base_var_group_find("*", type, NULL);
        if (0 > ret) {
            return;
        }

        (void) mca_base_var_group_get(ret, &group);

        opal_info_show_mca_group_params(group, want_internal);
    } else {
        ret = mca_base_var_group_find("*", type, component);
        if (0 > ret) {
            return;
        }

        (void) mca_base_var_group_get(ret, &group);
        opal_info_show_mca_group_params(group, want_internal);
    }
}



void opal_info_do_arch()
{
    opal_info_out("Configured architecture", "config:arch", OPAL_ARCH);
}


void opal_info_do_hostname()
{
    opal_info_out("Configure host", "config:host", OPAL_CONFIGURE_HOST);
}


/*
 * Private variables - set some reasonable screen size defaults
 */

static int centerpoint = 24;
static int screen_width = 78;

/* 
 * Prints the passed integer in a pretty or parsable format.
 */
void opal_info_out(const char *pretty_message, const char *plain_message, const char *value)
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

    /* Strip leading and trailing whitespace from the string value */
    value_offset = strspn(value, " ");

    v = v_to_free = strdup(value + value_offset);
    len = strlen(v);

    if (len > 0) {
        while (len > 0 && isspace(v[len-1])) len--;
        v[len] = '\0';
    }

    if (opal_info_pretty && NULL != pretty_message) {
        if (centerpoint > (int)strlen(pretty_message)) {
            asprintf(&spaces, "%*s", centerpoint - 
                     (int)strlen(pretty_message), " ");
        } else {
            spaces = strdup("");
#if OPAL_ENABLE_DEBUG
            if (centerpoint < (int)strlen(pretty_message)) {
                opal_show_help("help-opal_info.txt", 
                               "developer warning: field too long", false,
                               pretty_message, centerpoint);
            }
#endif
        }
        max_value_width = screen_width - strlen(spaces) - strlen(pretty_message) - 2;
        if (0 < strlen(pretty_message)) {
            asprintf(&filler, "%s%s: ", spaces, pretty_message);
        } else {
            asprintf(&filler, "%s  ", spaces);
        }
        free(spaces);
        spaces = NULL;
        
        while (true) {
            if (strlen(v) < max_value_width) {
                printf("%s%s\n", filler, v);
                break;
            } else {
                asprintf(&spaces, "%*s", centerpoint + 2, " ");
                
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
            printf("%s:%s\n", plain_message, value);
        } else {
            printf("%s\n", value);
        }
    }
    if (NULL != v_to_free) {
        free(v_to_free);
    }
}

void opal_info_out_int(const char *pretty_message, 
                       const char *plain_message, 
                       int value)
{
    char *valstr;
    
    asprintf(&valstr, "%d", (int)value);
    opal_info_out(pretty_message, plain_message, valstr);
    free(valstr);
}

/*
 * Show all the components of a specific type/component combo (component may be
 * a wildcard)
 */
void opal_info_show_component_version(opal_pointer_array_t *mca_types,
                                      opal_pointer_array_t *component_map,
                                      const char *type_name, 
                                      const char *component_name,
                                      const char *scope, const char *ver_type)
{
    bool want_all_components = false;
    bool found;
    opal_list_item_t *item;
    mca_base_component_list_item_t *cli;
    const mca_base_component_t *component;
    opal_list_t *components;
    int j;
    char *pos;
    opal_info_component_map_t *map;
    
    /* see if all components wanted */
    if (0 == strcmp(opal_info_type_all, component_name)) {
        want_all_components = true;
    }
    
    /* Check to see if the type is valid */
    
    for (found = false, j = 0; j < mca_types->size; ++j) {
        if (NULL == (pos = (char*)opal_pointer_array_get_item(mca_types, j))) {
            continue;
        }
        if (0 == strcmp(pos, type_name)) {
            found = true;
            break;
        }
    }
    
    if (!found) {
        exit(1);
    }
    
    /* Now that we have a valid type, find the right component list */
    components = NULL;
    for (j=0; j < component_map->size; j++) {
        if (NULL == (map = (opal_info_component_map_t*)opal_pointer_array_get_item(component_map, j))) {
            continue;
        }
        if (0 == strcmp(type_name, map->type)) {
            /* found it! */
            components = map->components;
            break;
        }
    }

    if (NULL != components) {
        if (opal_list_get_size(components) > 0){
            for (item = opal_list_get_first(components);
                 opal_list_get_end(components) != item;
                 item = opal_list_get_next(item)) {
                cli = (mca_base_component_list_item_t *) item;
                component = cli->cli_component;
                if (want_all_components || 
                    0 == strcmp(component->mca_component_name, component_name)) {
                    opal_info_show_mca_version(component, scope, ver_type);
                }
            }
        }
    }
}


/*
 * Given a component, display its relevant version(s)
 */
void opal_info_show_mca_version(const mca_base_component_t* component,
                                const char *scope, const char *ver_type)
{
    bool printed;
    bool want_mca = false;
    bool want_type = false;
    bool want_component = false;
    char *message, *content;
    char *mca_version;
    char *api_version;
    char *component_version;
    char *tmp;
    
    if (0 == strcmp(ver_type, opal_info_ver_all) ||
        0 == strcmp(ver_type, opal_info_ver_mca)) {
        want_mca = true;
    }
    
    if (0 == strcmp(ver_type, opal_info_ver_all) ||
        0 == strcmp(ver_type, opal_info_ver_type)) {
        want_type = true;
    }
    
    if (0 == strcmp(ver_type, opal_info_ver_all) ||
        0 == strcmp(ver_type, opal_info_ver_component)) {
        want_component = true;
    }
    
    mca_version = opal_info_make_version_str(scope, component->mca_major_version,
                                             component->mca_minor_version,
                                             component->mca_release_version, "",
                                             false, "");
    api_version = opal_info_make_version_str(scope, component->mca_type_major_version,
                                             component->mca_type_minor_version,
                                             component->mca_type_release_version, "",
                                             false, "");
    component_version = opal_info_make_version_str(scope, component->mca_component_major_version,
                                                   component->mca_component_minor_version,
                                                   component->mca_component_release_version, 
                                                   "", false, "");
    if (opal_info_pretty) {
        asprintf(&message, "MCA %s", component->mca_type_name);
        printed = false;
        asprintf(&content, "%s (", component->mca_component_name);
        
        if (want_mca) {
            asprintf(&tmp, "%sMCA v%s", content, mca_version);
            free(content);
            content = tmp;
            printed = true;
        }

        if (want_type) {
            if (printed) {
                asprintf(&tmp, "%s, ", content);
                free(content);
                content = tmp;
            }
            asprintf(&tmp, "%sAPI v%s", content, api_version);
            free(content);
            content = tmp;
            printed = true;
        }

        if (want_component) {
            if (printed) {
                asprintf(&tmp, "%s, ", content);
                free(content);
                content = tmp;
            }
            asprintf(&tmp, "%sComponent v%s", content, component_version);
            free(content);
            content = tmp;
            printed = true;
        }
        if (NULL != content) {
            asprintf(&tmp, "%s)", content);
            free(content);
        } else {
            tmp = NULL;
        }
        
        opal_info_out(message, NULL, tmp);
        free(message);
        if (NULL != tmp) {
            free(tmp);
        }
        
    } else {
        asprintf(&message, "mca:%s:%s:version", component->mca_type_name, component->mca_component_name);
        if (want_mca) {
            asprintf(&tmp, "mca:%s", mca_version);
            opal_info_out(NULL, message, tmp);
            free(tmp);
        }
        if (want_type) {
            asprintf(&tmp, "api:%s", api_version);
            opal_info_out(NULL, message, tmp);
            free(tmp);
        }
        if (want_component) {
            asprintf(&tmp, "component:%s", component_version);
            opal_info_out(NULL, message, tmp);
            free(tmp);
        }
        free(message);
    }

    if (NULL != mca_version) {
        free(mca_version);
    }
    if (NULL != api_version) {
        free(api_version);
    }
    if (NULL != component_version) {
        free(component_version);
    }
}


char *opal_info_make_version_str(const char *scope,
                                 int major, int minor, int release,
                                 const char *greek, 
                                 bool want_repo_rev, const char *repo)
{
    char *str = NULL, *tmp;
    char temp[BUFSIZ];
    
    temp[BUFSIZ - 1] = '\0';
    if (0 == strcmp(scope, opal_info_ver_full) ||
        0 == strcmp(scope, opal_info_ver_all)) {
        snprintf(temp, BUFSIZ - 1, "%d.%d", major, minor);
        str = strdup(temp);
        if (release > 0) {
            snprintf(temp, BUFSIZ - 1, ".%d", release);
            asprintf(&tmp, "%s%s", str, temp);
            free(str);
            str = tmp;
        }
        if (NULL != greek) {
            asprintf(&tmp, "%s%s", str, greek);
            free(str);
            str = tmp;
        }
        if (want_repo_rev && NULL != repo) {
            asprintf(&tmp, "%s%s", str, repo);
            free(str);
            str = tmp;
        }
    } else if (0 == strcmp(scope, opal_info_ver_major)) {
        snprintf(temp, BUFSIZ - 1, "%d", major);
    } else if (0 == strcmp(scope, opal_info_ver_minor)) {
        snprintf(temp, BUFSIZ - 1, "%d", minor);
    } else if (0 == strcmp(scope, opal_info_ver_release)) {
        snprintf(temp, BUFSIZ - 1, "%d", release);
    } else if (0 == strcmp(scope, opal_info_ver_greek)) {
        str = strdup(greek);
    } else if (0 == strcmp(scope, opal_info_ver_repo)) {
        str = strdup(repo);
    }
    
    if (NULL == str) {
        str = strdup(temp);
    }
    
    return str;
}

void opal_info_show_opal_version(const char *scope)
{
    char *tmp, *tmp2;

    asprintf(&tmp, "%s:version:full", opal_info_type_opal);
    tmp2 = opal_info_make_version_str(scope, 
                                      OPAL_MAJOR_VERSION, OPAL_MINOR_VERSION, 
                                      OPAL_RELEASE_VERSION, 
                                      OPAL_GREEK_VERSION,
                                      OPAL_WANT_REPO_REV, OPAL_REPO_REV);
    opal_info_out("OPAL", tmp, tmp2);
    free(tmp);
    free(tmp2);
    asprintf(&tmp, "%s:version:repo", opal_info_type_opal);
    opal_info_out("OPAL repo revision", tmp, OPAL_REPO_REV);
    free(tmp);
    asprintf(&tmp, "%s:version:release_date", opal_info_type_opal);
    opal_info_out("OPAL release date", tmp, OPAL_RELEASE_DATE);
    free(tmp);
}
