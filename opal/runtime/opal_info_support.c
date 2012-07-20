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

#include "opal/class/opal_list.h"
#include "opal/class/opal_pointer_array.h"

#include "opal/util/output.h"
#include "opal/util/cmd_line.h"
#include "opal/util/error.h"
#include "opal/util/argv.h"
#include "opal/util/show_help.h"
#include "opal/runtime/opal.h"
#include "opal/dss/dss.h"

#include "opal/mca/installdirs/installdirs.h"
#include "opal/mca/event/base/base.h"
#include "opal/mca/base/base.h"
#include "opal/mca/backtrace/base/base.h"
#include "opal/mca/shmem/base/base.h"
#include "opal/mca/memory/base/base.h"
#include "opal/mca/memchecker/base/base.h"
#include "opal/mca/timer/base/base.h"
#include "opal/mca/installdirs/base/base.h"
#include "opal/mca/hwloc/base/base.h"
#if OPAL_ENABLE_FT_CR == 1
#include "opal/mca/crs/base/base.h"
#include "opal/mca/compress/base/base.h"
#endif
#include "opal/mca/if/base/base.h"

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

void opal_info_register_types(opal_pointer_array_t *mca_types)
{
    opal_pointer_array_add(mca_types, "backtrace");
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
    opal_pointer_array_add(mca_types, "opal");
    opal_pointer_array_add(mca_types, "shmem");
    opal_pointer_array_add(mca_types, "timer");

}

int opal_info_register_components(opal_pointer_array_t *mca_types,
                                  opal_pointer_array_t *component_map)
{
    opal_info_component_map_t *map;
    char *env, *str;
    int i;
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
            free(target);
        }
        free(env);
    }

    /* some components require the event library be active, so activate it */
    if (OPAL_SUCCESS != opal_event_base_open()) {
        str = "opal_event_base_open";
        goto error;
    }
    
    /* Open the DSS */
    if (OPAL_SUCCESS != opal_dss_open()) {
        str = "Unable to initialize the DSS";
        goto error;
    }
    
    /* Register the OPAL layer's MCA parameters */
    if (OPAL_SUCCESS != opal_register_params()) {
        str = "opal_register_params failed";
        goto error;
    }

    /* OPAL frameworks */
    
    if (OPAL_SUCCESS != opal_backtrace_base_open()) {
        str = "backtrace open failed";
        goto error;
    }
    map = OBJ_NEW(opal_info_component_map_t);
    map->type = strdup("backtrace");
    map->components = &opal_backtrace_base_components_opened;
    opal_pointer_array_add(component_map, map);
    
#if OPAL_ENABLE_FT_CR == 1
    if (OPAL_SUCCESS != opal_compress_base_open()) {
        str = "compress open failed";
        goto error;
    }
    map = OBJ_NEW(opal_info_component_map_t);
    map->type = strdup("compress");
    map->components = &opal_compress_base_components_available;
    opal_pointer_array_add(component_map, map);

    if (OPAL_SUCCESS != opal_crs_base_open()) {
        str = "crs open failed";
        goto error;
    }
    map = OBJ_NEW(opal_info_component_map_t);
    map->type = strdup("crs");
    map->components = &opal_crs_base_components_available;
    opal_pointer_array_add(component_map, map);
#endif

    /* the event framework is already open - just get its components */
    map = OBJ_NEW(opal_info_component_map_t);
    map->type = strdup("event");
    map->components = &opal_event_components;
    opal_pointer_array_add(component_map, map);

#if OPAL_HAVE_HWLOC
    if (OPAL_SUCCESS != opal_hwloc_base_open()) {
        str = "hwloc open failed";
        goto error;
    }
    map = OBJ_NEW(opal_info_component_map_t);
    map->type = strdup("hwloc");
    map->components = &opal_hwloc_base_components;
    opal_pointer_array_add(component_map, map);
#endif

    if (OPAL_SUCCESS != opal_if_base_open()) {
        str = "if open failed";
        goto error;
    }
    map = OBJ_NEW(opal_info_component_map_t);
    map->type = strdup("if");
    map->components = &opal_if_components;
    opal_pointer_array_add(component_map, map);

    /* OPAL's installdirs base open has already been called as part of
     * opal_init_util() back in main().
     */
    map = OBJ_NEW(opal_info_component_map_t);
    map->type = strdup("installdirs");
    map->components = &opal_installdirs_components;
    opal_pointer_array_add(component_map, map);

    if (OPAL_SUCCESS != opal_memory_base_open()) {
        str = "memory open failed";
        goto error;
    }
    map = OBJ_NEW(opal_info_component_map_t);
    map->type = strdup("memory");
    map->components = &opal_memory_base_components_opened;
    opal_pointer_array_add(component_map, map);
    
    if (OPAL_SUCCESS != opal_memchecker_base_open()) {
        str = "memchecker open failed";
        goto error;
    }
    map = OBJ_NEW(opal_info_component_map_t);
    map->type = strdup("memchecker");
    map->components = &opal_memchecker_base_components_opened;
    opal_pointer_array_add(component_map, map);
    
    if (OPAL_SUCCESS != opal_shmem_base_open()) {
        str = "shmem open failed";
        goto error;
    }
    map = OBJ_NEW(opal_info_component_map_t);
    map->type = strdup("shmem");
    map->components = &opal_shmem_base_components_opened;
    opal_pointer_array_add(component_map, map);
    
    if (OPAL_SUCCESS != opal_timer_base_open()) {
        str = "timer open failed";
        goto error;
    }
    map = OBJ_NEW(opal_info_component_map_t);
    map->type = strdup("timer");
    map->components = &opal_timer_base_components_opened;
    opal_pointer_array_add(component_map, map);
    
    /* Restore the environment to what it was before we started so that
     * if users setenv OMPI_MCA_<framework name> to some value, they'll
     * see that value when it is shown via --param output.
     */
    
    if (NULL != env_save) {
        for (i = 0; i < opal_argv_count(env_save); ++i) {
            putenv(env_save[i]);
        }
    }
    
    return OPAL_SUCCESS;

 error:
    fprintf(stderr, "opal_info_register: %s\n", str);
    return OPAL_ERROR;
}

void opal_info_close_components(void)
{
    (void) opal_backtrace_base_close();
    (void) opal_memory_base_close();
    (void) opal_memchecker_base_close();
    (void) opal_timer_base_close();
#if OPAL_HAVE_HWLOC
    (void) opal_hwloc_base_close();
#endif
#if OPAL_ENABLE_FT_CR == 1
    (void) opal_crs_base_close();
#endif
    (void) opal_dss_close();
    (void) opal_event_base_close();
        
    /* Do not call OPAL's installdirs close; it will be handled in
     * opal_finalize_util().
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

/*
 * External variables
 *
 * This exists in mca/base/mca_base_param.c.  It's not extern'ed
 * in mca_base_param.h so that no one else will use it.
 */

extern opal_value_array_t mca_base_params;


void opal_info_do_params(bool want_all_in, bool want_internal,
                         opal_pointer_array_t *mca_types,
                         opal_cmd_line_t *opal_info_cmd_line)
{
    int count;
    char *type, *component, *str;
    bool found;
    int i;
    bool want_all = false;
    opal_list_t *info;
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
        /* See if the special param "all" was givin to --param; that
         * superceeds any individual type
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
    
    /* Get a dump of all the MCA params */
    mca_base_param_dump(&info, want_internal);
    
    /* Show the params */
    
    if (want_all) {
        for (i = 0; i < mca_types->size; ++i) {
            if (NULL == (type = (char *)opal_pointer_array_get_item(mca_types, i))) {
                continue;
            }
            opal_info_show_mca_params(info, type, opal_info_component_all, want_internal);
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
            
            opal_info_show_mca_params(info, type, component, want_internal);
        }
    }
    
    /* Release all the MCA param memory */
    mca_base_param_dump_release(info);
}

void opal_info_show_mca_params(opal_list_t *info,
                               const char *type, const char *component, 
                               bool want_internal)
{
    opal_list_item_t *i;
    mca_base_param_info_t *p;
    char *value_string, *empty = "";
    char *message, *content, *tmp;
    int value_int, j;
    mca_base_param_source_t source;
    char *src_file;
    
    for (i = opal_list_get_first(info); i != opal_list_get_last(info);
         i = opal_list_get_next(i)) {
        p = (mca_base_param_info_t*) i;
        
        if (NULL != p->mbpp_type_name && 0 == strcmp(type, p->mbpp_type_name)) {
            if (0 == strcmp(component, opal_info_component_all) || 
                NULL == p->mbpp_component_name ||
                (NULL != p->mbpp_component_name &&
                 0 == strcmp(component, p->mbpp_component_name))) {
                
                /* Find the source of the value */
                if (OPAL_SUCCESS != 
                    mca_base_param_lookup_source(p->mbpp_index, &source, &src_file)) {
                    continue;
                }
                
                /* Make a char *for the default value.  Invoke a
                 * lookup because it may transform the char *("~/" ->
                 * "<home dir>/") or get the value from the
                 * environment, a file, etc.
                 */
                if (MCA_BASE_PARAM_TYPE_STRING == p->mbpp_type) {
                    mca_base_param_lookup_string(p->mbpp_index,
                                                 &value_string);
                    
                    /* Can't let the char *be NULL because we
                     * assign it to a std::string, below
                     */
                    if (NULL == value_string) {
                        value_string = strdup(empty);
                    }
                } else {
                    mca_base_param_lookup_int(p->mbpp_index, &value_int);
                    asprintf(&value_string, "%d", value_int);
                }
                
                /* Build up the strings to opal_info_output. */
                
                if (opal_info_pretty) {
                    asprintf(&message, "MCA %s", p->mbpp_type_name);
                    
                    /* Put in the real, full name (which may be
                     * different than the categorization).
                     */
                    asprintf(&content, "%s \"%s\" (%s: <%s>, data source: ",
                             p->mbpp_read_only ? "information" : "parameter",
                             p->mbpp_full_name,
                             p->mbpp_read_only ? "value" : "current value",
                             (0 == strlen(value_string)) ? "none" : value_string);
                    
                    /* Indicate where the param was set from */
                    switch(source) {
                        case MCA_BASE_PARAM_SOURCE_DEFAULT:
                            asprintf(&tmp, "%sdefault value", content);
                            free(content);
                            content = tmp;
                            break;
                        case MCA_BASE_PARAM_SOURCE_ENV:
                            asprintf(&tmp, "%senvironment or cmdline", content);
                            free(content);
                            content = tmp;
                            break;
                        case MCA_BASE_PARAM_SOURCE_FILE:
                            asprintf(&tmp, "%sfile [%s]", content, src_file);
                            free(content);
                            content = tmp;
                            break;
                        case MCA_BASE_PARAM_SOURCE_OVERRIDE:
                            asprintf(&tmp, "%sAPI override", content);
                            free(content);
                            content = tmp;
                            break;
                        default:
                            break;
                    }
                    
                    /* Is this parameter deprecated? */
                    if (p->mbpp_deprecated) {
                        asprintf(&tmp, "%s, deprecated", content);
                        free(content);
                        content = tmp;
                    }
                    
                    /* Does this parameter have any synonyms? */
                    if (p->mbpp_synonyms_len > 0) {
                        asprintf(&tmp, "%s, synonyms: ", content);
                        free(content);
                        content = tmp;
                        for (j = 0; j < p->mbpp_synonyms_len; ++j) {
                            if (j > 0) {
                                asprintf(&tmp, "%s, %s", content, p->mbpp_synonyms[j]->mbpp_full_name);
                                free(content);
                                content = tmp;
                            } else {
                                asprintf(&tmp, "%s%s", content, p->mbpp_synonyms[j]->mbpp_full_name);
                                free(content);
                                content = tmp;
                            }
                        }
                    }
                    
                    /* Is this parameter a synonym of something else? */
                    else if (NULL != p->mbpp_synonym_parent) {
                        asprintf(&tmp, "%s, synonym of: %s", content, p->mbpp_synonym_parent->mbpp_full_name);
                        free(content);
                        content = tmp;
                    }
                    asprintf(&tmp, "%s)", content);
                    free(content);
                    content = tmp;
                    opal_info_out(message, message, content);
                    free(message);
                    free(content);
                    
                    /* If we have a help message, opal_info_output it */
                    if (NULL != p->mbpp_help_msg) {
                        opal_info_out("", "", p->mbpp_help_msg);
                    }
                } else {
                    /* build the message*/
                    asprintf(&tmp, "mca:%s:%s:param:%s:", p->mbpp_type_name,
                             (NULL == p->mbpp_component_name) ? "base" : p->mbpp_component_name,
                             p->mbpp_full_name);

                    /* Output the value */
                    asprintf(&message, "%svalue", tmp);
                    opal_info_out(message, message, value_string);
                    free(message);
                    
                    /* Indicate where the param was set from */
                    
                    asprintf(&message, "%sdata_source", tmp);
                    switch(source) {
                        case MCA_BASE_PARAM_SOURCE_DEFAULT:
                            content = strdup("default value");
                            break;
                        case MCA_BASE_PARAM_SOURCE_ENV:
                            content = strdup("environment-cmdline");
                            break;
                        case MCA_BASE_PARAM_SOURCE_FILE:
                            asprintf(&content, "file: %s", src_file);
                            break;
                        case MCA_BASE_PARAM_SOURCE_OVERRIDE:
                            content = strdup("API override");
                            break;
                        default:
                            break;
                    }
                    opal_info_out(message, message, content);
                    free(message);
                    free(content);
                    
                    /* Output whether it's read only or writable */
                    
                    asprintf(&message, "%sstatus", tmp);
                    content = p->mbpp_read_only ? "read-only" : "writable";
                    opal_info_out(message, message, content);
                    free(message);
                    
                    /* If it has a help message, opal_info_output that */
                    
                    if (NULL != p->mbpp_help_msg) {
                        asprintf(&message, "%shelp", tmp);
                        content = p->mbpp_help_msg;
                        opal_info_out(message, message, content);
                        free(message);
                    }
                    
                    /* Is this parameter deprecated? */
                    asprintf(&message, "%sdeprecated", tmp);
                    content = p->mbpp_deprecated ? "yes" : "no";
                    opal_info_out(message, message, content);
                    free(message);
                    
                    /* Does this parameter have any synonyms? */
                    if (p->mbpp_synonyms_len > 0) {
                        for (j = 0; j < p->mbpp_synonyms_len; ++j) {
                            asprintf(&message, "%ssynonym:name", tmp);
                            content = p->mbpp_synonyms[j]->mbpp_full_name;
                            opal_info_out(message, message, content);
                            free(message);
                        }
                    }
                    
                    /* Is this parameter a synonym of something else? */
                    else if (NULL != p->mbpp_synonym_parent) {
                        asprintf(&message, "%ssynonym_of:name", tmp);
                        content = p->mbpp_synonym_parent->mbpp_full_name;
                        opal_info_out(message, message, content);
                        free(message);
                    }
                }
                
                /* If we allocated the string, then free it */
                
                if (NULL != value_string) {
                    free(value_string);
                }
            }
        }
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
    size_t i, len, max_value_width;
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
    v = v_to_free = strdup(value);
    len = strlen(v);
    if (isspace(v[0])) {
        char *newv;
        i = 0;
        while (isspace(v[i]) && i < len) {
            ++i;
        }
        newv = strdup(v + i);
        free(v_to_free);
        v_to_free = v = newv;
        len = strlen(v);
    }
    if (len > 0 && isspace(v[len - 1])) {
        i = len - 1;
        /* Note that i is size_t (unsigned), so we can't check for i
           >= 0.  But we don't need to, because if the value was all
           whitespace, stripping whitespace from the left (above)
           would have resulted in an empty string, and we wouldn't
           have gotten into this block. */
        while (isspace(v[i]) && i > 0) {
            --i;
        }
        v[i + 1] = '\0';
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
            printf("  %s\n", value);
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
