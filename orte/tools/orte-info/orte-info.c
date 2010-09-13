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
 * Copryight (c) 2007-2009 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

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
#include "opal/util/cmd_line.h"
#include "opal/util/argv.h"
#include "opal/mca/base/base.h"

#include "orte/util/show_help.h"
#include "orte/runtime/orte_locks.h"

#include "orte/tools/orte-info/orte-info.h"

/*
 * Public variables
 */

bool orte_info_pretty = true;
opal_cmd_line_t *orte_info_cmd_line = NULL;

const char *orte_info_type_all = "all";
const char *orte_info_type_orte = "orte";
const char *orte_info_type_opal = "opal";
const char *orte_info_type_base = "base";

opal_pointer_array_t mca_types;

int main(int argc, char *argv[])
{
    int ret = 0;
    bool want_help = false;
    bool cmd_error = false;
    bool acted = false;
    bool want_all = false;
    char **app_env = NULL, **global_env = NULL;
    int i, len;
    char *str;
    
    /* Initialize the argv parsing handle */
    if (ORTE_SUCCESS != opal_init_util(&argc, &argv)) {
        orte_show_help("help-orte-info.txt", "lib-call-fail", true, 
                       "opal_init_util", __FILE__, __LINE__, NULL);
        exit(ret);
    }
    
    orte_info_cmd_line = OBJ_NEW(opal_cmd_line_t);
    if (NULL == orte_info_cmd_line) {
        ret = errno;
        orte_show_help("help-orte-info.txt", "lib-call-fail", true, 
                       "opal_cmd_line_create", __FILE__, __LINE__, NULL);
        opal_finalize_util();
        exit(ret);
    }
    
    opal_cmd_line_make_opt3(orte_info_cmd_line, 'v', NULL, "version", 2, 
                            "Show version of ORTE or a component.  The first parameter can be the keywords \"orte\" or \"all\", a framework name (indicating all components in a framework), or a framework:component string (indicating a specific component).  The second parameter can be one of: full, major, minor, release, greek, svn.");
    opal_cmd_line_make_opt3(orte_info_cmd_line, '\0', NULL, "param", 2, 
                            "Show MCA parameters.  The first parameter is the framework (or the keyword \"all\"); the second parameter is the specific component name (or the keyword \"all\").");
    opal_cmd_line_make_opt3(orte_info_cmd_line, '\0', NULL, "internal", 0, 
                            "Show internal MCA parameters (not meant to be modified by users)");
    opal_cmd_line_make_opt3(orte_info_cmd_line, '\0', NULL, "path", 1, 
                            "Show paths that Open MPI was configured with.  Accepts the following parameters: prefix, bindir, libdir, incdir, mandir, pkglibdir, sysconfdir");
    opal_cmd_line_make_opt3(orte_info_cmd_line, '\0', NULL, "arch", 0, 
                            "Show architecture Open MPI was corteled on");
    opal_cmd_line_make_opt3(orte_info_cmd_line, 'c', NULL, "config", 0, 
                            "Show configuration options");
    opal_cmd_line_make_opt3(orte_info_cmd_line, 'h', NULL, "help", 0, 
                            "Show this help message");
    opal_cmd_line_make_opt3(orte_info_cmd_line, '\0', NULL, "orte_info_pretty", 0, 
                            "When used in conjunction with other parameters, the output is displayed in 'orte_info_prettyprint' format (default)");
    opal_cmd_line_make_opt3(orte_info_cmd_line, '\0', NULL, "parsable", 0, 
                            "When used in conjunction with other parameters, the output is displayed in a machine-parsable format");
    opal_cmd_line_make_opt3(orte_info_cmd_line, '\0', NULL, "parseable", 0, 
                            "Synonym for --parsable");
    opal_cmd_line_make_opt3(orte_info_cmd_line, '\0', NULL, "hostname", 0, 
                            "Show the hostname that Open MPI was configured "
                            "and built on");
    opal_cmd_line_make_opt3(orte_info_cmd_line, 'a', NULL, "all", 0, 
                            "Show all configuration options and MCA parameters");
    
    /* Call some useless functions in order to guarantee to link in some
     * global variables.  Only check the return value so that the
     * corteler doesn't optimize out the useless function.
     */
    
    if (ORTE_SUCCESS != orte_locks_init()) {
        /* Stop .. or I'll say stop again! */
        ++ret;
    } else {
        --ret;
    }
    
    /* set our threading level */
    opal_set_using_threads(false);
    
    /* Get MCA parameters, if any */
    
    if( ORTE_SUCCESS != mca_base_open() ) {
        orte_show_help("help-orte-info.txt", "lib-call-fail", true, "mca_base_open", __FILE__, __LINE__ );
        OBJ_RELEASE(orte_info_cmd_line);
        opal_finalize_util();
        exit(1);
    }
    mca_base_cmd_line_setup(orte_info_cmd_line);
    
    /* Do the parsing */
    
    if (ORTE_SUCCESS != opal_cmd_line_parse(orte_info_cmd_line, false, argc, argv)) {
        cmd_error = true;
    }
    if (!cmd_error && 
        (opal_cmd_line_is_taken(orte_info_cmd_line, "help") || 
         opal_cmd_line_is_taken(orte_info_cmd_line, "h"))) {
        want_help = true;
    }
    if (cmd_error || want_help) {
        char *usage = opal_cmd_line_get_usage_msg(orte_info_cmd_line);
        orte_show_help("help-orte-info.txt", "usage", true, usage);
        free(usage);
        mca_base_close();
        OBJ_RELEASE(orte_info_cmd_line);
        opal_finalize_util();
        exit(cmd_error ? 1 : 0);
    }
    
    mca_base_cmd_line_process_args(orte_info_cmd_line, &app_env, &global_env);
    
    /* putenv() all the stuff that we got back from env (in case the
     * user specified some --mca params on the command line).  This
     * creates a memory leak, but that's unfortunately how putenv()
     * works.  :-(
     */
    
    len = opal_argv_count(app_env);
    for (i = 0; i < len; ++i) {
        putenv(app_env[i]);
    }
    len = opal_argv_count(global_env);
    for (i = 0; i < len; ++i) {
        putenv(global_env[i]);
    }
    
    /* setup the mca_types array */
    OBJ_CONSTRUCT(&mca_types, opal_pointer_array_t);
    opal_pointer_array_init(&mca_types, 256, INT_MAX, 128);
    
    opal_pointer_array_add(&mca_types, "mca");
    opal_pointer_array_add(&mca_types, "orte");
    opal_pointer_array_add(&mca_types, "opal");
    
    opal_pointer_array_add(&mca_types, "filter");
    opal_pointer_array_add(&mca_types, "backtrace");
    opal_pointer_array_add(&mca_types, "memchecker");
    opal_pointer_array_add(&mca_types, "memory");
    opal_pointer_array_add(&mca_types, "paffinity");
    opal_pointer_array_add(&mca_types, "carto");
    opal_pointer_array_add(&mca_types, "maffinity");
    opal_pointer_array_add(&mca_types, "timer");
    opal_pointer_array_add(&mca_types, "installdirs");
    opal_pointer_array_add(&mca_types, "sysinfo");
#if OPAL_ENABLE_FT_CR == 1
    opal_pointer_array_add(&mca_types, "crs");
#endif
    opal_pointer_array_add(&mca_types, "if");
    
#if !ORTE_DISABLE_FULL_SUPPORT
    opal_pointer_array_add(&mca_types, "debugger");
    opal_pointer_array_add(&mca_types, "iof");
    opal_pointer_array_add(&mca_types, "oob");
    opal_pointer_array_add(&mca_types, "odls");
    opal_pointer_array_add(&mca_types, "ras");
    opal_pointer_array_add(&mca_types, "rmaps");
    opal_pointer_array_add(&mca_types, "rmcast");
    opal_pointer_array_add(&mca_types, "rml");
    opal_pointer_array_add(&mca_types, "routed");
    opal_pointer_array_add(&mca_types, "plm");
#if OPAL_ENABLE_FT_CR == 1
    opal_pointer_array_add(&mca_types, "snapc");
#endif
    opal_pointer_array_add(&mca_types, "sensor");
    opal_pointer_array_add(&mca_types, "filem");
#endif
    /* these are always included */
    opal_pointer_array_add(&mca_types, "errmgr");
    opal_pointer_array_add(&mca_types, "ess");
    opal_pointer_array_add(&mca_types, "grpcomm");
    opal_pointer_array_add(&mca_types, "db");
    opal_pointer_array_add(&mca_types, "notifier");
    
    /* Execute the desired action(s) */
    
    if (opal_cmd_line_is_taken(orte_info_cmd_line, "orte_info_pretty")) {
        orte_info_pretty = true;
    } else if (opal_cmd_line_is_taken(orte_info_cmd_line, "parsable") || opal_cmd_line_is_taken(orte_info_cmd_line, "parseable")) {
        orte_info_pretty = false;
    }
    
    want_all = opal_cmd_line_is_taken(orte_info_cmd_line, "all");
    if (want_all || opal_cmd_line_is_taken(orte_info_cmd_line, "version")) {
        orte_info_do_version(want_all, orte_info_cmd_line);
        acted = true;
    }
    if (want_all || opal_cmd_line_is_taken(orte_info_cmd_line, "path")) {
        orte_info_do_path(want_all, orte_info_cmd_line);
        acted = true;
    }
    if (want_all || opal_cmd_line_is_taken(orte_info_cmd_line, "arch")) {
        orte_info_do_arch();
        acted = true;
    }
    if (want_all || opal_cmd_line_is_taken(orte_info_cmd_line, "hostname")) {
        orte_info_do_hostname();
        acted = true;
    }
    if (want_all || opal_cmd_line_is_taken(orte_info_cmd_line, "config")) {
        orte_info_do_config(true);
        acted = true;
    }
    if (want_all || opal_cmd_line_is_taken(orte_info_cmd_line, "param")) {
        orte_info_do_params(want_all, opal_cmd_line_is_taken(orte_info_cmd_line, "internal"));
        acted = true;
    }
    
    /* If no command line args are specified, show default set */
    
    if (!acted) {
        orte_info_show_orte_version(orte_info_ver_full);
        orte_info_show_path(orte_info_path_prefix, opal_install_dirs.prefix);
        orte_info_do_arch();
        orte_info_do_hostname();
        orte_info_do_config(false);
        orte_info_open_components();
        for (i = 0; i < mca_types.size; ++i) {
            if (NULL == (str = (char*)opal_pointer_array_get_item(&mca_types, i))) {
                continue;
            }
            orte_info_show_component_version(str, orte_info_component_all, 
                                             orte_info_ver_full, orte_info_type_all);
        }
    }
    
    /* All done */
    
    if (NULL != app_env) {
        opal_argv_free(app_env);
    }
    if (NULL != global_env) {
        opal_argv_free(global_env);
    }
    orte_info_close_components();
    OBJ_RELEASE(orte_info_cmd_line);
    OBJ_DESTRUCT(&mca_types);
    mca_base_close();
    
    opal_finalize_util();
    
    return 0;
}
