/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2016-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"

#include <stdio.h>
#include <string.h>
#ifdef HAVE_SYSLOG_H
#    include <syslog.h>
#endif
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif

#include "pmix_common.h"
#include "src/mca/base/pmix_base.h"
#include "src/mca/base/pmix_mca_base_component_repository.h"
#include "src/mca/mca.h"
#include "src/mca/pinstalldirs/pinstalldirs.h"
#include "src/util/pmix_environ.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_os_dirpath.h"
#include "src/util/pmix_printf.h"

/*
 * Public variables
 */
char *pmix_mca_base_component_path = NULL;
int pmix_mca_base_opened = 0;
char *pmix_mca_base_system_default_path = NULL;
char *pmix_mca_base_user_default_path = NULL;
char *pmix_mca_base_component_show_load_errors = NULL;
bool pmix_mca_base_component_abort_on_load_error = false;
bool pmix_mca_base_component_track_load_errors = false;
bool pmix_mca_base_component_disable_dlopen = false;

static char *pmix_mca_base_verbose = NULL;
static char *path_from_param = NULL;

/*
 * Private functions
 */
static void set_defaults(pmix_output_stream_t *lds);
static void parse_verbose(char *e, pmix_output_stream_t *lds);

/*
 * Main MCA initialization.
 */
int pmix_mca_base_open(const char *add_path)
{
#if PMIX_WANT_HOME_CONFIG_FILES
    char *value;
#endif
    char **paths = NULL, *cptr;
    pmix_output_stream_t lds;
    char hostname[PMIX_MAXHOSTNAMELEN] = {0};
    int var_id;
    int rc;

    if (0 < pmix_mca_base_opened) {
        /* allow someone to extend the component search path */
        if (NULL != add_path) {
            /* put the requested added paths at the front */
            if (NULL == pmix_mca_base_component_path) {
                pmix_mca_base_component_path = strdup(add_path);
            } else {
                pmix_asprintf(&cptr, "%s;%s", add_path, pmix_mca_base_component_path);
                free(pmix_mca_base_component_path);
                pmix_mca_base_component_path = cptr;
            }
        }
        pmix_mca_base_opened++;  // track ref count
        return PMIX_SUCCESS;
    }
    pmix_mca_base_opened++;

    /* define the system and user default paths */
    pmix_mca_base_system_default_path = strdup(pmix_pinstall_dirs.pmixlibdir);
    PMIx_Argv_append_nosize(&paths, pmix_mca_base_system_default_path);
#if PMIX_WANT_HOME_CONFIG_FILES
    value = (char *) pmix_home_directory(geteuid());
    pmix_asprintf(&pmix_mca_base_user_default_path,
                  "%s" PMIX_PATH_SEP ".pmix" PMIX_PATH_SEP "components", value);
    PMIx_Argv_append_nosize(&paths, pmix_mca_base_user_default_path);
#endif

    var_id = pmix_mca_base_var_register("pmix", "mca", "base", "component_path",
                                        "Path where to look for additional components",
                                        PMIX_MCA_BASE_VAR_TYPE_STRING,
                                        &path_from_param);
    (void) pmix_mca_base_var_register_synonym(var_id, "pmix", "mca", NULL, "component_path",
                                              PMIX_MCA_BASE_VAR_SYN_FLAG_DEPRECATED);
    if (NULL != path_from_param) {
        PMIx_Argv_append_nosize(&paths, path_from_param);
    }
    cptr = PMIx_Argv_join(paths, PMIX_ENV_SEP);
    PMIx_Argv_free(paths);
    if (NULL != add_path) {
        pmix_asprintf(&pmix_mca_base_component_path, "%s;pmix@%s", add_path, cptr);
    } else {
        pmix_asprintf(&pmix_mca_base_component_path, "pmix@%s", cptr);
    }
    free(cptr);

    pmix_mca_base_component_show_load_errors = PMIX_SHOW_LOAD_ERRORS_DEFAULT;
    var_id = pmix_mca_base_var_register(
                                        "pmix", "mca", "base", "component_show_load_errors",
                                        "Whether to show errors for components that failed to load or not. "
                                        "Valid values are \"all\" (meaning: all load failures are reported), "
                                        "\"none\" (no load failures are reported), or a comma-delimited list "
                                        "of items, each of which can be a framework/component pair or a framework "
                                        "name (only load failures from the specifically-listed items are reported). "
                                        "If the comma-delimited list is prefixed with \"^\", then orientation of "
                                        "the list is negated: warn about all load failures *except* for the listed items.",
                                        PMIX_MCA_BASE_VAR_TYPE_STRING,
                                        &pmix_mca_base_component_show_load_errors);
    (void) pmix_mca_base_var_register_synonym(var_id, "pmix", "mca", NULL,
                                              "component_show_load_errors",
                                              PMIX_MCA_BASE_VAR_SYN_FLAG_DEPRECATED);

    // Parse the mca_base_component_show_load_errors value
    int ret = pmix_mca_base_show_load_errors_init();
    if (PMIX_SUCCESS != ret) {
        return ret;
    }

    pmix_mca_base_component_abort_on_load_error = false;
    var_id = pmix_mca_base_var_register(
        "pmix", "mca", "base", "abort_on_load_error",
        "Whether to abort when a specified component isn't found or cannot be loaded",
        PMIX_MCA_BASE_VAR_TYPE_BOOL,
        &pmix_mca_base_component_abort_on_load_error);


    pmix_mca_base_component_track_load_errors = false;
    var_id = pmix_mca_base_var_register(
        "pmix", "mca", "base", "component_track_load_errors",
        "Whether to track errors for components that failed to load or not",
        PMIX_MCA_BASE_VAR_TYPE_BOOL,
         &pmix_mca_base_component_track_load_errors);

    pmix_mca_base_component_disable_dlopen = false;
    var_id = pmix_mca_base_var_register(
        "pmix", "mca", "base", "component_disable_dlopen",
        "Whether to attempt to disable opening dynamic components or not",
        PMIX_MCA_BASE_VAR_TYPE_BOOL,
        &pmix_mca_base_component_disable_dlopen);
    (void) pmix_mca_base_var_register_synonym(var_id, "pmix", "mca", NULL,
                                              "component_disable_dlopen",
                                              PMIX_MCA_BASE_VAR_SYN_FLAG_DEPRECATED);

    /* What verbosity level do we want for the default 0 stream? */
    pmix_mca_base_verbose = "stderr";
    var_id = pmix_mca_base_var_register(
        "pmix", "mca", "base", "verbose",
        "Specifies where the default error output stream goes (this is separate from distinct help "
        "messages).  Accepts a comma-delimited list of: stderr, stdout, syslog, "
        "syslogpri:<notice|info|debug>, syslogid:<str> (where str is the prefix string for all "
        "syslog notices), file[:filename] (if filename is not specified, a default filename is "
        "used), fileappend (if not specified, the file is opened for truncation), level[:N] (if "
        "specified, integer verbose level; otherwise, 0 is implied)",
        PMIX_MCA_BASE_VAR_TYPE_STRING,
        &pmix_mca_base_verbose);
    (void) pmix_mca_base_var_register_synonym(var_id, "pmix", "mca", NULL, "verbose",
                                              PMIX_MCA_BASE_VAR_SYN_FLAG_DEPRECATED);

    memset(&lds, 0, sizeof(lds));
    if (NULL != pmix_mca_base_verbose) {
        parse_verbose(pmix_mca_base_verbose, &lds);
    } else {
        set_defaults(&lds);
    }
    gethostname(hostname, PMIX_MAXHOSTNAMELEN - 1);
    rc = asprintf(&lds.lds_prefix, "[%s:%05d] ", hostname, getpid());
    if (0 > rc) {
        return PMIX_ERR_OUT_OF_RESOURCE;
    }
    pmix_output_reopen(0, &lds);
    pmix_output_verbose(PMIX_MCA_BASE_VERBOSE_COMPONENT, 0, "mca: base: opening components at %s",
                        pmix_mca_base_component_path);
    free(lds.lds_prefix);

    /* Open up the component repository */

    return pmix_mca_base_component_repository_init();
}

/*
 * Set sane default values for the lds
 */
static void set_defaults(pmix_output_stream_t *lds)
{

    /* Load up defaults */

    PMIX_CONSTRUCT(lds, pmix_output_stream_t);
#if defined(HAVE_SYSLOG) && defined(HAVE_SYSLOG_H)
    lds->lds_syslog_priority = LOG_INFO;
#endif /* defined(HAVE_SYSLOG) && defined(HAVE_SYSLOG_H) */
    lds->lds_syslog_ident = "ompi";
    lds->lds_want_stderr = true;
}

/*
 * Parse the value of an environment variable describing verbosity
 */
static void parse_verbose(char *e, pmix_output_stream_t *lds)
{
    char *edup;
    char *ptr, *next;
    bool have_output = false;

    if (NULL == e) {
        return;
    }

    edup = strdup(e);
    ptr = edup;

    /* Now parse the environment variable */

    while (NULL != ptr && strlen(ptr) > 0) {
        next = strchr(ptr, ',');
        if (NULL != next) {
            *next = '\0';
        }

        if (0 == strcasecmp(ptr, "syslog")) {
#if defined(HAVE_SYSLOG) && defined(HAVE_SYSLOG_H)
            lds->lds_want_syslog = true;
            have_output = true;
#else
            pmix_output(0, "syslog support requested but not available on this system");
#endif /* defined(HAVE_SYSLOG) && defined(HAVE_SYSLOG_H) */
        } else if (strncasecmp(ptr, "syslogpri:", 10) == 0) {
#if defined(HAVE_SYSLOG) && defined(HAVE_SYSLOG_H)
            lds->lds_want_syslog = true;
            have_output = true;
            if (strcasecmp(ptr + 10, "notice") == 0)
                lds->lds_syslog_priority = LOG_NOTICE;
            else if (strcasecmp(ptr + 10, "INFO") == 0)
                lds->lds_syslog_priority = LOG_INFO;
            else if (strcasecmp(ptr + 10, "DEBUG") == 0)
                lds->lds_syslog_priority = LOG_DEBUG;
#else
            pmix_output(0, "syslog support requested but not available on this system");
#endif /* defined(HAVE_SYSLOG) && defined(HAVE_SYSLOG_H) */
        } else if (strncasecmp(ptr, "syslogid:", 9) == 0) {
#if defined(HAVE_SYSLOG) && defined(HAVE_SYSLOG_H)
            lds->lds_want_syslog = true;
            lds->lds_syslog_ident = ptr + 9;
#else
            pmix_output(0, "syslog support requested but not available on this system");
#endif /* defined(HAVE_SYSLOG) && defined(HAVE_SYSLOG_H) */
        }

        else if (strcasecmp(ptr, "stdout") == 0) {
            lds->lds_want_stdout = true;
            have_output = true;
        } else if (strcasecmp(ptr, "stderr") == 0) {
            lds->lds_want_stderr = true;
            have_output = true;
        }

        else if (strcasecmp(ptr, "file") == 0 || strcasecmp(ptr, "file:") == 0) {
            lds->lds_want_file = true;
            have_output = true;
        } else if (strncasecmp(ptr, "file:", 5) == 0) {
            lds->lds_want_file = true;
            lds->lds_file_suffix = strdup(ptr + 5);
            have_output = true;
        } else if (strcasecmp(ptr, "fileappend") == 0) {
            lds->lds_want_file = true;
            lds->lds_want_file_append = 1;
            have_output = true;
        }

        else if (strncasecmp(ptr, "level", 5) == 0) {
            lds->lds_verbose_level = 0;
            if (ptr[5] == PMIX_ENV_SEP)
                lds->lds_verbose_level = atoi(ptr + 6);
        }

        if (NULL == next) {
            break;
        }
        ptr = next + 1;
    }

    /* If we didn't get an output, default to stderr */

    if (!have_output) {
        lds->lds_want_stderr = true;
    }

    /* All done */

    free(edup);
}
