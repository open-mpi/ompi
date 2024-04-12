/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018      Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * Copyright (c) 2018-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      IBM Corporation.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "pmix_config.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#ifdef HAVE_SYS_STAT_H
#    include <sys/stat.h>
#endif /* HAVE_SYS_STAT_H */
#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif /* HAVE_SYS_TYPES_H */
#ifdef HAVE_REGEX_H
#    include <regex.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#    include <sys/wait.h>
#endif /* HAVE_SYS_WAIT_H */
#include <string.h>

#include "pmix_common.h"
#include "src/mca/pinstalldirs/base/base.h"
#include "src/mca/pinstalldirs/pinstalldirs.h"
#include "src/runtime/pmix_rte.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_basename.h"
#include "src/util/pmix_error.h"
#include "src/util/pmix_few.h"
#include "src/util/pmix_keyval_parse.h"
#include "src/util/pmix_os_path.h"
#include "src/util/pmix_path.h"
#include "src/util/pmix_environ.h"
#include "src/util/pmix_printf.h"
#include "src/util/pmix_show_help.h"

#define PMIX_INCLUDE_FLAG "-I"
#define PMIX_LIBDIR_FLAG  "-L"

static const char * filtered_args[] = { "-I/usr/include",
                                        "-L/usr/lb",
                                        "-L/usr/lib64",
                                        NULL };

/* This structure is used to represent the config file read in during
 * initialization.  A single file may have multiple options_data
 * structures, with the compiler_args keying on which section to
 * use in returning results. */
struct options_data_t {
    /* compiler_args is a key to chose the right block in the case
     * that there are multiple blocks in a single file.  All
     * arguments in compiler_args in must appear in the compiler's argv
     * in order for this block to be selected. */
    char **compiler_args;
    /* language for this compiler.  This field is only used for
     * pretty-printing messages.  It does not have any impact on the
     * behavior of the wrapper compiler. */
    char *language;
    /* project name.  This field is only used for pretty-printing
     * messages.  It does not have any impact on the behavior of the
     * wrapper compiler. */
    char *project;
    /* project short name.  The environment variables to influence
     * setting of compiler flags are named <project_short>_{CPPFLAGS,
     * LDFLAGS, LIBS}. */
    char *project_short;
    /* project release version.  Used to pretty-print help strings. */
    char *version;
    /* Second part of the variable name to change the underlying
     * compiler the wrapper compiler invokes.  For example, for C,
     * compiler_env should be CC, resulting in <project_short>_CC
     * being the environment variable used to change the underlying
     * compiler that is invoked. */
    char *compiler_env;
    /* Second part of the variable name to change the compiler flags
     * passed to the underlying compiler.  For example, for C,
     * compiler_flags_env should be CFLAGS, resulting in
     * <project_short>_CFLAGS being the environment variable used to
     * change the CFLAGS passed to the underlying compiler. */
    char *compiler_flags_env;
    /* Default underlying compiler to invoke */
    char *compiler;
    /* Default preprocessor (ie, CPPFLAGS) to pass to underlying
     * compiler.  These are only passed to the underlying compiler
     * when it looks like we are compiling (ie, not in a link-only
     * situation). */
    char **preproc_flags;
    /* Default compiler flags (ie, CFLAGS) to pass to the underlying
     * compiler. */
    char **comp_flags;
    /* Compiler flags to pass to the underlying compiler before
     * comp_flags.  These args are generally passed directly from
     * configure options provided from the user. */
    char **comp_flags_prefix;
    /* Linker flags to pass to to the underlying compiler when it
     * appears that we are linking.  These flags will be passed to the
     * underlying compiler whether or not we see -static (or friends) in
     * the arguments. */
    char **link_flags;
    /* Linker flags to pass to to the underlying compiler when it
     * appears that we are linking.  These flags ONLY will be passed
     * to the underlying compiler when we see -static (or friends) in
     * the arguments. */
    char **link_flags_static;
    /* Libs flags to pass to to the underlying compiler when it
     * appears that we are linking.  These flags will be passed to the
     * underlying compiler whether or not we see -static (or friends)
     * in the arguments. */
    char **libs;
    /* Libs flags to pass to to the underlying compiler when it
     * appears that we are linking.  These flags ONLY will be passed
     * to the underlying compiler when we see -static (or friends) in
     * the arguments. */
    char **libs_static;
    /* Name of a file that should exist in ${libdir} if shared
     * libraries were properly installed.  Historically, this was
     * used to determine what flags should be added to the compiler.
     * Now it is not used. */
    char *dyn_lib_file;
    /* Name of a file that should exist in ${libdir} if static
     * libraries were properly installed.  Historically, this was
     * used to determine what flags should be added to the compiler.
     * Now it is not used. */
    char *static_lib_file;
    /* A (full path) of a file that must exist for the wrapper
     * compiler to succeed.  A common use case is to specify a
     * language-binding specific file (for example, a C++ header file)
     * when invoking an optional language binding. */
    char *req_file;
    /* Default includedir, before variable expansion.  Almost always
     * set as ${includedir} outside of multilib situations. */
    char *path_includedir;
    /* Default libdir, before variable expansion.  Almost always set
    as ${libdir} outside of multilib situations */
    char *path_libdir;
};

static struct options_data_t *options_data = NULL;
/* index used by parser */
static int parse_options_idx = -1;
/* index of options specified by user */
static int user_data_idx = -1;
/* index of options to use by default */
static int default_data_idx = -1;

#define COMP_DRY_RUN      0x001
#define COMP_SHOW_ERROR   0x002
#define COMP_WANT_COMMAND 0x004
#define COMP_WANT_PREPROC 0x008
#define COMP_WANT_COMPILE 0x010
#define COMP_WANT_LINK    0x020
#define COMP_WANT_PMPI    0x040
#define COMP_WANT_STATIC  0x080

static void options_data_init(struct options_data_t *data)
{
    data->compiler_args = (char **) malloc(sizeof(char *));
    data->compiler_args[0] = NULL;
    data->language = NULL;
    data->compiler = NULL;
    data->project = NULL;
    data->project_short = NULL;
    data->version = NULL;
    data->compiler_env = NULL;
    data->compiler_flags_env = NULL;
    data->preproc_flags = (char **) malloc(sizeof(char *));
    data->preproc_flags[0] = NULL;
    data->comp_flags = (char **) malloc(sizeof(char *));
    data->comp_flags[0] = NULL;
    data->comp_flags_prefix = (char **) malloc(sizeof(char *));
    data->comp_flags_prefix[0] = NULL;
    data->link_flags = (char **) malloc(sizeof(char *));
    data->link_flags[0] = NULL;
    data->link_flags_static = (char **) malloc(sizeof(char *));
    data->link_flags_static[0] = NULL;
    data->libs = (char **) malloc(sizeof(char *));
    data->libs[0] = NULL;
    data->libs_static = (char **) malloc(sizeof(char *));
    data->libs_static[0] = NULL;
    data->dyn_lib_file = NULL;
    data->static_lib_file = NULL;
    data->req_file = NULL;
    data->path_includedir = NULL;
    data->path_libdir = NULL;
}

static void options_data_free(struct options_data_t *data)
{
    if (NULL != data->compiler_args) {
        PMIx_Argv_free(data->compiler_args);
    }
    if (NULL != data->language)
        free(data->language);
    if (NULL != data->compiler)
        free(data->compiler);
    if (NULL != data->project)
        free(data->project);
    if (NULL != data->project_short)
        free(data->project_short);
    if (NULL != data->version)
        free(data->version);
    if (NULL != data->compiler_env)
        free(data->compiler_env);
    if (NULL != data->compiler_flags_env)
        free(data->compiler_flags_env);
    PMIx_Argv_free(data->preproc_flags);
    PMIx_Argv_free(data->comp_flags);
    PMIx_Argv_free(data->comp_flags_prefix);
    PMIx_Argv_free(data->link_flags);
    PMIx_Argv_free(data->link_flags_static);
    PMIx_Argv_free(data->libs);
    PMIx_Argv_free(data->libs_static);
    if (NULL != data->dyn_lib_file)
        free(data->dyn_lib_file);
    if (NULL != data->static_lib_file)
        free(data->static_lib_file);
    if (NULL != data->req_file)
        free(data->req_file);
    if (NULL != data->path_includedir)
        free(data->path_includedir);
    if (NULL != data->path_libdir)
        free(data->path_libdir);
}

static void options_data_expand(const char *value)
{
    /* make space for the new set of args */
    parse_options_idx++;
    options_data = (struct options_data_t *) realloc(options_data, sizeof(struct options_data_t)
                                                                       * (parse_options_idx + 1));
    options_data_init(&(options_data[parse_options_idx]));

    /* if there are values, this is not the default case.
       Otherwise, it's the default case... */
    if (NULL != value && 0 != strcmp(value, "")) {
        char **values = PMIx_Argv_split(value, ';');
        pmix_argv_insert(&(options_data[parse_options_idx].compiler_args),
                         PMIx_Argv_count(options_data[parse_options_idx].compiler_args), values);
        PMIx_Argv_free(values);
    } else {
        free(options_data[parse_options_idx].compiler_args);
        options_data[parse_options_idx].compiler_args = NULL;
        /* this is a default */
        default_data_idx = parse_options_idx;
    }
}

static int find_options_index(const char *arg)
{
    int i, j;
#ifdef HAVE_REGEXEC
    int args_count;
    regex_t res;
#endif

    for (i = 0; i <= parse_options_idx; ++i) {
        if (NULL == options_data[i].compiler_args) {
            continue;
        }

#ifdef HAVE_REGEXEC
        args_count = PMIx_Argv_count(options_data[i].compiler_args);
        for (j = 0; j < args_count; ++j) {
            if (0 != regcomp(&res, options_data[i].compiler_args[j], REG_NOSUB)) {
                return -1;
            }

            if (0 == regexec(&res, arg, (size_t) 0, NULL, 0)) {
                regfree(&res);
                return i;
            }

            regfree(&res);
        }
#else
        for (j = 0; j < PMIx_Argv_count(options_data[i].compiler_args); ++j) {
            if (0 == strcmp(arg, options_data[i].compiler_args[j])) {
                return i;
            }
        }
#endif
    }

    return -1;
}

static void expand_flags(char **argv)
{
    int i;
    char *tmp;

    for (i = 0; argv[i] != NULL; ++i) {
        tmp = pmix_pinstall_dirs_expand(argv[i]);
        if (tmp != argv[i]) {
            free(argv[i]);
            argv[i] = tmp;
        }
    }
}

static void filter_flags(char*** argvp)
{
    int argc;
    int idx;

    argc = PMIx_Argv_count(*argvp);
    idx = 0;

    while (idx < argc) {
        char *arg = (*argvp)[idx];
        size_t j = 0;
        bool found = false;

        while (filtered_args[j] != NULL) {
            if (0 == strcmp(arg, filtered_args[j])) {
                pmix_argv_delete(&argc, argvp, idx, 1);
                found = true;
                break;
            }
            j++;
        }

        if (!found) {
            /* skip moving the idx pointer if we found an entry (since
             * the previous idx + 1 entry is the new idx pointer).  Note
             * that argc might have moved, so the outer while loop make still
             * give a different answer to its conditional. */
            idx++;
        }
    }

}

static void data_callback(const char *file, int lineno,
                          const char *key, const char *value)
{
    PMIX_HIDE_UNUSED_PARAMS(file, lineno);

    /* handle case where text file does not contain any special
     compiler options field */
    if (parse_options_idx < 0 && 0 != strcmp(key, "compiler_args")) {
        options_data_expand(NULL);
    }

    if (0 == strcmp(key, "compiler_args")) {
        options_data_expand(value);
    } else if (0 == strcmp(key, "language")) {
        if (NULL != value)
            options_data[parse_options_idx].language = strdup(value);
    } else if (0 == strcmp(key, "compiler")) {
        if (NULL != value)
            options_data[parse_options_idx].compiler = strdup(value);
    } else if (0 == strcmp(key, "project")) {
        if (NULL != value)
            options_data[parse_options_idx].project = strdup(value);
    } else if (0 == strcmp(key, "version")) {
        if (NULL != value)
            options_data[parse_options_idx].version = strdup(value);
    } else if (0 == strcmp(key, "preprocessor_flags")) {
        char **values = PMIx_Argv_split(value, ' ');
        pmix_argv_insert(&options_data[parse_options_idx].preproc_flags,
                         PMIx_Argv_count(options_data[parse_options_idx].preproc_flags), values);
        expand_flags(options_data[parse_options_idx].preproc_flags);
        filter_flags(&options_data[parse_options_idx].preproc_flags);
        PMIx_Argv_free(values);
    } else if (0 == strcmp(key, "compiler_flags")) {
        char **values = PMIx_Argv_split(value, ' ');
        pmix_argv_insert(&options_data[parse_options_idx].comp_flags,
                         PMIx_Argv_count(options_data[parse_options_idx].comp_flags), values);
        expand_flags(options_data[parse_options_idx].comp_flags);
        filter_flags(&options_data[parse_options_idx].comp_flags);
        PMIx_Argv_free(values);
    } else if (0 == strcmp(key, "compiler_flags_prefix")) {
        char **values = PMIx_Argv_split(value, ' ');
        pmix_argv_insert(&options_data[parse_options_idx].comp_flags_prefix,
                         PMIx_Argv_count(options_data[parse_options_idx].comp_flags_prefix),
                         values);
        expand_flags(options_data[parse_options_idx].comp_flags_prefix);
        PMIx_Argv_free(values);
    } else if (0 == strcmp(key, "linker_flags")) {
        char **values = PMIx_Argv_split(value, ' ');
        pmix_argv_insert(&options_data[parse_options_idx].link_flags,
                         PMIx_Argv_count(options_data[parse_options_idx].link_flags), values);
        expand_flags(options_data[parse_options_idx].link_flags);
        filter_flags(&options_data[parse_options_idx].link_flags);
        PMIx_Argv_free(values);
    } else if (0 == strcmp(key, "linker_flags_static")) {
        char **values = PMIx_Argv_split(value, ' ');
        pmix_argv_insert(&options_data[parse_options_idx].link_flags_static,
                         PMIx_Argv_count(options_data[parse_options_idx].link_flags_static), values);
        expand_flags(options_data[parse_options_idx].link_flags_static);
        filter_flags(&options_data[parse_options_idx].link_flags_static);
        PMIx_Argv_free(values);
    } else if (0 == strcmp(key, "libs")) {
        char **values = PMIx_Argv_split(value, ' ');
        pmix_argv_insert(&options_data[parse_options_idx].libs,
                         PMIx_Argv_count(options_data[parse_options_idx].libs), values);
        PMIx_Argv_free(values);
    } else if (0 == strcmp(key, "libs_static")) {
        char **values = PMIx_Argv_split(value, ' ');
        pmix_argv_insert(&options_data[parse_options_idx].libs_static,
                         PMIx_Argv_count(options_data[parse_options_idx].libs_static), values);
        PMIx_Argv_free(values);
    } else if (0 == strcmp(key, "dyn_lib_file")) {
        if (NULL != value)
            options_data[parse_options_idx].dyn_lib_file = strdup(value);
    } else if (0 == strcmp(key, "static_lib_file")) {
        if (NULL != value)
            options_data[parse_options_idx].static_lib_file = strdup(value);
    } else if (0 == strcmp(key, "required_file")) {
        if (NULL != value)
            options_data[parse_options_idx].req_file = strdup(value);
    } else if (0 == strcmp(key, "project_short")) {
        if (NULL != value)
            options_data[parse_options_idx].project_short = strdup(value);
    } else if (0 == strcmp(key, "compiler_env")) {
        if (NULL != value)
            options_data[parse_options_idx].compiler_env = strdup(value);
    } else if (0 == strcmp(key, "compiler_flags_env")) {
        if (NULL != value)
            options_data[parse_options_idx].compiler_flags_env = strdup(value);
    } else if (0 == strcmp(key, "includedir")) {
        if (NULL != value) {
            options_data[parse_options_idx].path_includedir = pmix_pinstall_dirs_expand(value);
        }
    } else if (0 == strcmp(key, "libdir")) {
        if (NULL != value) {
            options_data[parse_options_idx].path_libdir = pmix_pinstall_dirs_expand(value);
        }
    }
}

static int data_init(void)
{
    int ret;
    char *datafile;

    /* now load the data */
    pmix_asprintf(&datafile, "%s%spmixcc-wrapper-data.txt", pmix_pinstall_dirs.pmixdatadir,
                  PMIX_PATH_SEP);
    if (NULL == datafile)
        return PMIX_ERR_OUT_OF_RESOURCE;

    ret = pmix_util_keyval_parse(datafile, data_callback);
    if (PMIX_SUCCESS != ret) {
        fprintf(stderr, "Cannot open configuration file %s\n", datafile);
    }
    free(datafile);

    return ret;
}

static int data_finalize(void)
{
    int i;

    for (i = 0; i <= parse_options_idx; ++i) {
        options_data_free(&(options_data[i]));
    }
    free(options_data);

    return PMIX_SUCCESS;
}


/*
 * Print the flags in args, stripping off pattern (which must be a
 * simple exact match string, no regex) from the front of every
 * argument in args.
 */
static void print_flags(char **args, char *pattern)
{
    int i;
    bool found = false;

    for (i = 0; args[i] != NULL; ++i) {
        if (0 == strncmp(args[i], pattern, strlen(pattern))) {
            if (found)
                printf(" ");
            printf("%s", args[i] + strlen(pattern));
            found = true;
        }
    }

    if (found)
        printf("\n");
}

static void load_env_data(const char *project, const char *flag, char **data)
{
    char *envname;
    char *envvalue;

    if (NULL == project || NULL == flag)
        return;

    pmix_asprintf(&envname, "%s_%s", project, flag);
    if (NULL == (envvalue = getenv(envname))) {
        free(envname);
        return;
    }
    free(envname);

    if (NULL != *data) {
        free(*data);
    }
    *data = strdup(envvalue);
}

static void load_env_data_argv(const char *project, const char *flag, char ***data)
{
    char *envname;
    char *envvalue;

    if (NULL == project || NULL == flag)
        return;

    pmix_asprintf(&envname, "%s_%s", project, flag);
    if (NULL == (envvalue = getenv(envname))) {
        free(envname);
        return;
    }
    free(envname);

    if (NULL != *data)
        PMIx_Argv_free(*data);

    *data = PMIx_Argv_split(envvalue, ' ');
}

int main(int argc, char *argv[])
{
    int exit_status = 0, ret, flags = 0, i;
    int exec_argc = 0, user_argc = 0;
    char **exec_argv = NULL, **user_argv = NULL;
    char *exec_command, *base_argv0 = NULL;
    bool disable_flags = true;
    bool real_flag = false;

    /* initialize the output system */
    if (!pmix_output_init()) {
        return PMIX_ERROR;
    }

    /* initialize install dirs code */
    if (PMIX_SUCCESS != (ret = pmix_mca_base_framework_open(&pmix_pinstalldirs_base_framework,
                                               PMIX_MCA_BASE_OPEN_DEFAULT))) {
        fprintf(stderr,
                "pmix_pinstalldirs_base_open() failed -- process will likely abort (%s:%d, "
                "returned %d instead of PMIX_SUCCESS)\n",
                __FILE__, __LINE__, ret);
        return ret;
    }

    if (PMIX_SUCCESS != (ret = pmix_pinstall_dirs_base_init(NULL, 0))) {
        fprintf(stderr,
                "pmix_pinstalldirs_base_init() failed -- process will likely abort (%s:%d, "
                "returned %d instead of PMIX_SUCCESS)\n",
                __FILE__, __LINE__, ret);
        return ret;
    }

    /* initialize the help system */
    pmix_show_help_init(NULL);

    /* keyval lex-based parser */
    if (PMIX_SUCCESS != (ret = pmix_util_keyval_parse_init())) {
        pmix_show_help("help-pmix-runtime.txt", "pmix_init:startup:internal-failure", true,
                       "pmix_util_keyval_parse_init", ret);
        return ret;
    }

    /* initialize the mca */
    if (PMIX_SUCCESS != (ret = pmix_mca_base_open(NULL))) {
        pmix_show_help("help-pmix-runtime.txt", "pmix_init:startup:internal-failure", true,
                       "pmix_mca_base_open", ret);
        return ret;
    }

    /****************************************************
     *
     * Setup compiler information
     *
     ****************************************************/

    base_argv0 = pmix_basename(argv[0]);
#if defined(EXEEXT)
    if (0 != strlen(EXEEXT)) {
        char extension[] = EXEEXT;
        char *temp = strstr(base_argv0, extension);
        char *old_match = temp;
        while (NULL != temp) {
            old_match = temp;
            temp = strstr(temp + 1, extension);
        }
        /* Only if there was a match of .exe, erase the last occurrence of .exe */
        if (NULL != old_match) {
            *old_match = '\0';
        }
    }
#endif /* defined(EXEEXT) */

    if (PMIX_SUCCESS != (ret = data_init())) {
        fprintf(stderr, "Error parsing data file %s: %s\n", base_argv0, PMIx_Error_string(ret));
        return ret;
    }

    for (i = 1; i < argc && user_data_idx < 0; ++i) {
        user_data_idx = find_options_index(argv[i]);
    }
    /* if we didn't find a match, look for the NULL (base case) options */
    if (user_data_idx < 0) {
        user_data_idx = default_data_idx;
    }
    /* if we still didn't find a match, abort */
    if (user_data_idx < 0) {
        char *flat = PMIx_Argv_join(argv, ' ');
        pmix_show_help("help-pmixcc.txt", "no-options-support", true, base_argv0, flat, NULL);
        free(flat);
        exit(1);
    }

    /* compiler */
    load_env_data(options_data[user_data_idx].project_short,
                  options_data[user_data_idx].compiler_env, &options_data[user_data_idx].compiler);

    /* preprocessor flags */
    load_env_data_argv(options_data[user_data_idx].project_short, "CPPFLAGS",
                       &options_data[user_data_idx].preproc_flags);

    /* compiler flags */
    load_env_data_argv(options_data[user_data_idx].project_short,
                       options_data[user_data_idx].compiler_flags_env,
                       &options_data[user_data_idx].comp_flags);

    /* linker flags */
    load_env_data_argv(options_data[user_data_idx].project_short, "LDFLAGS",
                       &options_data[user_data_idx].link_flags);

    /* libs */
    load_env_data_argv(options_data[user_data_idx].project_short, "LIBS",
                       &options_data[user_data_idx].libs);

    /****************************************************
     *
     * Sanity Checks
     *
     ****************************************************/

    if (NULL != options_data[user_data_idx].req_file) {
        /* make sure the language is supported */
        if (0 == strcmp(options_data[user_data_idx].req_file, "not supported")) {
            pmix_show_help("help-pmixcc.txt", "no-language-support", true,
                           options_data[user_data_idx].language, base_argv0, NULL);
            exit_status = 1;
            goto cleanup;
        }

        if (options_data[user_data_idx].req_file[0] != '\0') {
            char *filename;
            struct stat buf;
            filename = pmix_os_path(false, options_data[user_data_idx].path_libdir,
                                    options_data[user_data_idx].req_file, NULL);
            if (0 != stat(filename, &buf)) {
                pmix_show_help("help-pmixcc.txt", "file-not-found", true, base_argv0,
                               options_data[user_data_idx].req_file,
                               options_data[user_data_idx].language, NULL);
            }
        }
    }

    /****************************************************
     *
     * Parse user flags
     *
     ****************************************************/
    flags = COMP_WANT_COMMAND | COMP_WANT_PREPROC | COMP_WANT_COMPILE | COMP_WANT_LINK;

    user_argv = PMIx_Argv_copy(argv + 1);
    user_argc = PMIx_Argv_count(user_argv);

    for (i = 0; i < user_argc; ++i) {
        if (0 == strncmp(user_argv[i], "-showme", strlen("-showme"))
            || 0 == strncmp(user_argv[i], "--showme", strlen("--showme"))
            || 0 == strncmp(user_argv[i], "-show", strlen("-show"))
            || 0 == strncmp(user_argv[i], "--show", strlen("--show"))) {
            bool done_now = false;

            /* check for specific things we want to see.  First three
               still invoke all the building routines.  Last set want
               to parse out certain flags, so we don't go through the
               normal build routine - skip to cleanup. */
            if (0 == strncmp(user_argv[i], "-showme:command", strlen("-showme:command"))
                || 0 == strncmp(user_argv[i], "--showme:command", strlen("--showme:command"))) {
                flags = COMP_WANT_COMMAND;
                /* we know what we want, so don't process any more args */
                done_now = true;
            } else if (0 == strncmp(user_argv[i], "-showme:compile", strlen("-showme:compile"))
                       || 0
                              == strncmp(user_argv[i], "--showme:compile",
                                         strlen("--showme:compile"))) {
                flags = COMP_WANT_PREPROC | COMP_WANT_COMPILE;
                /* we know what we want, so don't process any more args */
                done_now = true;
            } else if (0 == strncmp(user_argv[i], "-showme:link", strlen("-showme:link"))
                       || 0 == strncmp(user_argv[i], "--showme:link", strlen("--showme:link"))) {
                flags = COMP_WANT_COMPILE | COMP_WANT_LINK;
                /* we know what we want, so don't process any more args */
                done_now = true;
            } else if (0 == strncmp(user_argv[i], "-showme:incdirs", strlen("-showme:incdirs"))
                       || 0
                              == strncmp(user_argv[i], "--showme:incdirs",
                                         strlen("--showme:incdirs"))) {
                print_flags(options_data[user_data_idx].preproc_flags, PMIX_INCLUDE_FLAG);
                goto cleanup;
            } else if (0 == strncmp(user_argv[i], "-showme:libdirs_static", strlen("-showme:libdirs_static"))
                       || 0
                              == strncmp(user_argv[i], "--showme:libdirs_static",
                                         strlen("--showme:libdirs_static"))) {
                char **all_args = NULL;
                int args_count;

                all_args = PMIx_Argv_copy(options_data[user_data_idx].link_flags);
                args_count = PMIx_Argv_count(all_args);
                pmix_argv_insert(&all_args, args_count, options_data[user_data_idx].link_flags_static);
                print_flags(all_args, PMIX_LIBDIR_FLAG);
                PMIx_Argv_free(all_args);
                goto cleanup;
            } else if (0 == strncmp(user_argv[i], "-showme:libdirs", strlen("-showme:libdirs"))
                       || 0
                              == strncmp(user_argv[i], "--showme:libdirs",
                                         strlen("--showme:libdirs"))) {
                print_flags(options_data[user_data_idx].link_flags, PMIX_LIBDIR_FLAG);
                goto cleanup;
            } else if (0 == strncmp(user_argv[i], "-showme:libs_static", strlen("-showme:libs_static"))
                       || 0 == strncmp(user_argv[i], "--showme:libs_static", strlen("--showme:libs_static"))) {
                char **all_args = NULL;
                int args_count;

                all_args = PMIx_Argv_copy(options_data[user_data_idx].libs);
                args_count = PMIx_Argv_count(all_args);
                pmix_argv_insert(&all_args, args_count, options_data[user_data_idx].libs_static);
                print_flags(all_args, "-l");
                PMIx_Argv_free(all_args);
                goto cleanup;
            } else if (0 == strncmp(user_argv[i], "-showme:libs", strlen("-showme:libs"))
                       || 0 == strncmp(user_argv[i], "--showme:libs", strlen("--showme:libs"))) {
                print_flags(options_data[user_data_idx].libs, "-l");
                goto cleanup;
            } else if (0 == strncmp(user_argv[i], "-showme:version", strlen("-showme:version"))
                       || 0
                              == strncmp(user_argv[i], "--showme:version",
                                         strlen("--showme:version"))) {
                char *str;
                str = pmix_show_help_string("help-pmixcc.txt", "version", false, argv[0],
                                            options_data[user_data_idx].project,
                                            options_data[user_data_idx].version,
                                            options_data[user_data_idx].language, NULL);
                if (NULL != str) {
                    printf("%s", str);
                    free(str);
                }
                goto cleanup;
            } else if (0 == strncmp(user_argv[i], "-showme:help", strlen("-showme:help"))
                       || 0 == strncmp(user_argv[i], "--showme:help", strlen("--showme:help"))) {
                char *str;
                str = pmix_show_help_string("help-pmixcc.txt", "usage", false, argv[0],
                                            options_data[user_data_idx].project, NULL);
                if (NULL != str) {
                    printf("%s", str);
                    free(str);
                }

                exit_status = 0;
                goto cleanup;
            } else if (0 == strncmp(user_argv[i], "-showme:", strlen("-showme:"))
                       || 0 == strncmp(user_argv[i], "--showme:", strlen("--showme:"))) {
                fprintf(stderr, "%s: unrecognized option: %s\n", argv[0], user_argv[i]);
                fprintf(stderr, "Type '%s --showme:help' for usage.\n", argv[0]);
                exit_status = 1;
                goto cleanup;
            }

            flags |= (COMP_DRY_RUN | COMP_SHOW_ERROR);
            /* remove element from user_argv */
            pmix_argv_delete(&user_argc, &user_argv, i, 1);
            --i;

            if (done_now) {
                disable_flags = false;
                break;
            }

        } else if (0 == strcmp(user_argv[i], "-c")) {
            flags &= ~COMP_WANT_LINK;
            real_flag = true;
        } else if (0 == strcmp(user_argv[i], "-E") || 0 == strcmp(user_argv[i], "-M")) {
            flags &= ~(COMP_WANT_COMPILE | COMP_WANT_LINK);
            real_flag = true;
        } else if (0 == strcmp(user_argv[i], "-S")) {
            flags &= ~COMP_WANT_LINK;
            real_flag = true;
        } else if (0 == strcmp(user_argv[i], "-static") || 0 == strcmp(user_argv[i], "--static")
                   || 0 == strcmp(user_argv[i], "-Bstatic")
                   || 0 == strcmp(user_argv[i], "-Wl,-static")
                   || 0 == strcmp(user_argv[i], "-Wl,--static")
                   || 0 == strcmp(user_argv[i], "-Wl,-Bstatic")) {
            flags |= COMP_WANT_STATIC;
        } else if (0 == strcmp(user_argv[i], "-dynamic") || 0 == strcmp(user_argv[i], "--dynamic")
                   || 0 == strcmp(user_argv[i], "-Bdynamic")
                   || 0 == strcmp(user_argv[i], "-Wl,-dynamic")
                   || 0 == strcmp(user_argv[i], "-Wl,--dynamic")
                   || 0 == strcmp(user_argv[i], "-Wl,-Bdynamic")) {
            flags &= ~COMP_WANT_STATIC;
        } else if ('-' != user_argv[i][0]) {
            disable_flags = false;
            flags |= COMP_SHOW_ERROR;
            real_flag = true;
        } else {
            /* if the option flag is one that we use to determine
               which set of compiler data to use, don't count it as a
               real option */
            if (find_options_index(user_argv[i]) < 0) {
                real_flag = true;
            }
        }
    }

    /* clear out the want_flags if we got no arguments not starting
       with a - (dash) and -showme wasn't given OR -showme was given
       and we had at least one more non-showme argument that started
       with a - (dash) and no other non-dash arguments.  Some examples:

       pmix_wrapper                : clear our flags
       pmix_wrapper -v             : clear our flags
       pmix_wrapper -E a.c         : don't clear our flags
       pmix_wrapper a.c            : don't clear our flags
       pmix_wrapper -showme        : don't clear our flags
       pmix_wrapper -showme -v     : clear our flags
       pmix_wrapper -showme -E a.c : don't clear our flags
       pmix_wrapper -showme a.c    : don't clear our flags
    */
    if (disable_flags && !((flags & COMP_DRY_RUN) && !real_flag)) {
        flags &= ~(COMP_WANT_PREPROC | COMP_WANT_COMPILE | COMP_WANT_LINK);
    }

    /****************************************************
     *
     * Assemble the command line
     *
     ****************************************************/

    /* compiler (may be multiple arguments, so split) */
    if (flags & COMP_WANT_COMMAND) {
        exec_argv = PMIx_Argv_split(options_data[user_data_idx].compiler, ' ');
        exec_argc = PMIx_Argv_count(exec_argv);
    } else {
        exec_argv = (char **) malloc(sizeof(char *));
        exec_argv[0] = NULL;
        exec_argc = 0;
    }

    /* This error would normally not happen unless the user edits the
       wrapper data files manually */
    if (NULL == exec_argv) {
        pmix_show_help("help-pmixcc.txt", "no-compiler-specified", true);
        return 1;
    }

    if (flags & COMP_WANT_COMPILE) {
        pmix_argv_insert(&exec_argv, exec_argc, options_data[user_data_idx].comp_flags_prefix);
        exec_argc = PMIx_Argv_count(exec_argv);
    }

    /* Per https://svn.open-mpi.org/trac/ompi/ticket/2201, add all the
       user arguments before anything else. */
    pmix_argv_insert(&exec_argv, exec_argc, user_argv);
    exec_argc = PMIx_Argv_count(exec_argv);

    /* preproc flags */
    if (flags & COMP_WANT_PREPROC) {
        pmix_argv_insert(&exec_argv, exec_argc, options_data[user_data_idx].preproc_flags);
        exec_argc = PMIx_Argv_count(exec_argv);
    }

    /* compiler flags */
    if (flags & COMP_WANT_COMPILE) {
        pmix_argv_insert(&exec_argv, exec_argc, options_data[user_data_idx].comp_flags);
        exec_argc = PMIx_Argv_count(exec_argv);
    }

    /* link flags and libs */
    if (flags & COMP_WANT_LINK) {
        /* Configure will set the libs, libs_static, link_flags, and
           link_flags_static based on whether or not shared and static
           libraries are enabled (see the large comment in
           pmix_setup_wrappers.m4).  The wrapper will always add libs
           and link_flags in a link situation, and should add the
           _static variants if -static was seen. */
        pmix_argv_insert(&exec_argv, exec_argc, options_data[user_data_idx].link_flags);
        exec_argc = PMIx_Argv_count(exec_argv);
        if (flags & COMP_WANT_STATIC) {
            pmix_argv_insert(&exec_argv, exec_argc, options_data[user_data_idx].link_flags_static);
            exec_argc = PMIx_Argv_count(exec_argv);
        }
        pmix_argv_insert(&exec_argv, exec_argc, options_data[user_data_idx].libs);
        exec_argc = PMIx_Argv_count(exec_argv);
        if (flags & COMP_WANT_STATIC) {
            pmix_argv_insert(&exec_argv, exec_argc, options_data[user_data_idx].libs_static);
            exec_argc = PMIx_Argv_count(exec_argv);
        }
    }

    /****************************************************
     *
     * Execute the command
     *
     ****************************************************/

    if (flags & COMP_DRY_RUN) {
        exec_command = PMIx_Argv_join(exec_argv, ' ');
        printf("%s\n", exec_command);
    } else {
        char *tmp;

#if 0
        exec_command = PMIx_Argv_join(exec_argv, ' ');
        printf("command: %s\n", exec_command);
#endif

        tmp = pmix_path_findv(exec_argv[0], 0, environ, NULL);
        if (NULL == tmp) {
            pmix_show_help("help-pmixcc.txt", "no-compiler-found", true, exec_argv[0], NULL);
            errno = 0;
            exit_status = 1;
        } else {
            int status;

            free(exec_argv[0]);
            exec_argv[0] = tmp;
            ret = pmix_few(exec_argv, &status);
            exit_status = WIFEXITED(status) ? WEXITSTATUS(status)
                                            : (WIFSIGNALED(status)
                                                   ? WTERMSIG(status)
                                                   : (WIFSTOPPED(status) ? WSTOPSIG(status) : 255));
            if ((PMIX_SUCCESS != ret) || ((0 != exit_status) && (flags & COMP_SHOW_ERROR))) {
                char *myexec_command = PMIx_Argv_join(exec_argv, ' ');
                if (PMIX_SUCCESS != ret) {
                    pmix_show_help("help-pmixcc.txt", "spawn-failed", true, exec_argv[0],
                                   strerror(status), myexec_command, NULL);
                } else {
#if 0
                    pmix_show_help("help-pmixcc.txt", "compiler-failed", true,
                                   exec_argv[0], exit_status, myexec_command, NULL);
#endif
                }
                free(myexec_command);
            }
        }
    }

    /****************************************************
     *
     * Cleanup
     *
     ****************************************************/
cleanup:

    PMIx_Argv_free(exec_argv);
    PMIx_Argv_free(user_argv);
    if (NULL != base_argv0)
        free(base_argv0);

    if (PMIX_SUCCESS != (ret = data_finalize())) {
        return ret;
    }

    /* keyval lex-based parser */
    pmix_util_keyval_parse_finalize();

    (void) pmix_mca_base_framework_close(&pmix_pinstalldirs_base_framework);
    pmix_mca_base_close();
    /* finalize the show_help system */
    pmix_show_help_finalize();

    /* finalize the output system.  This has to come *after* the
       malloc code, as the malloc code needs to call into this, but
       the malloc code turning off doesn't affect pmix_output that
       much */
    pmix_output_finalize();

    return exit_status;
}
