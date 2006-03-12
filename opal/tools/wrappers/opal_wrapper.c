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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include <stdio.h>
#include <errno.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif  /*  HAVE_STDLIB_H */
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_LIBGEN_H
#include <libgen.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */

#include "opal/install_dirs.h"
#include "opal/runtime/opal.h"
#include "opal/constants.h"
#include "opal/util/argv.h"
#include "opal/util/keyval_parse.h"
#include "opal/util/show_help.h"
#include "opal/util/path.h"
#include "opal/util/few.h"

extern char **environ;

struct {
    char *language;
    char *project;
    char *project_short;
    char *version;
    char *compiler_env;
    char *compiler_flags_env;
    char *compiler;
    char *module_option;
    char **preproc_flags;
    char **comp_flags;
    char **link_flags;
    char **libs;
    char *req_file;
} data;

#define COMP_DRY_RUN       0x001
#define COMP_SHOW_ERROR    0x002
#define COMP_WANT_COMMAND  0x004
#define COMP_WANT_PREPROC  0x008
#define COMP_WANT_COMPILE  0x010
#define COMP_WANT_LINK     0x020
#define COMP_WANT_PMPI     0x040

static void
data_callback(const char *key, const char *value)
{
    if (0 == strcmp(key, "language")) {
        if (NULL != value) data.language = strdup(value);
    } else if (0 == strcmp(key, "compiler")) {
        if (NULL != value) data.compiler = strdup(value);
    } else if (0 == strcmp(key, "project")) {
        if (NULL != value) data.project = strdup(value);
    } else if (0 == strcmp(key, "version")) {
        if (NULL != value) data.version = strdup(value);
    } else if (0 == strcmp(key, "module_option")) {
        if (NULL != value) data.module_option = strdup(value);
    } else if (0 == strcmp(key, "extra_includes")) {
        /* this is the hard one - need to put it together... */
        int i;
        char **values = opal_argv_split(value, ' ');

        for (i = 0 ; i < opal_argv_count(values) ; ++i) {
            char *line;
            asprintf(&line, "-I%s%s%s", OPAL_INCLUDEDIR, OMPI_PATH_SEP, values[i]);
            opal_argv_append_nosize(&data.preproc_flags, line);
            free(line);
        }
    } else if (0 == strcmp(key, "preprocessor_flags")) {
        char **values = opal_argv_split(value, ' ');
        opal_argv_insert(&data.preproc_flags, 
                         opal_argv_count(data.preproc_flags),
                         values);
        opal_argv_free(values);
    } else if (0 == strcmp(key, "compiler_flags")) {
        char **values = opal_argv_split(value, ' ');
        opal_argv_insert(&data.comp_flags,
                         opal_argv_count(data.comp_flags),
                         values);
        opal_argv_free(values);
    } else if (0 == strcmp(key, "linker_flags")) {
        char **values = opal_argv_split(value, ' ');
        opal_argv_insert(&data.link_flags,
                         opal_argv_count(data.link_flags),
                         values);
        opal_argv_free(values);
    } else if (0 == strcmp(key, "libs")) {
        char **values = opal_argv_split(value, ' ');
        opal_argv_insert(&data.libs,
                         opal_argv_count(data.libs),
                         values);
        opal_argv_free(values);
    } else if (0 == strcmp(key, "required_file")) {
        if (NULL != value) data.req_file = strdup(value);
    } else if (0 == strcmp(key, "project_short")) {
        if (NULL != value) data.project_short = strdup(value);
    } else if (0 == strcmp(key, "compiler_env")) {
        if (NULL != value) data.compiler_env = strdup(value);
    } else if (0 == strcmp(key, "compiler_flags_env")) {
        if (NULL != value) data.compiler_flags_env = strdup(value);
    }
}

static int
data_init(const char *appname) 
{
    int ret;
    char *datafile;

    data.language = NULL;
    data.compiler = NULL;
    data.project = NULL;
    data.project_short = NULL;
    data.version = NULL;
    data.compiler_env = NULL;
    data.compiler_flags_env = NULL;
    data.module_option = NULL;
    data.preproc_flags = malloc(sizeof(char*));
    data.preproc_flags[0] = NULL;
    data.comp_flags = malloc(sizeof(char*));
    data.comp_flags[0] = NULL;
    data.link_flags = malloc(sizeof(char*));
    data.link_flags[0] = NULL;
    data.libs = malloc(sizeof(char*));
    data.libs[0] = NULL;
    data.req_file = NULL;

    /* load the default -I<incdir> and -L<libdir> */
    if (0 != strcmp(OPAL_INCLUDEDIR, "/usr/include")) {
        char *line;
        asprintf(&line, "-I%s", OPAL_INCLUDEDIR);
        opal_argv_append_nosize(&data.preproc_flags, line);
        free(line);
    }
    if (0 != strcmp(OPAL_LIBDIR, "/usr/lib")) {
        char *line;
        asprintf(&line, "-L%s", OPAL_LIBDIR);
        opal_argv_append_nosize(&data.link_flags, line);
        free(line);
    }

    /* now load the data */
    asprintf(&datafile, "%s%s%s-wrapper-data.txt", 
             OPAL_PKGDATADIR, OMPI_PATH_SEP, appname);
    if (NULL == datafile) return OPAL_ERR_TEMP_OUT_OF_RESOURCE;

    ret = opal_util_keyval_parse(datafile, data_callback);

    free(datafile);

    return ret;
}


static int
data_finalize(void)
{
    if (NULL != data.language) free(data.language);
    if (NULL != data.compiler) free(data.compiler);
    if (NULL != data.project) free(data.project);
    if (NULL != data.project_short) free(data.project_short);
    if (NULL != data.version) free(data.version);
    if (NULL != data.compiler_env) free(data.compiler_env);
    if (NULL != data.compiler_flags_env) free(data.compiler_flags_env);
    if (NULL != data.module_option) free(data.module_option);
    opal_argv_free(data.preproc_flags);
    opal_argv_free(data.comp_flags);
    opal_argv_free(data.link_flags);
    opal_argv_free(data.libs);
    if (NULL != data.req_file) free(data.req_file);

    return OPAL_SUCCESS;
}


static void
print_flags(char **args, char *pattern)
{
    int i;
    bool found = false;

    for (i = 0 ; args[i] != NULL ; ++i) {
        if (0 == strncmp(args[i], pattern, strlen(pattern))) {
            if (found)  printf(" ");
            printf("%s", args[i] + strlen(pattern));
            found = true;
        }
    }

    if (found) printf("\n");
}


static void
load_env_data(const char *project, const char *flag, char **data)
{
    char *envname;
    char *envvalue;

    if (NULL == project || NULL == flag) return;

    asprintf(&envname, "%s_MPI%s", project, flag);
    if (NULL == (envvalue = getenv(envname))) {
        free(envname);
        asprintf(&envname, "%s_%s", project, flag);
        if (NULL == (envvalue = getenv(envname))) {
            free(envname);
            return;
        }
    } 
    free(envname);

    if (NULL != *data) free(*data);
    *data = strdup(envvalue);
}


static void
load_env_data_argv(const char *project, const char *flag, char ***data)
{
    char *envname;
    char *envvalue;

    if (NULL == project || NULL == flag) return;

    asprintf(&envname, "%s_MPI%s", project, flag);
    if (NULL == (envvalue = getenv(envname))) {
        free(envname);
        asprintf(&envname, "%s_%s", project, flag);
        if (NULL == (envvalue = getenv(envname))) {
            free(envname);
            return;
        }
    } 
    free(envname);

    if (NULL != *data) opal_argv_free(*data);

    *data = opal_argv_split(envvalue, ' ');
}


int
main(int argc, char *argv[])
{
    int exit_status = 0, ret, flags = 0, i;
    int exec_argc = 0, user_argc = 0;
    char **exec_argv = NULL, **user_argv = NULL;
    char *exec_command, *base_argv0 = NULL;
    bool disable_flags = true;
    bool real_flag = false;

    if (OPAL_SUCCESS != (ret = opal_init_util())) {
        return ret;
    }


    /****************************************************
     *
     * Setup compiler information
     *
     ****************************************************/

    base_argv0 = strdup(basename(argv[0]));
    if (OPAL_SUCCESS != (ret = data_init(base_argv0))) {
        return ret;
    }

    /* compiler */
    load_env_data(data.project_short, data.compiler_env, &data.compiler);

    /* preprocessor flags */
    load_env_data_argv(data.project_short, "CPPFLAGS", &data.preproc_flags);

    /* compiler flags */
    load_env_data_argv(data.project_short, data.compiler_flags_env,
                       &data.comp_flags);

    /* linker flags */
    load_env_data_argv(data.project_short, "LDFLAGS", &data.link_flags);

    /* libs */
    load_env_data_argv(data.project_short, "LIBS", &data.libs);


    /****************************************************
     *
     * Sanity Checks
     *
     ****************************************************/
    
    if (NULL != data.req_file) {
        /* make sure the language is supported */
        if (0 == strcmp(data.req_file, "not supported")) {
            opal_show_help("help-opal-wrapper.txt", "no-language-support", true,
                           data.language, base_argv0, NULL);
            goto cleanup;
        }

        if (data.req_file[0] != '\0') {
            char *filename;
            struct stat buf;
            asprintf(&filename, "%s%s%s", OPAL_LIBDIR, OMPI_PATH_SEP, data.req_file);
            if (0 != stat(filename, &buf)) {
                opal_show_help("help-opal-wrapper.txt", "file-not-found", true,
                               base_argv0, data.req_file, data.language, NULL);
            }
        }
    }

    /****************************************************
     *
     * Parse user flags
     *
     ****************************************************/
    flags = COMP_WANT_COMMAND|COMP_WANT_PREPROC|
        COMP_WANT_COMPILE|COMP_WANT_LINK;

    user_argv = opal_argv_copy(argv + 1);
    user_argc = opal_argv_count(user_argv);

    for (i = 0 ; i < user_argc ; ++i) {
        if (0 == strncmp(user_argv[i], "-showme", strlen("-showme")) ||
            0 == strncmp(user_argv[i], "--showme", strlen("--showme")) ||
            0 == strncmp(user_argv[i], "-show", strlen("-show")) ||
            0 == strncmp(user_argv[i], "--show", strlen("--show"))) {
            bool done_now = false;

            /* check for specific things we want to see.  First three
               still invoke all the building routines.  Last set want
               to parse out certain flags, so we don't go through the
               normal build routine - skip to cleanup. */
            if (0 == strncmp(user_argv[i], "-showme:command", strlen("-showme:command")) ||
                0 == strncmp(user_argv[i], "--showme:command", strlen("--showme:command"))) {
                flags = COMP_WANT_COMMAND;
                /* we know what we want, so don't process any more args */
                done_now = true;
            } else if (0 == strncmp(user_argv[i], "-showme:compile", strlen("-showme:compile")) ||
                0 == strncmp(user_argv[i], "--showme:compile", strlen("--showme:compile"))) {
                flags = COMP_WANT_PREPROC|COMP_WANT_COMPILE;
                /* we know what we want, so don't process any more args */
                done_now = true;
            } else if (0 == strncmp(user_argv[i], "-showme:link", strlen("-showme:link")) ||
                       0 == strncmp(user_argv[i], "--showme:link", strlen("--showme:link"))) {
                flags = COMP_WANT_COMPILE|COMP_WANT_LINK;
                /* we know what we want, so don't process any more args */
                done_now = true;
            } else if (0 == strncmp(user_argv[i], "-showme:incdirs", strlen("-showme:incdirs")) ||
                       0 == strncmp(user_argv[i], "--showme:incdirs", strlen("--showme:incdirs"))) {
                print_flags(data.preproc_flags, "-I");
                goto cleanup;
            } else if (0 == strncmp(user_argv[i], "-showme:libdirs", strlen("-showme:libdirs")) ||
                       0 == strncmp(user_argv[i], "--showme:libdirs", strlen("--showme:libdirs"))) {
                print_flags(data.link_flags, "-L");
                goto cleanup;
            } else if (0 == strncmp(user_argv[i], "-showme:libs", strlen("-showme:libs")) ||
                       0 == strncmp(user_argv[i], "--showme:libs", strlen("--showme:libs"))) {
                print_flags(data.libs, "-l");
                goto cleanup;
            } else if (0 == strncmp(user_argv[i], "-showme:version", strlen("-showme:version")) ||
                       0 == strncmp(user_argv[i], "--showme:version", strlen("--showme:version"))) {
                opal_show_help("help-opal-wrapper.txt", "version", false,
                               argv[0], data.project, data.version, data.language, NULL);
                goto cleanup;
            } else if (0 == strncmp(user_argv[i], "-showme:", strlen("-showme:")) ||
                       0 == strncmp(user_argv[i], "--showme:", strlen("--showme:"))) {
                opal_show_help("help-opal-wrapper.txt", "usage", true,
                               argv[0], data.project, NULL);
                goto cleanup;
            }

            flags |= (COMP_DRY_RUN|COMP_SHOW_ERROR);
            /* remove element from user_argv */
            opal_argv_delete(&user_argc, &user_argv, i, 1);
            --i;

            if (done_now) {
                disable_flags = false;
                break;
            }

        } else if (0 == strcmp(user_argv[i], "-c")) {
            flags &= ~COMP_WANT_LINK;
            real_flag = true;
        } else if (0 == strcmp(user_argv[i], "-E") || 
                   0 == strcmp(user_argv[i], "-M")) {
            flags &= ~(COMP_WANT_COMPILE | COMP_WANT_LINK);
            real_flag = true;
        } else if (0 == strcmp(user_argv[i], "-S")) {
            flags &= ~COMP_WANT_LINK;
            real_flag = true;
        } else if (0 == strcmp(user_argv[i], "-lpmpi")) {
            flags |= COMP_WANT_PMPI;

            /* remove element from user_argv */
            opal_argv_delete(&user_argc, &user_argv, i, 1);
            --i;
        } else if ('-' != user_argv[i][0]) {
            disable_flags = false;
            flags |= COMP_SHOW_ERROR;
            real_flag = true;
        } else { 
            real_flag = true;
        }
    }

    /* clear out the want_flags if we got no arguments not starting
       with a - (dash) and -showme wasn't given OR -showme was given
       and we had at least one more non-showme argument that started
       with a - (dash) and no other non-dash arguments.  Some examples:

       opal_wrapper                : clear our flags
       opal_wrapper -v             : clear our flags
       opal_wrapper -E a.c         : don't clear our flags
       opal_wrapper a.c            : don't clear our flags
       opal_wrapper -showme        : don't clear our flags
       opal_wrapper -showme -v     : clear our flags
       opal_wrapper -showme -E a.c : don't clear our flags
       opal_wrapper -showme a.c    : don't clear our flags
    */
    if (disable_flags && !((flags & COMP_DRY_RUN) && !real_flag)) {
        flags &= ~(COMP_WANT_PREPROC|COMP_WANT_COMPILE|COMP_WANT_LINK);
    }

#if !OMPI_ENABLE_MPI_PROFILING
    /* sanity check */
    if (flags & COMP_WANT_PMPI) {
	opal_show_help("help-opal-wrapper.txt", "no-profiling-support", true,
		       argv[0], NULL);
    }
#endif


    /****************************************************
     *
     * Assemble the command line
     *
     ****************************************************/

    /* compiler (may be multiple arguments, so split) */
    if (flags & COMP_WANT_COMMAND) {
        exec_argv = opal_argv_split(data.compiler, ' ');
        exec_argc = opal_argv_count(exec_argv);
    } else {
        exec_argv = malloc(sizeof(char*));
        exec_argv[0] = NULL;
        exec_argc = 0;
    }

    /* preproc flags */
    if (flags & COMP_WANT_PREPROC) {
        opal_argv_insert(&exec_argv, exec_argc, data.preproc_flags);
        exec_argc = opal_argv_count(exec_argv);
    }

    /* compiler flags */
    if (flags & COMP_WANT_COMPILE) {
        opal_argv_insert(&exec_argv, exec_argc, data.comp_flags);
        /* Deal with languages like Fortran 90 that have special
           places and flags for modules or whatever */
        if (data.module_option != NULL) {
            char *line;
            asprintf(&line, "%s%s", data.module_option, OPAL_LIBDIR);
            opal_argv_append_nosize(&exec_argv, line);
            free(line);
        }
        exec_argc = opal_argv_count(exec_argv);
    }

    /* add all the user arguments */
    opal_argv_insert(&exec_argv, exec_argc, user_argv);
    exec_argc = opal_argv_count(exec_argv);

    /* link flags and libs */
    if (flags & COMP_WANT_LINK) {
        opal_argv_insert(&exec_argv, exec_argc, data.link_flags);
        exec_argc = opal_argv_count(exec_argv);

        opal_argv_insert(&exec_argv, exec_argc, data.libs);
        exec_argc = opal_argv_count(exec_argv);
    }


    /****************************************************
     *
     * Execute the command
     *
     ****************************************************/

    if (flags & COMP_DRY_RUN) {
        exec_command = opal_argv_join(exec_argv, ' ');
        printf("%s\n", exec_command);
    } else {
        char *tmp;

#if 0
        exec_command = opal_argv_join(exec_argv, ' ');
        printf("command: %s\n", exec_command);
#endif

        tmp = opal_path_findv(exec_argv[0], 0, environ, NULL);
        if (NULL == tmp) {
            opal_show_help("help-opal-wrapper.txt", "no-compiler-found", true,
                           exec_argv[0], NULL);
            errno = 0;
            exit_status = 1;
        }  else {
            int status;

            free(tmp);
            ret = opal_few(exec_argv, &status);
            exit_status = WIFEXITED(status) ? WEXITSTATUS(status) :
                (WIFSIGNALED(status) ? WTERMSIG(status) :
                 (WIFSTOPPED(status) ? WSTOPSIG(status) : 255));

            if (0 != ret && 0 != errno && (flags & COMP_SHOW_ERROR)) {
                perror(base_argv0);
            }
        }
    }

    /****************************************************
     *
     * Cleanup
     *
     ****************************************************/
 cleanup:

    opal_argv_free(exec_argv);
    opal_argv_free(user_argv);
    if (NULL != base_argv0) free(base_argv0);

    if (OPAL_SUCCESS != (ret = data_finalize())) {
        return ret;
    }

    if (OPAL_SUCCESS != (ret = opal_finalize_util())) {
        return ret;
    }

    return exit_status;
}
