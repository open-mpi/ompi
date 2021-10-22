/***************************************************************************
 *                                                                         *
 *          Open MPI: Open Source High Performance Computing               *
 *                                                                         *
 *                   http://www.open-mpi.org/                              *
 *                                                                         *
 ***************************************************************************/
#include "ompi_config.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#if HAVE_SYS_STAT_H
#    include <sys/stat.h>
#endif /* HAVE_SYS_STAT_H */

#include "opal/mca/base/base.h"
#include "opal/mca/installdirs/base/base.h"
#include "opal/util/argv.h"
#include "opal/util/basename.h"
#include "opal/util/opal_environ.h"
#include "opal/util/os_dirpath.h"
#include "opal/util/os_path.h"
#include "opal/util/path.h"

int main(int argc, char *argv[])
{
    char *evar;
    char **pargs = NULL;
    char *pfx = NULL;
    int m, param_len;
    char *truepath;

    if (NULL != (evar = getenv("OPAL_PREFIX"))) {

#if OMPI_USING_INTERNAL_PRRTE
        setenv("PRTE_PREFIX", evar, true);
#endif

#if OPAL_USING_INTERNAL_PMIX
        setenv("PMIX_PREFIX", evar, true);
#endif
    }
    setenv("PRTE_MCA_schizo_proxy", "ompi", true);

    opal_argv_append_nosize(&pargs, "prterun");
    for (m=1; NULL != argv[m]; m++) {
        opal_argv_append_nosize(&pargs, argv[m]);
        /* Did the user specify a prefix, or want prefix by default? */
        if (0 == strcmp(argv[m], "--prefix")) {
            asprintf(&pfx, "%s%s", argv[m+1], "/bin");
        }
    }

    if (NULL != pfx) {
        /* "Parse" the param, aka remove superfluous path_sep. */
        param_len = strlen(pfx);
        while (0 == strcmp(OPAL_PATH_SEP, &(pfx[param_len - 1]))) {
            pfx[param_len - 1] = '\0';
            param_len--;
            if (0 == param_len) {
                fprintf(stderr, "A prefix was supplied to mpirun that only contained slashes.\n"
                        "This is a fatal error; mpirun will now abort.\nNo processes were launched.\n");
                exit(1);
            }
        }
    } else if (opal_path_is_absolute(argv[0])) {
        /* Check if called with fully-qualified path to mpirun.
         * (Note: Put this second so can override with --prefix (above). */
        pfx = opal_dirname(argv[0]);
#if OMPI_USING_INTERNAL_PRRTE
    } else {
        /* in case --enable-prefix-by-default was given */
        mca_base_framework_open(&opal_installdirs_base_framework, 0);  // fill in the installdirs
        if (NULL != opal_install_dirs.bindir) {
            pfx = strdup(opal_install_dirs.bindir);
        }
#else
    } else {
        char *tmp, *t2;
        /* for external PRRTE, use the full path to prterun and set the lib */
        if (NULL != PRTE_PATH) {
            tmp = opal_basename(PRTE_PATH);
            if (NULL != tmp && 0 != strcmp(tmp, "prterun")) {
                /* this is a pure path */
                asprintf(&pfx, "%s/bin", PRTE_PATH);
                free(tmp);
                tmp = opal_dirname(PRTE_PATH);
            } else {
                pfx = opal_dirname(PRTE_PATH);
                /* we need to move up one more level to get to the lib directory */
                t2 = opal_dirname(pfx);
                asprintf(&tmp, "%s/lib", t2);
                free(t2);
            }
        }
        if (NULL != (evar = getenv("LD_LIBRARY_PATH"))) {
            asprintf(&t2, "%s:%s", tmp, evar);
            free(tmp);
            tmp = t2;
        }
        setenv("LD_LIBRARY_PATH", tmp, true);
        free(tmp);
#endif
    }

    if (NULL == pfx) {
        truepath = opal_path_findv("prterun", X_OK, environ, NULL);
    } else {
        truepath = opal_os_path(0, pfx, "prterun", NULL);
        free(pfx);
    }

    if (NULL == truepath) {
        fprintf(stderr, "prterun executable could not be found - unable to run\n");
        exit(1);
    }

    /* handle any external PMIx library path */
#if !OPAL_USING_INTERNAL_PMIX
    if (NULL != OPAL_PMIX_PATH) {
        char *tmp;
        asprintf(&tmp, "%s/lib", OPAL_PMIX_PATH);
        if (NULL != (evar = getenv("LD_LIBRARY_PATH"))) {
            char *t2;
            asprintf(&t2, "%s:%s", tmp, evar);
            free(tmp);
            tmp = t2;
        }
        setenv("LD_LIBRARY_PATH", tmp, true);
        free(tmp);
    }
#endif

    /* handle any opal_output directives */
    if (NULL != (evar = getenv("OPAL_OUTPUT_STDERR_FD"))) {
        setenv("PRTE_OUTPUT_STDERR_FD", evar, true);
        setenv("PMIX_OUTPUT_STDERR_FD", evar, true);

    }
    if (NULL != (evar = getenv("OPAL_OUTPUT_REDIRECT"))) {
        setenv("PRTE_OUTPUT_REDIRECT", evar, true);
        setenv("PMIX_OUTPUT_REDIRECT", evar, true);

    }
    if (NULL != (evar = getenv("OPAL_OUTPUT_SYSLOG_PRI"))) {
        setenv("PRTE_OUTPUT_SYSLOG_PRI", evar, true);
        setenv("PMIX_OUTPUT_SYSLOG_PRI", evar, true);

    }
    if (NULL != (evar = getenv("OPAL_OUTPUT_SYSLOG_IDENT"))) {
        setenv("PRTE_OUTPUT_SYSLOG_IDENT", evar, true);
        setenv("PMIX_OUTPUT_SYSLOG_IDENT", evar, true);

    }
    if (NULL != (evar = getenv("OPAL_OUTPUT_INTERNAL_TO_STDOUT"))) {
        setenv("PRTE_OUTPUT_INTERNAL_TO_STDOUT", evar, true);
        setenv("PMIX_OUTPUT_INTERNAL_TO_STDOUT", evar, true);

    }
    if (NULL != (evar = getenv("OPAL_OUTPUT_SUFFIX"))) {
        setenv("PRTE_OUTPUT_SUFFIX", evar, true);
        setenv("PMIX_OUTPUT_SUFFIX", evar, true);

    }

    /* exec prterun */
    execve(truepath, pargs, environ);
    fprintf(stderr, "The mpirun (\"%s\") cmd failed to exec its actual executable - your application will NOT execute. Error: %s\n",
                     truepath ? truepath : "NULL", strerror(errno));
    exit(1);
}

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
 * Copyright (c) 2017-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2021      Nanook Consulting.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
