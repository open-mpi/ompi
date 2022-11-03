/***************************************************************************
 *                                                                         *
 *          Open MPI: Open Source High Performance Computing               *
 *                                                                         *
 *                   https://www.open-mpi.org/                             *
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
#include <string.h>

#include "opal/mca/base/base.h"
#include "opal/mca/installdirs/base/base.h"
#include "opal/runtime/opal.h"
#include "opal/util/argv.h"
#include "opal/util/basename.h"
#include "opal/util/opal_environ.h"
#include "opal/util/os_dirpath.h"
#include "opal/util/os_path.h"
#include "opal/util/path.h"
#include "opal/util/printf.h"
#include "opal/util/show_help.h"
#include "ompi/constants.h"

static char *find_prterun(void)
{
    char *filename = NULL;
#if !OMPI_USING_INTERNAL_PRRTE
    char *prrte_prefix = NULL;
#endif

    /* 1) Did the user tell us exactly where to find prterun? */
    filename = getenv("OMPI_PRTERUN");
    if (NULL != filename) {
        return filename;
    }

#if OMPI_USING_INTERNAL_PRRTE
    /* 2) If using internal PRRTE, use our bindir.  Note that this
     * will obey OPAL_PREFIX and OPAL_DESTDIR */
    opal_asprintf(&filename, "%s%sprterun", opal_install_dirs.bindir, OPAL_PATH_SEP);
    return filename;
#else

    /* 3) Look in ${PRTE_PREFIX}/bin */
    prrte_prefix = getenv("PRTE_PREFIX");
    if (NULL != prrte_prefix) {
        opal_asprintf(&filename, "%s%sbin%sprterun", prrte_prefix, OPAL_PATH_SEP, OPAL_PATH_SEP);
        return filename;
    }

    /* 4) See if configure told us where to look, if set */
#if defined(OMPI_PRTERUN_PATH)
    return strdup(OMPI_PRTERUN_PATH);
#else

    /* 5) Use path search */
    filename = opal_find_absolute_path("prterun");

    return filename;
#endif
#endif
}

static void append_prefixes(char ***out, const char *in)
{
    if (NULL == in) {
        return;
    }

    char **tokenized;
    tokenized = opal_argv_split(in, ' ');
    if (NULL == tokenized) {
        return;
    }

    int count = opal_argv_count(*out);
    for (int i = 0; tokenized[i] != NULL; ++i) {
        // Skip adding the names "common" and "pmix" to the list
        if (strcmp(tokenized[i], "common") == 0 ||
            strcmp(tokenized[i], "pmix") == 0) {
            continue;
        }
        opal_argv_append(&count, out, tokenized[i]);
    }

    opal_argv_free(tokenized);
}

static void setup_mca_prefixes(void)
{
    int count = 0;
    char **tmp = NULL;

    opal_argv_append(&count, &tmp, "mca");
    opal_argv_append(&count, &tmp, "opal");
    opal_argv_append(&count, &tmp, "ompi");

    append_prefixes(&tmp, MCA_oshmem_FRAMEWORKS);
    append_prefixes(&tmp, MCA_ompi_FRAMEWORKS);
    append_prefixes(&tmp, MCA_opal_FRAMEWORKS);

    char *env_str = opal_argv_join(tmp, ',');
    opal_setenv("OMPI_MCA_PREFIXES", env_str, true,
                &environ);
    free(env_str);

    opal_argv_free(tmp);
}


int main(int argc, char *argv[])
{
    char *opal_prefix = getenv("OPAL_PREFIX");
    char *full_prterun_path = NULL;
    char **prterun_args = NULL;
    int ret;
    size_t i;

    ret = opal_init_util(&argc, &argv);
    if (OMPI_SUCCESS != ret) {
        fprintf(stderr, "Failed initializing opal: %d\n", ret);
        exit(1);
    }

    /* note that we just modify our environment rather than create a
     * child environment because it is easier and we're not going to
     * be around long enough for it to matter (since we exec prterun
     * asap */
    setenv("PRTE_MCA_schizo_proxy", "ompi", 1);
    setenv("OMPI_VERSION", OMPI_VERSION, 1);
    char *base_tool_name = opal_basename(argv[0]);
    setenv("OMPI_TOOL_NAME", base_tool_name, 1);
    free(base_tool_name);

    /* TODO: look for --prefix and compare with OPAL_PREFIX and pick
     * one */

    /* as a special case, if OPAL_PREFIX was set and either PRRTE or
     * PMIx are internal builds, set their prefix variables as well */
    if (NULL != opal_prefix) {
#if OMPI_USING_INTERNAL_PRRTE
        setenv("PRTE_PREFIX", opal_prefix, 1);
#endif
#if OPAL_USING_INTERNAL_PMIX
        setenv("PMIX_PREFIX", opal_prefix, 1);
#endif
    }

    full_prterun_path = find_prterun();
    if (NULL == full_prterun_path) {
        opal_show_help("help-mpirun.txt", "no-prterun-found", 1);
        exit(1);
    }

    /*
     * set environment variable for our install location
     * used within the OMPI prrte schizo component
     */

    setenv("OMPI_LIBDIR_LOC", opal_install_dirs.libdir, 1);

    // Set environment variable to tell PRTE what MCA prefixes belong
    // to Open MPI.
    setup_mca_prefixes();

    /* calling mpirun (and now prterun) with a full path has a special
     * meaning in terms of -prefix behavior, so copy that behavior
     * into prterun */
    if (opal_path_is_absolute(argv[0])) {
        opal_argv_append_nosize(&prterun_args, full_prterun_path);
    } else {
        opal_argv_append_nosize(&prterun_args, "prterun");
    }

    /* Copy all the mpirun arguments to prterun.
     * TODO: Need to handle --prefix rationally here. */
    for (i = 1; NULL != argv[i]; i++) {
        opal_argv_append_nosize(&prterun_args, argv[i]);
    }
    ret = execv(full_prterun_path, prterun_args);
    opal_show_help("help-mpirun.txt", "prterun-exec-failed",
                   1, full_prterun_path, strerror(errno));
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
 * Copyright (c) 2020-2022 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021      Nanook Consulting.  All rights reserved.
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * Copyright (c) 2022      Triad National Security, LLC. All rights
 *                         reserved.

 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
