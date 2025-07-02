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
#include "3rd-party/prrte/include/prte.h"


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
    int ret;

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

    /*
     * set environment variable for our install location
     * used within the OMPI prrte schizo component
     */

    setenv("OMPI_LIBDIR_LOC", opal_install_dirs.libdir, 1);

    // Set environment variable to tell PRTE what MCA prefixes belong
    // to Open MPI.
    setup_mca_prefixes();

    
    ret = prte_launch(argc, argv);
    if (OMPI_SUCCESS != ret) {
        opal_show_help("help-mpirun.txt", "prte-launch-failed", 1, strerror(errno));
        exit(1);
    }

    return 0;
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