/*
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
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

#include "src/mca/pinstalldirs/config/pinstall_dirs.h"
#include "src/mca/pinstalldirs/pinstalldirs.h"

const pmix_pinstalldirs_base_component_t pmix_mca_pinstalldirs_config_component = {
    /* First, the mca_component_t struct containing meta information
       about the component itself */
    .component = {
        PMIX_PINSTALLDIRS_BASE_VERSION_1_0_0,

        /* Component name and version */
        "config", PMIX_MAJOR_VERSION, PMIX_MINOR_VERSION, PMIX_RELEASE_VERSION,

        /* Component open and close functions */
        NULL, NULL
    },

    .install_dirs_data = {
        .prefix = PMIX_INSTALL_PREFIX,
        .exec_prefix = PMIX_EXEC_PREFIX,
        .bindir = PMIX_BINDIR,
        .sbindir = PMIX_SBINDIR,
        .libexecdir = PMIX_LIBEXECDIR,
        .datarootdir = PMIX_DATAROOTDIR,
        .datadir = PMIX_DATADIR,
        .sysconfdir = PMIX_SYSCONFDIR,
        .sharedstatedir = PMIX_SHAREDSTATEDIR,
        .localstatedir = PMIX_LOCALSTATEDIR,
        .libdir = PMIX_LIBDIR,
        .includedir = PMIX_INCLUDEDIR,
        .infodir = PMIX_INFODIR,
        .mandir = PMIX_MANDIR,
        .pmixdatadir = PMIX_PKGDATADIR,
        .pmixlibdir = PMIX_PKGLIBDIR,
        .pmixincludedir = PMIX_PKGINCLUDEDIR
    }
};
