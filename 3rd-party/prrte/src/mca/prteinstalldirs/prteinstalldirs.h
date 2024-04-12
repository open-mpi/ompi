/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006-2015 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PRTE_MCA_INSTALLDIRS_INSTALLDIRS_H
#define PRTE_MCA_INSTALLDIRS_INSTALLDIRS_H

#include "prte_config.h"

#include "src/pmix/pmix-internal.h"
#include "src/mca/base/pmix_base.h"
#include "src/mca/mca.h"

BEGIN_C_DECLS

/*
 * Most of this file is just for ompi_info.  The only public interface
 * once prte_init has been called is the prte_install_dirs structure
 * and the prte_install_dirs_expand() call */
struct prte_install_dirs_t {
    char *prefix;
    char *exec_prefix;
    char *bindir;
    char *sbindir;
    char *libexecdir;
    char *datarootdir;
    char *datadir;
    char *sysconfdir;
    char *sharedstatedir;
    char *localstatedir;
    char *libdir;
    char *includedir;
    char *infodir;
    char *mandir;
    char *prtedatadir;
    char *prtelibdir;
    char *prteincludedir;
};
typedef struct prte_install_dirs_t prte_install_dirs_t;

/* Install directories.  Only available after prte_init() */
PRTE_EXPORT extern prte_install_dirs_t prte_install_dirs;

/**
 * Expand out path variables (such as ${prefix}) in the input string
 * using the current prte_install_dirs structure */
PRTE_EXPORT char *prte_install_dirs_expand(const char *input);

/**
 * Structure for prteinstalldirs components.
 */
struct prte_prteinstalldirs_base_component_2_0_0_t {
    /** MCA base component */
    pmix_mca_base_component_t component;
    /** install directories provided by the given component */
    prte_install_dirs_t install_dirs_data;
};
/**
 * Convenience typedef
 */
typedef struct prte_prteinstalldirs_base_component_2_0_0_t prte_prteinstalldirs_base_component_t;

/*
 * Macro for use in components that are of type prteinstalldirs
 */
#define PRTE_INSTALLDIRS_BASE_VERSION_2_0_0 PRTE_MCA_BASE_VERSION_3_0_0("prteinstalldirs", 2, 0, 0)

END_C_DECLS

#endif /* PRTE_MCA_INSTALLDIRS_INSTALLDIRS_H */
