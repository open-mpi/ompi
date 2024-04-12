/*
 * Copyright (c) 2006-2012 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2007-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2010      Sandia National Laboratories. All rights reserved.
 * Copyright (c) 2015-2019 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "prte_config.h"

#include "constants.h"
#include "src/mca/mca.h"
#include "src/mca/prteinstalldirs/base/base.h"
#include "src/mca/prteinstalldirs/base/static-components.h"
#include "src/mca/prteinstalldirs/prteinstalldirs.h"

prte_install_dirs_t prte_install_dirs = {0};

#define CONDITIONAL_COPY(target, origin, field)             \
    do {                                                    \
        if (origin.field != NULL && target.field == NULL) { \
            target.field = origin.field;                    \
        }                                                   \
    } while (0)

static int prte_prteinstalldirs_base_open(pmix_mca_base_open_flag_t flags)
{
    pmix_mca_base_component_list_item_t *component_item;
    int ret;

    ret = pmix_mca_base_framework_components_open(&prte_prteinstalldirs_base_framework, flags);
    if (PRTE_SUCCESS != ret) {
        return ret;
    }

    PMIX_LIST_FOREACH(component_item, &prte_prteinstalldirs_base_framework.framework_components,
                      pmix_mca_base_component_list_item_t)
    {
        const prte_prteinstalldirs_base_component_t *component
            = (const prte_prteinstalldirs_base_component_t *) component_item->cli_component;

        /* copy over the data, if something isn't already there */
        CONDITIONAL_COPY(prte_install_dirs, component->install_dirs_data, prefix);
        CONDITIONAL_COPY(prte_install_dirs, component->install_dirs_data, exec_prefix);
        CONDITIONAL_COPY(prte_install_dirs, component->install_dirs_data, bindir);
        CONDITIONAL_COPY(prte_install_dirs, component->install_dirs_data, sbindir);
        CONDITIONAL_COPY(prte_install_dirs, component->install_dirs_data, libexecdir);
        CONDITIONAL_COPY(prte_install_dirs, component->install_dirs_data, datarootdir);
        CONDITIONAL_COPY(prte_install_dirs, component->install_dirs_data, datadir);
        CONDITIONAL_COPY(prte_install_dirs, component->install_dirs_data, sysconfdir);
        CONDITIONAL_COPY(prte_install_dirs, component->install_dirs_data, sharedstatedir);
        CONDITIONAL_COPY(prte_install_dirs, component->install_dirs_data, localstatedir);
        CONDITIONAL_COPY(prte_install_dirs, component->install_dirs_data, libdir);
        CONDITIONAL_COPY(prte_install_dirs, component->install_dirs_data, includedir);
        CONDITIONAL_COPY(prte_install_dirs, component->install_dirs_data, infodir);
        CONDITIONAL_COPY(prte_install_dirs, component->install_dirs_data, mandir);
        CONDITIONAL_COPY(prte_install_dirs, component->install_dirs_data, prtedatadir);
        CONDITIONAL_COPY(prte_install_dirs, component->install_dirs_data, prtelibdir);
        CONDITIONAL_COPY(prte_install_dirs, component->install_dirs_data, prteincludedir);
    }

    /* expand out all the fields */
    prte_install_dirs.prefix = prte_install_dirs_expand_setup(prte_install_dirs.prefix);
    prte_install_dirs.exec_prefix = prte_install_dirs_expand_setup(prte_install_dirs.exec_prefix);
    prte_install_dirs.bindir = prte_install_dirs_expand_setup(prte_install_dirs.bindir);
    prte_install_dirs.sbindir = prte_install_dirs_expand_setup(prte_install_dirs.sbindir);
    prte_install_dirs.libexecdir = prte_install_dirs_expand_setup(prte_install_dirs.libexecdir);
    prte_install_dirs.datarootdir = prte_install_dirs_expand_setup(prte_install_dirs.datarootdir);
    prte_install_dirs.datadir = prte_install_dirs_expand_setup(prte_install_dirs.datadir);
    prte_install_dirs.sysconfdir = prte_install_dirs_expand_setup(prte_install_dirs.sysconfdir);
    prte_install_dirs.sharedstatedir = prte_install_dirs_expand_setup(
        prte_install_dirs.sharedstatedir);
    prte_install_dirs.localstatedir = prte_install_dirs_expand_setup(
        prte_install_dirs.localstatedir);
    prte_install_dirs.libdir = prte_install_dirs_expand_setup(prte_install_dirs.libdir);
    prte_install_dirs.includedir = prte_install_dirs_expand_setup(prte_install_dirs.includedir);
    prte_install_dirs.infodir = prte_install_dirs_expand_setup(prte_install_dirs.infodir);
    prte_install_dirs.mandir = prte_install_dirs_expand_setup(prte_install_dirs.mandir);
    prte_install_dirs.prtedatadir = prte_install_dirs_expand_setup(prte_install_dirs.prtedatadir);
    prte_install_dirs.prtelibdir = prte_install_dirs_expand_setup(prte_install_dirs.prtelibdir);
    prte_install_dirs.prteincludedir = prte_install_dirs_expand_setup(
        prte_install_dirs.prteincludedir);

#if 0
    fprintf(stderr, "prefix:           %s\n", prte_install_dirs.prefix);
    fprintf(stderr, "exec_prefix:      %s\n", prte_install_dirs.exec_prefix);
    fprintf(stderr, "bindir:           %s\n", prte_install_dirs.bindir);
    fprintf(stderr, "sbindir:          %s\n", prte_install_dirs.sbindir);
    fprintf(stderr, "libexecdir:       %s\n", prte_install_dirs.libexecdir);
    fprintf(stderr, "datarootdir:      %s\n", prte_install_dirs.datarootdir);
    fprintf(stderr, "datadir:          %s\n", prte_install_dirs.datadir);
    fprintf(stderr, "sysconfdir:       %s\n", prte_install_dirs.sysconfdir);
    fprintf(stderr, "sharedstatedir:   %s\n", prte_install_dirs.sharedstatedir);
    fprintf(stderr, "localstatedir:    %s\n", prte_install_dirs.localstatedir);
    fprintf(stderr, "libdir:           %s\n", prte_install_dirs.libdir);
    fprintf(stderr, "includedir:       %s\n", prte_install_dirs.includedir);
    fprintf(stderr, "infodir:          %s\n", prte_install_dirs.infodir);
    fprintf(stderr, "mandir:           %s\n", prte_install_dirs.mandir);
    fprintf(stderr, "prtedatadir:     %s\n", prte_install_dirs.prtedatadir);
    fprintf(stderr, "prtelibdir:      %s\n", prte_install_dirs.prtelibdir);
    fprintf(stderr, "prteincludedir:  %s\n", prte_install_dirs.prteincludedir);
#endif

    /* NTH: Is it ok not to close the components? If not we can add a flag
       to mca_base_framework_components_close to indicate not to deregister
       variable groups */
    return PRTE_SUCCESS;
}

static int prte_prteinstalldirs_base_close(void)
{
    free(prte_install_dirs.prefix);
    free(prte_install_dirs.exec_prefix);
    free(prte_install_dirs.bindir);
    free(prte_install_dirs.sbindir);
    free(prte_install_dirs.libexecdir);
    free(prte_install_dirs.datarootdir);
    free(prte_install_dirs.datadir);
    free(prte_install_dirs.sysconfdir);
    free(prte_install_dirs.sharedstatedir);
    free(prte_install_dirs.localstatedir);
    free(prte_install_dirs.libdir);
    free(prte_install_dirs.includedir);
    free(prte_install_dirs.infodir);
    free(prte_install_dirs.mandir);
    free(prte_install_dirs.prtedatadir);
    free(prte_install_dirs.prtelibdir);
    free(prte_install_dirs.prteincludedir);
    memset(&prte_install_dirs, 0, sizeof(prte_install_dirs));

    return pmix_mca_base_framework_components_close(&prte_prteinstalldirs_base_framework, NULL);
}

/* Declare the prteinstalldirs framework */
PMIX_MCA_BASE_FRAMEWORK_DECLARE(prte, prteinstalldirs, NULL, NULL, prte_prteinstalldirs_base_open,
                                prte_prteinstalldirs_base_close,
                                prte_prteinstalldirs_base_static_components,
                                PMIX_MCA_BASE_FRAMEWORK_FLAG_NOREGISTER
                                    | PMIX_MCA_BASE_FRAMEWORK_FLAG_NO_DSO);
