/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006-2015 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2016      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_MCA_PINSTALLDIRS_PINSTALLDIRS_H
#define PMIX_MCA_PINSTALLDIRS_PINSTALLDIRS_H

#include <src/include/pmix_config.h>

#include "src/mca/mca.h"
#include "src/mca/base/base.h"

BEGIN_C_DECLS

/*
 * Most of this file is just for ompi_info.  The only public interface
 * once pmix_init has been called is the pmix_pinstall_dirs structure
 * and the pmix_pinstall_dirs_expand() call */
struct pmix_pinstall_dirs_t {
    char* prefix;
    char* exec_prefix;
    char* bindir;
    char* sbindir;
    char* libexecdir;
    char* datarootdir;
    char* datadir;
    char* sysconfdir;
    char* sharedstatedir;
    char* localstatedir;
    char* libdir;
    char* includedir;
    char* infodir;
    char* mandir;

    /* Note that the following fields intentionally have an "ompi"
       prefix, even though they're down in the PMIX layer.  This is
       not abstraction break because the "ompi" they're referring to
       is for the build system of the overall software tree -- not an
       individual project within that overall tree.

       Rather than using pkg{data,lib,includedir}, use our own
       ompi{data,lib,includedir}, which is always set to
       {datadir,libdir,includedir}/pmix. This will keep us from
       having help files in prefix/share/open-rte when building
       without PMIX, but in prefix/share/pmix when building
       with PMIX.

       Note that these field names match macros set by configure that
       are used in Makefile.am files.  E.g., project help files are
       installed into $(pmixdatadir). */
    char* pmixdatadir;
    char* pmixlibdir;
    char* pmixincludedir;
};
typedef struct pmix_pinstall_dirs_t pmix_pinstall_dirs_t;

/* Install directories.  Only available after pmix_init() */
extern pmix_pinstall_dirs_t pmix_pinstall_dirs;

/**
 * Expand out path variables (such as ${prefix}) in the input string
 * using the current pmix_pinstall_dirs structure */
char * pmix_pinstall_dirs_expand(const char* input);


/**
 * Structure for pinstalldirs components.
 */
struct pmix_pinstalldirs_base_component_2_0_0_t {
    /** MCA base component */
    pmix_mca_base_component_t component;
    /** MCA base data */
    pmix_mca_base_component_data_t component_data;
    /** install directories provided by the given component */
    pmix_pinstall_dirs_t install_dirs_data;
};
/**
 * Convenience typedef
 */
typedef struct pmix_pinstalldirs_base_component_2_0_0_t pmix_pinstalldirs_base_component_t;

/*
 * Macro for use in components that are of type pinstalldirs
 */
#define PMIX_PINSTALLDIRS_BASE_VERSION_1_0_0 \
    PMIX_MCA_BASE_VERSION_1_0_0("pinstalldirs", 1, 0, 0)

END_C_DECLS

#endif /* PMIX_MCA_PINSTALLDIRS_PINSTALLDIRS_H */
