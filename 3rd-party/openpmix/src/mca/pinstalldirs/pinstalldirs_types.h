/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006-2015 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2016-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_MCA_PINSTALLDIRS_PINSTALLDIRS_TYPES_H
#define PMIX_MCA_PINSTALLDIRS_PINSTALLDIRS_TYPES_H

#include "src/include/pmix_config.h"

#include "pmix_common.h"

BEGIN_C_DECLS

/*
 * Most of this file is just for pmix_info.  The only public interface
 * once pmix_init has been called is the pmix_pinstall_dirs structure
 * and the pmix_pinstall_dirs_expand() call */
struct pmix_pinstall_dirs_t {
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

    /* Rather than using pkg{data,lib,includedir}, use our own
       pmix{data,lib,includedir}, which is always set to
       {datadir,libdir,includedir}/pmix.

       Note that these field names match macros set by configure that
       are used in Makefile.am files.  E.g., project help files are
       installed into $(pmixdatadir). */
    char *pmixdatadir;
    char *pmixlibdir;
    char *pmixincludedir;
};
typedef struct pmix_pinstall_dirs_t pmix_pinstall_dirs_t;

/* Install directories.  Only available after pmix_init() */
PMIX_EXPORT extern pmix_pinstall_dirs_t pmix_pinstall_dirs;

END_C_DECLS

#endif /* PMIX_MCA_PINSTALLDIRS_PINSTALLDIRS_TYPES_H */
