/*
 * Copyright (c) 2006-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2007-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2010      Sandia National Laboratories. All rights reserved.
 * Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#ifndef PRTE_INSTALLDIRS_BASE_H
#define PRTE_INSTALLDIRS_BASE_H

#include "prte_config.h"
#include "src/mca/base/pmix_mca_base_framework.h"
#include "src/mca/prteinstalldirs/prteinstalldirs.h"

/*
 * Global functions for MCA overall prteinstalldirs open and close
 */
BEGIN_C_DECLS

/**
 * Framework structure declaration
 */
PRTE_EXPORT extern pmix_mca_base_framework_t prte_prteinstalldirs_base_framework;

/* Just like prte_install_dirs_expand() (see prteinstalldirs.h), but will
   also insert the value of the environment variable $PRTE_DESTDIR, if
   it exists/is set.  This function should *only* be used during the
   setup routines of prteinstalldirs. */
char *prte_install_dirs_expand_setup(const char *input);

END_C_DECLS

#endif /* PRTE_BASE_INSTALLDIRS_H */
