/*
 * Copyright (c) 2006      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#ifndef OPAL_INSTALLDIRS_BASE_H
#define OPAL_INSTALLDIRS_BASE_H

#include "opal_config.h"

#include "opal/mca/installdirs/installdirs.h"

/*
 * Global functions for MCA overall installdirs open and close
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    OPAL_DECLSPEC int opal_installdirs_base_open(void);
    OPAL_DECLSPEC int opal_installdirs_base_close(void);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

/*
 * Globals
 */
OPAL_DECLSPEC extern opal_list_t opal_installdirs_components;

#endif /* OPAL_BASE_INSTALLDIRS_H */
