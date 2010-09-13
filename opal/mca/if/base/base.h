/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#ifndef OPAL_IF_BASE_H
#define OPAL_IF_BASE_H

#include "opal_config.h"

#include "opal/mca/if/if.h"

/*
 * Global functions for MCA overall if open and close
 */
BEGIN_C_DECLS

OPAL_DECLSPEC int opal_if_base_open(void);
OPAL_DECLSPEC int opal_if_base_close(void);

/*
 * Globals
 */
OPAL_DECLSPEC extern opal_list_t opal_if_components;

END_C_DECLS

#endif /* OPAL_BASE_IF_H */
