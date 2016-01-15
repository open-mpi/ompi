/*
 * Copyright (c) 2015 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef _COMMON_VERBS_USNIC_H_
#define _COMMON_VERBS_USNIC_H_

#include "opal_config.h"

#include <stdint.h>
#include <infiniband/verbs.h>

BEGIN_C_DECLS

/*
 * Register fake verbs drivers
 */
void opal_common_verbs_usnic_register_fake_drivers(void);

END_C_DECLS

#endif
