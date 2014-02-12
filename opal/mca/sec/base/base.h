/*
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 */

#ifndef MCA_SEC_BASE_H
#define MCA_SEC_BASE_H

#include "opal_config.h"
#include "opal/types.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_framework.h"
#include "opal/class/opal_list.h"
#include "opal/dss/dss.h"

#include "opal/mca/sec/sec.h"

BEGIN_C_DECLS

OPAL_DECLSPEC extern mca_base_framework_t opal_sec_base_framework;

/**
 * Select a sec module
 */
OPAL_DECLSPEC int opal_sec_base_select(void);

END_C_DECLS

#endif
