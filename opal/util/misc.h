/*
 * Copyright (c) 2024      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** @file:
 * Miscellaneous utilities
 */

#ifndef OPAL_UTIL_MISC_H
#define OPAL_UTIL_MISC_H

#include "opal/include/opal_config.h"

BEGIN_C_DECLS

#ifndef container_of
#    define container_of(ptr, type, member) ((type *) (((char *) (ptr)) - offsetof(type, member)))
#endif

END_C_DECLS
#endif
