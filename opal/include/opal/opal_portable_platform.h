/*
 * Copyright (c) 2021      Amazon.com, Inc. or its affiliates.  All Rights
 *                         reserved.
 * Copyright (c) 2024      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Wrapper around GASNet's gasnet_portable_platform.h to avoid
 * compiler warnings.  This wrapper used to be necessary to add some
 * additional #defines, but those issues have now been resolved
 * upstream at gasnet.  However, we left this wrapper file just to be
 * able to handle any future workarounds, if necessary.
 */

#ifndef OPAL_PORTABLE_PLATFORM_H
#define OPAL_PORTABLE_PLATFORM_H 1

#include "opal/opal_portable_platform_real.h"

#endif
