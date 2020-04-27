/*
 * Copyright (c) 2020      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * TODO: UGLY HACK ALERT!
 *
 * PRRTE uses --with-pmix-header as a key that it is using an internal
 * PMIx build (same with libevent/hwloc), and that header has to
 * include all the files that the prrte source expects.  This is a
 * pain and the long term fix is to add the "cobuild" support to PRRTE
 * similar to what was done for PMIx.  Until those patches land, this
 * header includes all the right pieces required.
 */

#ifndef OPAL_PMIX_3RDPARTY_H
#define OPAL_PMIX_3RDPARTY_H

#include <pmix.h>
#include <pmix_server.h>
#include <pmix_tool.h>
#include <pmix_version.h>

#endif
