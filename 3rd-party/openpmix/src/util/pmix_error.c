/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2012 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"

#include "src/include/pmix_globals.h"
#include "src/include/pmix_event_strings.h"
#include "src/util/pmix_error.h"

PMIX_EXPORT const char *PMIx_Error_string(pmix_status_t errnum)
{
    size_t n;

    for (n=0; n < PMIX_EVENT_INDEX_BOUNDARY; n++) {
        if (errnum == pmix_event_strings[n].code) {
            return pmix_event_strings[n].name;
        }
    }

    return "ERROR STRING NOT FOUND";
}

PMIX_EXPORT pmix_status_t PMIx_Error_code(const char *errname)
{
    size_t n;

    for (n=0; n < PMIX_EVENT_INDEX_BOUNDARY; n++) {
        if (0 == strcasecmp(pmix_event_strings[n].name, errname)) {
            return pmix_event_strings[n].code;
        }
    }

    return INT32_MIN;
}
