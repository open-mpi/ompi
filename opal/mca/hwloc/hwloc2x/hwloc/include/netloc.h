/*
 * Copyright © 2013-2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright © 2013-2014 University of Wisconsin-La Crosse.
 *                         All rights reserved.
 * Copyright © 2015-2016 Inria.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 * See COPYING in top-level directory.
 *
 * $HEADER$
 */

#ifndef _NETLOC_H_
#define _NETLOC_H_

#ifndef _GNU_SOURCE
#define _GNU_SOURCE // for asprintf
#endif

#include <hwloc/autogen/config.h>

#include <hwloc.h>

#ifdef __cplusplus
extern "C" {
#endif

/** \defgroup netloc_api Netloc API
 * @{
 */
/**
 * Return codes
 */
enum {
    NETLOC_SUCCESS         =  0, /**< Success */
    NETLOC_ERROR           = -1, /**< Error: General condition */
    NETLOC_ERROR_NOTDIR    = -2, /**< Error: URI is not a directory */
    NETLOC_ERROR_NOENT     = -3, /**< Error: URI is invalid, no such entry */
    NETLOC_ERROR_EMPTY     = -4, /**< Error: No networks found */
    NETLOC_ERROR_MULTIPLE  = -5, /**< Error: Multiple matching networks found */
    NETLOC_ERROR_NOT_IMPL  = -6, /**< Error: Interface not implemented */
    NETLOC_ERROR_EXISTS    = -7, /**< Error: If the entry already exists when trying to add to a lookup table */
    NETLOC_ERROR_NOT_FOUND = -8, /**< Error: No path found */
    NETLOC_ERROR_MAX       = -9  /**< Error: Enum upper bound marker. No errors less than this number Will not be returned externally. */
};


#ifdef __cplusplus
} /* extern "C" */
#endif

/** @} */

#endif // _NETLOC_H_
