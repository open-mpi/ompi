/*
 * Copyright (c) 2015 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/* This header contains macros to help minimize usnic BTL differences
 * between v1.7/v1.8 and v1.9/v2.0. */

#ifndef MTL_OFI_COMPAT_H
#define MTL_OFI_COMPAT_H

/************************************************************************/

/* v2.0 and beyond */

#if (OPAL_MAJOR_VERSION >= 2)

#include "opal/mca/pmix/pmix.h"
#include "opal/mca/pmix/pmix_types.h"

#define OFI_COMPAT_MODEX_RECV(ret, mtl_version, proc, ep_name, size) \
    OPAL_MODEX_RECV((ret), (mtl_version), &(proc)->super.proc_name, (ep_name), (size));

#define OFI_COMPAT_MODEX_SEND(ret, mtl_version, ep_name, namelen) \
    OPAL_MODEX_SEND((ret),          \
                    OPAL_PMIX_GLOBAL,    \
                    (mtl_version),  \
                    (ep_name)[0],   \
                    (namelen));

#define OFI_COMPAT_MCA_VERSION \
    MCA_BASE_MAKE_VERSION(component, \
                          OMPI_MAJOR_VERSION, \
                          OMPI_MINOR_VERSION, \
                          OMPI_RELEASE_VERSION)


/************************************************************************/

/* v1.7, v1.8, and v1.10 (there was no v1.9) */

#elif (OPAL_MAJOR_VERSION == 1 && OPAL_MINOR_VERSION >= 7)

#include "ompi/runtime/ompi_module_exchange.h"

#define OFI_COMPAT_MODEX_RECV(ret, mtl_version, proc, ep_name, size) \
    (ret) = ompi_modex_recv((mtl_version), (proc), (ep_name), (size));

#define OFI_COMPAT_MODEX_SEND(ret, mtl_version, ep_name, namelen) \
    (ret) = ompi_modex_send((mtl_version), (ep_name), (namelen));

#define OFI_COMPAT_MCA_VERSION \
    OMPI_MAJOR_VERSION, \
    OMPI_MINOR_VERSION, \
    OMPI_RELEASE_VERSION

/************************************************************************/

#else
#  error OMPI version too old (< 1.7)
#endif

#endif /* MTL_OFI_COMPAT_H */
