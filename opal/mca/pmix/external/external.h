/*
 * Copyright (c) 2011-2019 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2016-2019 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 *
 * Copyright (c) 2016-2019 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * When this component is used, this file is included in the rest of
 * the OPAL/OMPI code base via opal/mca/pmix/pmix-internal.h.  As such,
 * this header represents the public interface to this static component.
 */

#ifndef MCA_OPAL_PMIX_EXTERNAL_H
#define MCA_OPAL_PMIX_EXTERNAL_H

BEGIN_C_DECLS

#include <opal_config.h>

/* Top-level configure will always configure the embedded pmix
 * component, even if we already know that we'll be using an external
 * pmix (because of complicated reasons). A side-effect of this is
 * that the embedded pmix will AC_DEFINE PMIX_VERSION (and friends)
 * in opal_config.h. If the external pmix defines a different value
 * of PMIX_VERSION (etc.), we'll get zillions of warnings about the
 * two PMIX_VERSION values not matching.  Hence, we undefined all of
 * them here (so that the external <pmix.h> can define them to
 * whatever it wants). */

#undef PMIX_VERSION
#undef PMIX_VERSION_MAJOR
#undef PMIX_VERSION_MINOR
#undef PMIX_VERSION_RELEASE
#undef PMIX_VERSION_GREEK

#include "pmix_common.h"
#include "pmix.h"

END_C_DECLS

#endif /* MCA_OPAL_PMIX_EXTERNAL_H */
