/*
 * Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * When this component is used, this file is included in the rest of
 * the OPAL/ORTE/OMPI code base via opal/mca/event/event.h.  As such,
 * this header represents the public interface to this static component.
 */

#ifndef MCA_OPAL_HWLOC_HWLOC132_H
#define MCA_OPAL_HWLOC_HWLOC132_H

BEGIN_C_DECLS

#include "hwloc/include/hwloc.h"

/* If we have verbs.h, then include the hwloc openfabrics helpers
   header file */
#if defined(HAVE_INFINIBAND_VERBS_H)
#include "hwloc/include/hwloc/openfabrics-verbs.h"
#endif

END_C_DECLS

#endif /* MCA_OPAL_HWLOC_HWLOC132_H */
