/*
 * Copyright (c) 2011-2014 Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * When this component is used, this file is included in the rest of
 * the OPAL/ORTE/OMPI code base via opal/mca/hwloc/hwloc.h.  As such,
 * this header represents the public interface to this static component.
 */

#ifndef MCA_OPAL_HWLOC_EXTERNAL_H
#define MCA_OPAL_HWLOC_EXTERNAL_H

BEGIN_C_DECLS

#include MCA_hwloc_external_header

/* If the including file requested it, also include the hwloc verbs
   helper file.  We can't just always include this file (even if we
   know we have <infiniband/verbs.h>) because there are some inline
   functions in that file that invoke ibv_* functions.  Some linkers
   (e.g., Solaris Studio Compilers) will instantiate those static
   inline functions even if we don't use them, and therefore we need
   to be able to resolve the ibv_* symbols at link time.  

   Since -libverbs is only specified in places where we use other
   ibv_* functions (e.g., the OpenFabrics-based BTLs), that means that
   linking random executables can/will fail (e.g., orterun).
 */
#if defined(OPAL_HWLOC_WANT_VERBS_HELPER) && OPAL_HWLOC_WANT_VERBS_HELPER
#    if defined(HAVE_INFINIBAND_VERBS_H)
#        include MCA_hwloc_external_openfabrics_header
#    else
#        error Tried to include hwloc verbs helper file, but hwloc was compiled with no OpenFabrics support
#    endif
#endif

END_C_DECLS

#endif /* MCA_OPAL_HWLOC_EXTERNAL_H */
