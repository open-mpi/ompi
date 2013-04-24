/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2011-2012 University of Houston. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdlib.h>
#include <string.h>

#include "opal/util/argv.h"
#include "opal/runtime/opal_info_support.h"

#if OMPI_RTE_ORTE
#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_info_support.h"
#endif

#include "ompi/mca/allocator/base/base.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/mca/io/io.h"
#include "ompi/mca/io/base/base.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/pml/base/base.h"
#include "ompi/mca/bml/base/base.h"
#include "ompi/mca/rcache/rcache.h"
#include "ompi/mca/rcache/base/base.h"
#include "ompi/mca/btl/base/base.h"
#include "ompi/mca/mtl/mtl.h"
#include "ompi/mca/mtl/base/base.h"
#include "ompi/mca/topo/base/base.h"
#include "ompi/mca/osc/osc.h"
#include "ompi/mca/osc/base/base.h"
#include "ompi/mca/op/base/base.h"
#include "ompi/mca/vprotocol/base/base.h"
#include "ompi/mca/fbtl/fbtl.h"
#include "ompi/mca/fbtl/base/base.h"
#include "ompi/mca/fs/fs.h"
#include "ompi/mca/fs/base/base.h"
#include "ompi/mca/fcoll/fcoll.h"
#include "ompi/mca/fcoll/base/base.h"
#include "ompi/mca/sharedfp/sharedfp.h"
#include "ompi/mca/sharedfp/base/base.h"
#include "ompi/runtime/params.h"

#if OPAL_ENABLE_FT_CR == 1
#include "ompi/mca/crcp/crcp.h"
#include "ompi/mca/crcp/base/base.h"
#endif
#include "ompi/include/ompi/frameworks.h"

#include "ompi/tools/ompi_info/ompi_info.h"


/*
 * Private variables
 */

void ompi_info_close_components()
{
    int i;

    /* Note that the order of shutdown here doesn't matter because
     * we aren't *using* any components -- none were selected, so
     * there are no dependencies between the frameworks.  We list
     * them generally "in order", but it doesn't really matter.
         
     * We also explicitly ignore the return values from the
     * close() functions -- what would we do if there was an
     * error?
     */
    for (i=0; NULL != ompi_frameworks[i]; i++) {
        (void) mca_base_framework_close(ompi_frameworks[i]);
    }

#if OMPI_RTE_ORTE
    /* close the ORTE components */
    (void) orte_info_close_components();
#endif
}
