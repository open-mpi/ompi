/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "portals_config.h"

#include "ptl_portals.h"
#include "ptl_portals_compat.h"
#include "ptl_portals_sendfrag.h"

OBJ_CLASS_INSTANCE(mca_ptl_portals_send_frag_t,
                   mca_ptl_base_send_frag_t,
                   NULL, NULL);
