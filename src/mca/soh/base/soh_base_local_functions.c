/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 */

/*
 * includes
 */
#include "ompi_config.h"

#include "class/ompi_list.h"
#include "mca/mca.h"

#include "mca/soh/base/base.h"

int mca_soh_base_update_cell_soh_not_available(mca_ns_base_cellid_t cellid)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}
