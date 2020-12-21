/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2012 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "opal/util/output.h"

#include <stdlib.h>
#include <string.h>

#include "opal/runtime/opal_progress.h"
#include "opal/mca/btl/base/base.h"
#include "opal/mca/pmix/pmix-internal.h"

#include "ompi/mca/bml/base/base.h"
#include "ompi/mca/bml/base/bml_base_btl.h"
#include "ompi/mca/pml/base/base.h"
#include "ompi/proc/proc.h"

#include "bml_r2.h"
#include "bml_r2_ft.h"

int mca_bml_r2_ft_event(int state)
{
    return OMPI_SUCCESS;
}
