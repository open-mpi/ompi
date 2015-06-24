/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2009 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012-2013 Inria.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/topo/topo.h"
#include "ompi/mca/topo/base/base.h"
#include "ompi/mca/topo/example/topo_example.h"


/*
 * Local functions
 */
static void example_module_constructor(mca_topo_example_module_t *u);
static void example_module_destructor(mca_topo_example_module_t *u);

OBJ_CLASS_INSTANCE(mca_topo_example_module_t, mca_topo_base_module_t,
                   example_module_constructor, example_module_destructor);


static void example_module_constructor(mca_topo_example_module_t *u)
{
    mca_topo_base_module_t *m = &(u->super);

    memset(&m->topo, 0, sizeof(m->topo));

    /* Here we can fill in additional, module-specific data if
       necessary */
    u->example_module_specific_data = 17;
}


static void example_module_destructor(mca_topo_example_module_t *u)
{
    /* Do whatever is necessary to clean up / destroy the module */
}

