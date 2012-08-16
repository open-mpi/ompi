/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 */

#include "ompi_config.h"
#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <errno.h>

#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/sbgp/basesmsocket/sbgp_basesmsocket.h"
#include "orte/mca/rml/rml.h"
#include "orte/util/proc_info.h"

/*
 * Local functions
 */
static void
mca_sbgp_basesmsocket_module_construct(mca_sbgp_basesmsocket_module_t *module)
{
}

static void
mca_sbgp_basesmsocket_module_destruct(mca_sbgp_basesmsocket_module_t *module)
{
    /* done */
}

OBJ_CLASS_INSTANCE(mca_sbgp_basesmsocket_module_t,
                   mca_sbgp_base_module_t,
                   mca_sbgp_basesmsocket_module_construct,
                   mca_sbgp_basesmsocket_module_destruct);
