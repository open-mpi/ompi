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
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif
#include <fcntl.h>
#include <errno.h>

#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/sbgp/basesmuma/sbgp_basesmuma.h"

/*
 * Local functions
 */
static void
mca_sbgp_basesmuma_module_construct(mca_sbgp_basesmuma_module_t *module)
{
}

static void
mca_sbgp_basesmuma_module_destruct(mca_sbgp_basesmuma_module_t *module)
{
    /* done */
}

OBJ_CLASS_INSTANCE(mca_sbgp_basesmuma_module_t,
                   mca_sbgp_base_module_t,
                   mca_sbgp_basesmuma_module_construct,
                   mca_sbgp_basesmuma_module_destruct);
