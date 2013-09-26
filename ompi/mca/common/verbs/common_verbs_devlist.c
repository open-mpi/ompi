/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2006-2007 Voltaire All rights reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <infiniband/verbs.h>
/* This is crummy, but <infiniband/driver.h> doesn't work on all
   platforms with all compilers.  Specifically, trying to include it
   on RHEL4U3 with the PGI 32 bit compiler will cause problems because
   certain 64 bit types are not defined.  Per advice from Roland D.,
   just include the one prototype that we need in this case
   (ibv_get_sysfs_path()). */
#ifdef HAVE_INFINIBAND_DRIVER_H
#include <infiniband/driver.h>
#else
const char *ibv_get_sysfs_path(void);
#endif
#include "opal/util/output.h"
#include "common_verbs.h"


/*
 * Portable wrapper around ibv_get_device_list() / ibv_get_devices().
 */
struct ibv_device **ompi_ibv_get_device_list(int *num_devs)
{
    struct ibv_device **ib_devs;

#ifdef HAVE_IBV_GET_DEVICE_LIST
    ib_devs = ibv_get_device_list(num_devs);
#else
    struct dlist *dev_list;
    struct ibv_device *ib_dev;
    *num_devs = 0;

    /* Determine the number of device's available on the host */
    dev_list = ibv_get_devices();
    if (NULL == dev_list) {
        return NULL;
    }

    dlist_start(dev_list);

    dlist_for_each_data(dev_list, ib_dev, struct ibv_device)
        (*num_devs)++;

    /* Allocate space for the ib devices */
    ib_devs = (struct ibv_device**)malloc(*num_devs * sizeof(struct ibv_dev*));
    if (NULL == ib_devs) {
        *num_devs = 0;
        opal_output(0, "Failed malloc: %s:%d", __FILE__, __LINE__);
        return NULL;
    }

    dlist_start(dev_list);

    dlist_for_each_data(dev_list, ib_dev, struct ibv_device)
        *(++ib_devs) = ib_dev;
#endif

    return ib_devs;
}


void ompi_ibv_free_device_list(struct ibv_device **ib_devs)
{
#ifdef HAVE_IBV_GET_DEVICE_LIST
    ibv_free_device_list(ib_devs);
#else
    free(ib_devs);
#endif
}
