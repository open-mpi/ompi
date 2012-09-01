/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
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
 * Copyright (c) 2006-2009 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2006-2012 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2006-2007 Voltaire All rights reserved.
 * Copyright (c) 2009-2012 Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2011      NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2012      Oak Ridge National Laboratory.  All rights reserved
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <infiniband/verbs.h>

#include "ompi/constants.h"

#include "common_verbs.h"

int ompi_common_verbs_port_bw(struct ibv_port_attr *port_attr,
                              uint32_t *bandwidth)
{
    *bandwidth = 0;

    /* To calculate the bandwidth available on this port, we have to
       look up the values corresponding to port->active_speed and
       port->active_width.  These are enums corresponding to the IB
       spec.  Overall forumula to get the true link speed is 8/10 or
       64/66 of the reported speed (depends on the coding that is
       being used for the particular speed) times the number of
       links. */
    switch (port_attr->active_speed) {
    case 1:
        /* SDR: 2.5 Gbps * 0.8, in megabits */
        *bandwidth = 2000;
        break;
    case 2:
        /* DDR: 5 Gbps * 0.8, in megabits */
        *bandwidth = 4000;
        break;
    case 4:
        /* QDR: 10 Gbps * 0.8, in megabits */
        *bandwidth = 8000;
        break;
    case 8:
        /* FDR10: 10.3125 Gbps * 64/66, in megabits */
        *bandwidth = 10000;
        break;
    case 16:
        /* FDR: 14.0625 Gbps * 64/66, in megabits */
        *bandwidth = 13636;
        break;
    case 32:
        /* EDR: 25.78125 Gbps * 64/66, in megabits */
        *bandwidth = 25000;
        break;
    default:
        /* Who knows? */
        return OMPI_ERR_NOT_FOUND;
    }

    switch (port_attr->active_width) {
    case 1:
        /* 1x */
        /* unity */
        break;
    case 2:
        /* 4x */
        *bandwidth *= 4;
        break;
    case 4:
        /* 8x */
        *bandwidth *= 8;
        break;
    case 8:
        /* 12x */
        *bandwidth *= 12;
        break;
    default:
        /* Who knows? */
        return OMPI_ERR_NOT_FOUND;
    }

    return OMPI_SUCCESS;
}


int ompi_common_verbs_mtu(struct ibv_port_attr *port_attr)
{
    if (NULL == port_attr) {
        return 0;
    }

    switch(port_attr->active_mtu) {
    case IBV_MTU_256:  return 256;
    case IBV_MTU_512:  return 512;
    case IBV_MTU_1024: return 1024;
    case IBV_MTU_2048: return 2048;
    case IBV_MTU_4096: return 4096;
    default:           return 0;
    }
}
