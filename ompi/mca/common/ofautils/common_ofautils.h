/*
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 *                         All rights reserved.
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef _COMMON_OFAUTILS_H_
#define _COMMON_OFAUTILS_H_

#include "ompi_config.h"

OMPI_DECLSPEC extern
struct ibv_device **ibv_get_device_list_compat(int *num_devs);

OMPI_DECLSPEC extern
void ibv_free_device_list_compat(struct ibv_device **ib_devs);

END_C_DECLS

#endif

