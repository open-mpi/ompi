/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Musawer Ahmad Saqif.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_COMM_SPLIT_TYPE_H
#define OMPI_COMM_SPLIT_TYPE_H

#include "ompi_config.h"

#include "opal/mca/hwloc/base/base.h"

#include "mpi.h"

BEGIN_C_DECLS

typedef struct {
    const char *info_value;
    const char *hwloc_uri;
    hwloc_obj_type_t hwloc_type;
    int split_type;
    bool use_for_unguided;
    bool report_in_hw_resource_info;
} ompi_comm_split_type_hw_guided_t;

OMPI_HIDDEN extern const ompi_comm_split_type_hw_guided_t
    ompi_comm_split_type_hw_guided_support[];

END_C_DECLS

#endif /* OMPI_COMM_SPLIT_TYPE_H */
