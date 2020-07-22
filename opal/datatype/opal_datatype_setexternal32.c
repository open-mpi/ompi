/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2020      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <stddef.h>

#include "opal/runtime/opal.h"
#include "opal/util/arch.h"
#include "opal/util/output.h"
#include "opal/datatype/opal_datatype_internal.h"
#include "opal/datatype/opal_datatype.h"
#include "opal/datatype/opal_convertor_internal.h"
#include "opal/mca/base/mca_base_var.h"

void
opal_convertor_set_ompi_remote_size(opal_convertor_t *convertor, int idx, int size)
{
    convertor->master->ompi_remote_sizes[idx] = size;
}
void
opal_convertor_ompi_remote_size_is_ready(opal_convertor_t *convertor)
{
    convertor->master->ompi_remote_sizes_is_set = 1;
}
