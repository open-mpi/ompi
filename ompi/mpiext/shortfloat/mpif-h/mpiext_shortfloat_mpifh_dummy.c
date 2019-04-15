/*
 * Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
 * Copyright (c) 2019 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/* MacOS will not allow having an empty library file.
 * So we must have at least one .c file with a function symbol in it,
 * even if we don't use that function anywhere. */

#include "ompi_config.h"

int ompi_mpiexec_shortfloat_mpifh_dummy(int i);

int ompi_mpiexec_shortfloat_mpifh_dummy(int i)
{
    return i + 1;
}
