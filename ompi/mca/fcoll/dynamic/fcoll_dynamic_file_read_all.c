/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2015 University of Houston. All rights reserved.
 * Copyright (c) 2015-2018 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017      IBM Corporation. All rights reserved.
 * Copyright (c) 2024      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "fcoll_dynamic.h"

#include "mpi.h"
#include "ompi/mca/common/ompio/common_ompio.h"



int
mca_fcoll_dynamic_file_read_all (struct ompio_file_t *fh,
                                 void *buf,
                                 size_t count,
                                 struct ompi_datatype_t *datatype,
                                 ompi_status_public_t *status)
{
    return mca_common_ompio_base_file_read_all (fh, buf, count, datatype, status);
}


