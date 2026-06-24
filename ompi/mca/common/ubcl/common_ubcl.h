/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2025      Bull SAS.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_MCA_COMMON_UBCL_H
#define OMPI_MCA_COMMON_UBCL_H

#include <stddef.h>

#include "ompi/communicator/communicator.h"
#include "ompi/include/mpi.h"
#include "opal/mca/common/ubcl/common_ubcl.h"

/* Holds common variable used in multiple UBCL components */
struct mca_ompi_common_ubcl_component_s {
    int n_addr; /**< Max number of void * addresses in printed stack*/
};
typedef struct mca_ompi_common_ubcl_component_s mca_ompi_common_ubcl_component_t;
extern mca_ompi_common_ubcl_component_t mca_ompi_common_ubcl_component;

int mca_common_ubcl_get_mpi_rank(const int rank, const struct ompi_communicator_t *comm,
                                 const uint64_t ubcl_rank);
void mca_common_ubcl_status_to_ompi(ompi_status_public_t *status,
                                    ubcl_status_t ubcl_status,
                                    struct ompi_communicator_t *comm, int rank);
int ubcl_error_to_ompi(ubcl_error_t code);
/* UBCL rank is on 61 bits, ompi jobid is 32bits, vpid must be truncated to 29bits */
#define COMMON_UBCL_VPID_MAX    (((1 << 29) - 1)) /* We need 3 bits for UBCL rank */
#define PML_UBCL_JOBID_MAX  (OPAL_JOBID_MAX)

/* Error and warning output function used by UBCL components */
void _mca_common_ubcl_error(char *filename, int line, int err, char abort, int verbose,
                            int output, int is_init, int comp_verbose, char *comp_name,
                            char *format, ...);


#endif /* OMPI_MCA_COMMON_UBCL_H */

