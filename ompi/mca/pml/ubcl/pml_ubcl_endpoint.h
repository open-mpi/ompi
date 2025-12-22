#ifndef PML_UBCL_ENDPOINT_INCLUDE_H
#define PML_UBCL_ENDPOINT_INCLUDE_H

#include <stdint.h>
#include "opal/util/proc.h"
/** * Endpoint structure */
#include "opal/mca/common/ubcl/common_ubcl.h"

/* endpoint functions */

int mca_pml_ubcl_create_local_endpoint(void);
int mca_pml_ubcl_free_local_endpoints(void);
int mca_pml_ubcl_endpoint_release(ompi_proc_t *proc);
void mca_pml_ubcl_endpoint_retain(ompi_proc_t *proc);
int mca_pml_ubcl_add_procs(ompi_proc_t **procs, size_t nprocs);
int mca_pml_ubcl_del_procs(ompi_proc_t **procs, size_t nprocs);

#endif /* #ifndef PML_UBCL_ENDPOINT_INCLUDE_H */
