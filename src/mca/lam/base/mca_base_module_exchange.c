/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi/proc/proc.h"
#include "mca/mca.h"
#include "mca/lam/base/mca_base_module_exchange.h"


int mca_base_modex_send(mca_base_module_t *source_module, 
                        void *buffer, size_t size, size_t count)
{
  return LAM_ERR_NOT_IMPLEMENTED;
}


int mca_base_modex_recv(mca_base_module_t *dest_module,
                        lam_proc_t *source_proc,
                        void **buffer, size_t *size, size_t *count)
{
  return LAM_ERR_NOT_IMPLEMENTED;
}
