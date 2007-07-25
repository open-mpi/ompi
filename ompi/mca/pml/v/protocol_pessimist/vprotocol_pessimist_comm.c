#include "ompi_config.h"
#include "../pml_v.h"
#include "vprotocol_pessimist.h"

int mca_vprotocol_pessimist_add_comm(struct ompi_communicator_t* comm)
{
  return mca_pml_v.host_pml.pml_add_comm(comm);
}

int mca_vprotocol_pessimist_del_comm(struct ompi_communicator_t* comm)
{
  return mca_pml_v.host_pml.pml_del_comm(comm);
}
