#include "ompi_config.h"
#include "vprotocol_example.h"
#include "vprotocol_example_start.h"

OMPI_DECLSPEC int mca_vprotocol_example_start(size_t count, ompi_request_t **requests)
{ 
  V_OUTPUT_VERBOSE(50, "starting %d requests", count);
# ifdef OMPI_ENABLE_DEBUG



#endif
  return mca_pml_v.host_pml.pml_start(count, requests);
}
