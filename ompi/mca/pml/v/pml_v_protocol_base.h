#ifndef __INCLUDE_V_PROTOCOL_BASE_H_
#define __INCLUDE_V_PROTOCOL_BASE_H_

#include "ompi_config.h"
#include "ompi/request/request.h"
#include "opal/mca/mca.h"
#include "ompi/mca/pml/pml.h"
#include "pml_v_protocol.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

OMPI_DECLSPEC int mca_pml_v_protocol_base_load_all(void);
OMPI_DECLSPEC int mca_pml_v_protocol_base_select(bool enable_progress_threads, bool enable_mpi_threads);

/*
 * Macro for use in components that are of type vprotocol v1.0.0
 */
#define MCA_VPROTOCOL_BASE_VERSION_1_0_0 \
  /* vprotocol v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* vprotocol v1.0 */ \
  "vprotocol", 1, 0, 0

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* __INCLUDE_V_PROTOCOL_BASE_H_ */
