//
// Copyright (c) 2004-2005 The Trustees of Indiana University.
//                         All rights reserved.
// Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
//                         All rights reserved.
// Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
//                         University of Stuttgart.  All rights reserved.
// $COPYRIGHT$
// 
// Additional copyrights may follow
// 
// $HEADER$
//

#include "ompi_config.h"

#include <iostream>
#include <string>

#include <stdlib.h>
#include <string.h>

#include "mpi/runtime/params.h"
#include "mca/base/base.h"
#include "mca/allocator/allocator.h"
#include "mca/allocator/base/base.h"
#include "mca/mpool/mpool.h"
#include "mca/mpool/base/base.h"
#include "mca/llm/llm.h"
#include "mca/llm/base/base.h"
#include "mca/pcm/pcm.h"
#include "mca/pcm/base/base.h"
#include "mca/pcmclient/pcmclient.h"
#include "mca/pcmclient/base/base.h"
#include "mca/oob/oob.h"
#include "mca/oob/base/base.h"
#include "mca/pml/pml.h"
#include "mca/pml/base/base.h"
#include "mca/ptl/ptl.h"
#include "mca/ptl/base/base.h"
#include "mca/soh/soh.h"
#include "mca/soh/base/base.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"
#include "mca/ns/ns.h"
#include "mca/ns/base/base.h"
#include "mca/topo/topo.h"
#include "mca/topo/base/base.h"
#include "mca/gpr/gpr.h"
#include "mca/gpr/base/base.h"
#include "mca/io/io.h"
#include "mca/io/base/base.h"
#include "tools/ompi_info/ompi_info.h"

using namespace std;
using namespace ompi_info;


//
// Public variables
//

ompi_info::component_map_t ompi_info::component_map;


//
// Private variables
//

static bool opened_components = false;


//
// Open all MCA components so that they can register their MCA
// parameters.  Take a shotgun approach here and indiscriminately open
// all components -- don't be selective.  To this end, we need to clear
// out the environment of all OMPI_MCA_<type> variables to ensure
// that the open algorithms don't try to only open one component.
//
void ompi_info::open_components()
{
  ompi_info::type_vector_t::size_type i;
  string env;
  char *target;

  if (opened_components)
    return;

  // Clear out the environment.  Use strdup() to orphan the resulting
  // strings because items are placed in the environment by reference,
  // not by value.

  for (i = 0; i < mca_types.size(); ++i) {
    env = "OMPI_MCA_" + mca_types[i];
    if (NULL != getenv(env.c_str())) {
      env += "=";
      target = strdup(env.c_str());
      putenv(target);
    }
  }

  // Open up the MCA

  mca_base_open();

  // Register the MPI layer's MCA parameters

  ompi_mpi_register_params();

  // Find / open all components

  component_map["base"] = NULL;

  mca_allocator_base_open();
  component_map["allocator"] = &mca_allocator_base_components;

  mca_coll_base_open();
  component_map["coll"] = &mca_coll_base_components_opened;

  mca_gpr_base_open();
  component_map["gpr"] = &mca_gpr_base_components_available;

  mca_llm_base_open();
  component_map["llm"] = &mca_llm_base_components_available;

  mca_io_base_open();
  component_map["io"] = &mca_io_base_components_opened;

  mca_ns_base_open();
  component_map["ns"] = &mca_ns_base_components_available;

  mca_mpool_base_open();
  component_map["mpool"] = &mca_mpool_base_components;

#if 0
  // one component opening not implemented yet
  mca_one_base_open();
  component_map["one"] = &mca_one_base_components_available;
#else
  component_map["one"] = NULL;
#endif

  mca_oob_base_open();
  component_map["oob"] = &mca_oob_base_components;

#if 0
  // op component framework not yet implemented
  mca_op_base_open();
  component_map["op"] = &mca_oob_base_components;
#else
  component_map["op"] = NULL;
#endif

  mca_pcm_base_open();
  component_map["pcm"] = &mca_pcm_base_components_available;

  mca_pcmclient_base_open();
  component_map["pcmclient"] = &mca_pcmclient_base_components_available;

  mca_pml_base_open();
  component_map["pml"] = &mca_pml_base_components_available;

  mca_ptl_base_open();
  component_map["ptl"] = &mca_ptl_base_components_opened;

#if 0
  // JMS waiting for ralph to finish
  mca_soh_base_open();
  component_map["soh"] = &mca_soh_base_components_available;
#endif

  mca_topo_base_open();
  component_map["topo"] = &mca_topo_base_components_opened;

  // All done

  opened_components = true;
}


void ompi_info::close_components()
{
  if (opened_components) {
    mca_pcm_base_close();
    mca_oob_base_close();
    mca_ns_base_close();
    mca_gpr_base_close();
#if 0
    // JMS waiting for ralph to finish
    mca_soh_base_close();
#endif
    mca_coll_base_close();
    mca_pml_base_close();
    mca_ptl_base_close();
    mca_topo_base_close();
    mca_mpool_base_close();
    mca_llm_base_close();
    mca_allocator_base_close();
    mca_base_close();

    component_map.clear();
  }

  opened_components = false;
}
