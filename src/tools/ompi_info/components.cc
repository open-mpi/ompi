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
#include "runtime/runtime.h"
#include "tools/ompi_info/ompi_info.h"

#include "mca/base/base.h"
#include "mca/allocator/allocator.h"
#include "mca/allocator/base/base.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"
#include "mca/io/io.h"
#include "mca/io/base/base.h"
#include "mca/mpool/mpool.h"
#include "mca/mpool/base/base.h"
#include "mca/pml/pml.h"
#include "mca/pml/base/base.h"
#include "mca/ptl/ptl.h"
#include "mca/ptl/base/base.h"
#include "mca/topo/topo.h"
#include "mca/topo/base/base.h"

#include "mca/errmgr/errmgr.h"
#include "mca/errmgr/base/base.h"
#include "mca/gpr/gpr.h"
#include "mca/gpr/base/base.h"
#include "mca/iof/iof.h"
#include "mca/iof/base/base.h"
#include "mca/ns/ns.h"
#include "mca/ns/base/base.h"
#include "mca/oob/oob.h"
#include "mca/oob/base/base.h"
#include "mca/ras/ras.h"
#include "mca/ras/base/base.h"
#include "mca/rds/rds.h"
#include "mca/rds/base/base.h"
#include "mca/rmaps/rmaps.h"
#include "mca/rmaps/base/base.h"
#include "mca/rmgr/rmgr.h"
#include "mca/rmgr/base/base.h"
#include "mca/rml/rml.h"
#include "mca/rml/base/base.h"
#include "mca/pls/pls.h"
#include "mca/pls/base/base.h"
#include "mca/soh/soh.h"
#include "mca/soh/base/base.h"

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

  // ORTE frameworks

  mca_oob_base_open();
  component_map["oob"] = &mca_oob_base_components;

  orte_errmgr_base_open();
  component_map["errmgr"] = &orte_errmgr_base_components_available;

  orte_gpr_base_open();
  component_map["gpr"] = &orte_gpr_base_components_available;

  orte_iof_base_open();
  component_map["iof"] = &orte_iof_base.iof_components_opened;

  orte_ns_base_open();
  component_map["ns"] = &mca_ns_base_components_available;

  orte_ras_base_open();
  component_map["ras"] = &orte_ras_base.ras_opened;

  orte_rds_base_open();
  component_map["rds"] = &orte_rds_base.rds_components;

  orte_rmaps_base_open();
  component_map["rmaps"] = &orte_rmaps_base.rmaps_opened;

  orte_rmgr_base_open();
  component_map["rmgr"] = &orte_rmgr_base.rmgr_components;

  orte_rml_base_open();
  component_map["rml"] = &orte_rml_base.rml_components;

  orte_pls_base_open();
  component_map["pls"] = &orte_pls_base.pls_opened;

  orte_soh_base_open();
  component_map["soh"] = &orte_soh_base.soh_components;

  // MPI frameworks

  mca_allocator_base_open();
  component_map["allocator"] = &mca_allocator_base_components;

  mca_coll_base_open();
  component_map["coll"] = &mca_coll_base_components_opened;

  mca_io_base_open();
  component_map["io"] = &mca_io_base_components_opened;

  mca_mpool_base_open();
  component_map["mpool"] = &mca_mpool_base_components;

  mca_pml_base_open();
  component_map["pml"] = &mca_pml_base_components_available;

  mca_ptl_base_open();
  component_map["ptl"] = &mca_ptl_base_components_opened;

  mca_topo_base_open();
  component_map["topo"] = &mca_topo_base_components_opened;

  // All done

  opened_components = true;
}


void ompi_info::close_components()
{
    if (opened_components) {

        // Note that the order of shutdown here doesn't matter because
        // we aren't *using* any components -- none were selected, so
        // there are no dependencies between the frameworks.  We list
        // them generally "in order", but it doesn't really matter.

        mca_topo_base_close();
        mca_ptl_base_close();
        mca_pml_base_close();
        mca_mpool_base_close();
        mca_io_base_close();
        mca_coll_base_close();
        mca_allocator_base_close();

        orte_iof_base_close();
        orte_soh_base_close();
        orte_pls_base_close();
        orte_rml_base_close();
        orte_rmgr_base_close();
        orte_rmaps_base_close();
        orte_rds_base_close();
        orte_ras_base_close();
        orte_ns_base_close();
        orte_gpr_base_close();
        orte_errmgr_base_close();
        mca_oob_base_close();
    
        component_map.clear();
    }

    opened_components = false;
}
