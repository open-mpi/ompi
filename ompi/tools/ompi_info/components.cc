//
// Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
//                         University Research and Technology
//                         Corporation.  All rights reserved.
// Copyright (c) 2004-2005 The University of Tennessee and The University
//                         of Tennessee Research Foundation.  All rights
//                         reserved.
// Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
//                         University of Stuttgart.  All rights reserved.
// Copyright (c) 2004-2005 The Regents of the University of California.
//                         All rights reserved.
// Copyright (c) 2006-2008 Cisco Systems, Inc.  All rights reserved.
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

#include "ompi/runtime/params.h"
#include "orte/runtime/runtime.h"
#include "ompi/tools/ompi_info/ompi_info.h"

#include "opal/event/event.h"
#include "opal/mca/base/base.h"
#include "opal/mca/backtrace/backtrace.h"
#include "opal/mca/backtrace/base/base.h"
#include "opal/mca/paffinity/paffinity.h"
#include "opal/mca/paffinity/base/base.h"
#include "opal/mca/carto/carto.h"
#include "opal/mca/carto/base/base.h"
#include "opal/mca/maffinity/maffinity.h"
#include "opal/mca/maffinity/base/base.h"
#include "opal/mca/memory/memory.h"
#include "opal/mca/memory/base/base.h"
#include "opal/mca/memchecker/memchecker.h"
#include "opal/mca/memchecker/base/base.h"
#include "opal/mca/timer/timer.h"
#include "opal/mca/timer/base/base.h"
#include "opal/mca/installdirs/installdirs.h"
#include "opal/mca/installdirs/base/base.h"
#if OPAL_ENABLE_FT == 1
#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"
#endif
#include "opal/runtime/opal.h"
#include "opal/dss/dss.h"

#include "ompi/mca/allocator/allocator.h"
#include "ompi/mca/allocator/base/base.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/mca/io/io.h"
#include "ompi/mca/io/base/base.h"
#include "ompi/mca/mpool/mpool.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/pml/base/base.h"
#include "ompi/mca/bml/bml.h"
#include "ompi/mca/bml/base/base.h"
#include "ompi/mca/rcache/rcache.h"
#include "ompi/mca/rcache/base/base.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/base.h"
#include "ompi/mca/mtl/mtl.h"
#include "ompi/mca/mtl/base/base.h"
#include "ompi/mca/topo/topo.h"
#include "ompi/mca/topo/base/base.h"
#include "ompi/mca/osc/osc.h"
#include "ompi/mca/osc/base/base.h"
#include "ompi/mca/pubsub/base/base.h"
#include "ompi/mca/dpm/base/base.h"

#if OPAL_ENABLE_FT == 1
#include "ompi/mca/crcp/crcp.h"
#include "ompi/mca/crcp/base/base.h"
#endif

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/grpcomm/base/base.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/base/base.h"
#include "orte/util/show_help.h"
#if !ORTE_DISABLE_FULL_SUPPORT
#include "orte/mca/iof/iof.h"
#include "orte/mca/iof/base/base.h"
#include "orte/mca/oob/oob.h"
#include "orte/mca/oob/base/base.h"
#include "orte/mca/odls/odls.h"
#include "orte/mca/odls/base/base.h"
#include "orte/mca/ras/ras.h"
#include "orte/mca/ras/base/ras_private.h"
#include "orte/mca/rmaps/rmaps.h"
#include "orte/mca/rmaps/base/base.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/base/base.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/routed/base/base.h"
#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/base.h"
#if OPAL_ENABLE_FT == 1
#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"
#endif
#include "orte/mca/filem/filem.h"
#include "orte/mca/filem/base/base.h"
#endif

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
  char *target, *save;
  vector<std::string> env_save;
  vector<std::string>::iterator esi;

  if (opened_components) {
      return;
  }

  // Clear out the environment.  Use strdup() to orphan the resulting
  // strings because items are placed in the environment by reference,
  // not by value.

  for (i = 0; i < mca_types.size(); ++i) {
    env = "OMPI_MCA_" + mca_types[i];
    if (NULL != (save = getenv(env.c_str()))) {
      env += "=";
      env_save.push_back(env + save);
      target = strdup(env.c_str());
      putenv(target);
    }
  }

  // some components require the event library be active, so activate
  // it.
  opal_event_init();

  // Open the DSS

  if (ORTE_SUCCESS != opal_dss_open()) {
     printf( "Unable to initialize the DSS\n" );
     return;
  }
    
  // Open up the MCA

  mca_base_open();

  // Register the OPAL layer's MCA parameters

  opal_register_params();

  // Register the ORTE layer's MCA parameters

  orte_register_params();

  // Initialize the opal_output system
  opal_output_init();
    
  // Register the MPI layer's MCA parameters

  ompi_mpi_register_params();

  // Find / open all components

  component_map["base"] = NULL;

  // OPAL frameworks

  opal_backtrace_base_open();
  component_map["backtrace"] = &opal_backtrace_base_components_opened;

  opal_memory_base_open();
  component_map["memory"] = &opal_memory_base_components_opened;

  opal_memchecker_base_open();
  component_map["memchecker"] = &opal_memchecker_base_components_opened;

  opal_paffinity_base_open();
  component_map["paffinity"] = &opal_paffinity_base_components_opened;

  opal_carto_base_open();
  component_map["carto"] = &opal_carto_base_components_opened;

  opal_maffinity_base_open();
  component_map["maffinity"] = &opal_maffinity_base_components_opened;

  opal_timer_base_open();
  component_map["timer"] = &opal_timer_base_components_opened;
#if OPAL_ENABLE_FT == 1
  opal_crs_base_open();
  component_map["crs"] = &opal_crs_base_components_available;
#endif

  // OPAL's installdirs base open has already been called as part of
  // opal_init_util() back in main().
  component_map["installdirs"] = &opal_installdirs_components;

  // ORTE frameworks
  // Set orte_process_info.hnp to true to force all frameworks to
  // open components
  orte_process_info.hnp = true;

  orte_errmgr_base_open();
  component_map["errmgr"] = &orte_errmgr_base_components_available;
    
  orte_grpcomm_base_open();
  component_map["grpcomm"] = &mca_grpcomm_base_components_available;
    
  orte_ess_base_open();
  component_map["ess"] = &orte_ess_base_components_available;
    
#if !ORTE_DISABLE_FULL_SUPPORT
  mca_oob_base_open();
  component_map["oob"] = &mca_oob_base_components;

  orte_odls_base_open();
  component_map["odls"] = &orte_odls_base.available_components;

  orte_iof_base_open();
  component_map["iof"] = &orte_iof_base.iof_components_opened;

  orte_ras_base_open();
  component_map["ras"] = &orte_ras_base.ras_opened;

  orte_rmaps_base_open();
  component_map["rmaps"] = &orte_rmaps_base.available_components;

  orte_rml_base_open();
  component_map["rml"] = &orte_rml_base_components;

  orte_routed_base_open();
  component_map["routed"] = &orte_routed_base_components;
  
  orte_plm_base_open();
  component_map["plm"] = &orte_plm_base.available_components;

#if OPAL_ENABLE_FT == 1
  orte_snapc_base_open();
  component_map["snapc"] = &orte_snapc_base_components_available;
#endif

  orte_filem_base_open();
  component_map["filem"] = &orte_filem_base_components_available;
#endif

  // MPI frameworks

  mca_allocator_base_open();
  component_map["allocator"] = &mca_allocator_base_components;

  mca_coll_base_open();
  component_map["coll"] = &mca_coll_base_components_opened;

  mca_io_base_open();
  component_map["io"] = &mca_io_base_components_opened;

  mca_rcache_base_open();
  component_map["rcache"] = &mca_rcache_base_components;

  mca_mpool_base_open();
  component_map["mpool"] = &mca_mpool_base_components;

  mca_pml_base_open();
  component_map["pml"] = &mca_pml_base_components_available;

  // No need to call the bml_base_open() because the ob1 pml calls it.
  //mca_bml_base_open();
  component_map["bml"] = &mca_bml_base_components_available;

  ompi_osc_base_open();
  component_map["osc"] = &ompi_osc_base_open_components;

  mca_btl_base_open();
  component_map["btl"] = &mca_btl_base_components_opened;

  ompi_mtl_base_open();
  component_map["mtl"] = &ompi_mtl_base_components_opened;

  mca_topo_base_open();
  component_map["topo"] = &mca_topo_base_components_opened;

  ompi_pubsub_base_open();
  component_map["pubsub"] = &ompi_pubsub_base_components_available;
  
  ompi_dpm_base_open();
  component_map["dpm"] = &ompi_dpm_base_components_available;

#if OPAL_ENABLE_FT == 1
  ompi_crcp_base_open();
  component_map["crcp"] = &ompi_crcp_base_components_available;
#endif

  // Restore the environment to what it was before we started so that
  // if users setenv OMPI_MCA_<framework name> to some value, they'll
  // see that value when it is shown via --param output.

  if (!env_save.empty()) {
      for (esi = env_save.begin(); esi != env_save.end(); ++esi) {
          target = strdup(esi->c_str());
          putenv(target);
      }
  }

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

#if OPAL_ENABLE_FT == 1
        ompi_crcp_base_close();
#endif
        ompi_dpm_base_close();
        ompi_pubsub_base_close();
        mca_topo_base_close();
        // the PML has to call the base PTL close function.
        mca_btl_base_close();
        ompi_mtl_base_close();
        mca_pml_base_close();
        mca_mpool_base_close();
        mca_rcache_base_close();
        mca_io_base_close();
        mca_coll_base_close();
        mca_allocator_base_close();
        ompi_osc_base_close();

        orte_grpcomm_base_close();
        orte_ess_base_close();
        orte_show_help_finalize();
#if !ORTE_DISABLE_FULL_SUPPORT
#if OPAL_ENABLE_FT == 1
        orte_snapc_base_close();
#endif
        orte_filem_base_close();
        orte_iof_base_close();
        orte_plm_base_close();
        orte_odls_base_close();
        orte_rmaps_base_close();
        orte_ras_base_close();
        orte_rml_base_close();
        orte_routed_base_close();
        mca_oob_base_close();
#endif
        orte_errmgr_base_close();
      
        opal_backtrace_base_close();
        opal_memory_base_close();
        opal_memchecker_base_close();
        opal_paffinity_base_close();
        opal_carto_base_close();
        opal_maffinity_base_close();
        opal_timer_base_close();
#if OPAL_ENABLE_FT == 1
        opal_crs_base_close();
#endif

        // Do not call OPAL's installdirs close; it will be handled in
        // opal_finalize_util().

        component_map.clear();
    }

    opened_components = false;
}
