//
// $HEADER$
//

#include "lam_config.h"

#include <iostream>
#include <string>

#include <stdlib.h>
#include <string.h>

#include "mca/lam/base/base.h"
#include "tools/laminfo/laminfo.h"

using namespace std;
using namespace laminfo;


//
// Public variables
//

laminfo::module_map_t laminfo::module_map;


//
// Private variables
//

static bool opened_modules = false;


//
// Open all MCA modules so that they can register their MCA
// parameters.  Take a shotgun approach here and indiscriminately open
// all modules -- don't be selective.  To this end, we need to clear
// out the environment of all LAM_MPI_mca_<type> variables to ensure
// that the open algorithms don't try to only open one module.
//
void laminfo::open_modules()
{
  laminfo::type_list_t::size_type i;
  string env;
  char *target;

  if (opened_modules)
    return;

  // Clear out the environment.  Use strdup() to orphan the resulting
  // strings because items are placed in the environment by reference,
  // not by value.

  for (i = 0; i < mca_types.size(); ++i) {
    env = "LAM_MPI_mca_" + mca_types[i];
    if (NULL != getenv(env.c_str())) {
      env += "=";
      target = strdup(env.c_str());
      putenv(target);
    }
  }

  // Open all modules

  mca_base_open();

#if 0
  // pcm module opening not implemented yet
  mca_pcm_open();
  module_map("pcm") = mca_pcm_base_module_list;
#endif

#if 0
  // oob module opening not implemented yet
  mca_oob_open();
  module_map("oob") = mca_oob_base_module_list;
#endif

#if 0
  // registry module opening not implemented yet
  mca_registry_open();
  module_map("registry") = mca_registry_base_module_list;
#endif

#if 0
  // coll module opening not implemented yet
  mca_coll_open();
  module_map("coll") = mca_coll_base_module_list;
#endif

#if 0
  // io module opening not implemented yet
  mca_io_open();
  module_map("io") = mca_io_base_module_list;
#endif

#if 0
  // one module opening not implemented yet
  mca_one_open();
  module_map("one") = mca_one_base_module_list;
#endif

#if 0
  // pml module opening not implemented yet
  mca_pml_open();
  module_map("pml") = mca_pml_base_module_list;
#endif

#if 0
  // ptl module opening not implemented yet
  mca_ptl_open();
  module_map("ptl") = mca_ptl_base_module_list;
#endif

#if 0
  // topo module opening not implemented yet
  mca_topo_open();
  module_map("topo") = mca_topo_base_module_list;
#endif

  // All done

  opened_modules = true;
}


void laminfo::close_modules()
{
  if (opened_modules) {
#if 0
    mca_crmpi_base_close();
    mca_coll_base_close();
    mca_pml_base_close();
    mca_boot_close();
    mca_base_close();
#endif

    module_map.clear();
  }

  opened_modules = false;
}
