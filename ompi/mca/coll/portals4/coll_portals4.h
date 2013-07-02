/*
 * Copyright (c) 2013      Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_COLL_PORTALS4_EXPORT_H
#define MCA_COLL_PORTALS4_EXPORT_H

#include "ompi_config.h"

#include "mpi.h"
#include "opal/mca/mca.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/request/request.h"
#include "ompi/communicator/communicator.h"

BEGIN_C_DECLS

OMPI_MODULE_DECLSPEC extern const mca_coll_base_component_2_0_0_t mca_coll_portals4_component;

struct mca_coll_portals4_module_t {
    mca_coll_base_module_t super;
};
typedef struct mca_coll_portals4_module_t mca_coll_portals4_module_t;
OBJ_CLASS_DECLARATION(mca_coll_portals4_module_t);

END_C_DECLS

#endif /* MCA_COLL_PORTALS4_EXPORT_H */
