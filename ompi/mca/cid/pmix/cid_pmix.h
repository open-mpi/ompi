/*
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef _MCA_CID_PMIX_H_
#define _MCA_CID_PMIX_H

#include "ompi_config.h"

#include "opal/mca/base/base.h"

#include "ompi/mca/cid/cid.h"


BEGIN_C_DECLS

ORTE_MODULE_DECLSPEC extern ompi_cid_base_component_t mca_cid_pmix_component;
extern ompi_cid_base_module_t ompi_cid_pmix_module;

END_C_DECLS

#endif /* MCA_CID_PMIX_H_ */
