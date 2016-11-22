/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PML_UCX_DATATYPE_H_
#define PML_UCX_DATATYPE_H_

#include "pml_ucx.h"


struct pml_ucx_convertor {
    opal_free_list_item_t     super;
    ompi_datatype_t           *datatype;
    opal_convertor_t          opal_conv;
};


ucp_datatype_t mca_pml_ucx_init_datatype(ompi_datatype_t *datatype);

OBJ_CLASS_DECLARATION(mca_pml_ucx_convertor_t);


static inline ucp_datatype_t mca_pml_ucx_get_datatype(ompi_datatype_t *datatype)
{
    ucp_datatype_t ucp_type = datatype->pml_data;

    if (OPAL_LIKELY(ucp_type != 0)) {
        return ucp_type;
    }

    return mca_pml_ucx_init_datatype(datatype);
}

#endif /* PML_UCX_DATATYPE_H_ */
