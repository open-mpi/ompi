/*
 * Copyright (c) 2013-2015 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2013-2018 Inria.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <ompi_config.h>
#include "pml_monitoring.h"
#include <ompi/mca/pml/base/pml_base_request.h>

/* manage persistant requests*/
int mca_pml_monitoring_start(size_t count,
                             ompi_request_t** requests)
{
    size_t i;

    for( i = 0; i < count; i++ ) {
        mca_pml_base_request_t *pml_request = (mca_pml_base_request_t*)requests[i];
        int world_rank;

        if(NULL == pml_request) {
            continue;
        }
        if(OMPI_REQUEST_PML != requests[i]->req_type) {
            continue;
        }
        if(MCA_PML_REQUEST_SEND != pml_request->req_type) {
            continue;
        }

        /**
         * If this fails the destination is not part of my MPI_COM_WORLD
         */
        if(OPAL_SUCCESS == mca_common_monitoring_get_world_rank(pml_request->req_peer,
                                                                pml_request->req_comm->c_remote_group,
								&world_rank)) {
            size_t type_size, data_size;
            ompi_datatype_type_size(pml_request->req_datatype, &type_size);
            data_size = pml_request->req_count * type_size;
            mca_common_monitoring_record_pml(world_rank, data_size, 1);
        }
    }
    return pml_selected_module.pml_start(count, requests);
}

