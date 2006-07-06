/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/datatype/datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/datatype/convertor.h"
#include "ompi/mca/mtl/base/mtl_base_datatype.h"

#include "mtl_mx.h"
#include "mtl_mx_request.h"


int
ompi_mtl_mx_isend(struct mca_mtl_base_module_t* mtl, 
                  struct ompi_communicator_t* comm,
                  int dest,
                  int tag,
                  struct ompi_convertor_t *convertor,
                  mca_pml_base_send_mode_t mode,
                  bool blocking,
                  mca_mtl_request_t * mtl_request)
{
    mx_return_t mx_return;
    uint64_t match_bits;
    int ret;
    mca_mtl_mx_request_t * mtl_mx_request = (mca_mtl_mx_request_t*) mtl_request;
    size_t length;

    
mca_mtl_mx_endpoint_t* mx_endpoint = 
        (mca_mtl_mx_endpoint_t*) comm->c_pml_procs[dest]->proc_ompi->proc_pml;

    assert(mtl == &ompi_mtl_mx.super);

    MX_SET_SEND_BITS(match_bits, comm->c_contextid, comm->c_my_rank, tag); 
    
    ret = ompi_mtl_datatype_pack(convertor, 
                                 &mtl_mx_request->mx_segment[0].segment_ptr, 
                                 &length, 
                                 &mtl_mx_request->free_after);
    mtl_mx_request->mx_segment[0].segment_length = length;
    mtl_mx_request->convertor = convertor;
    mtl_mx_request->type = OMPI_MTL_MX_ISEND;

    if (OMPI_SUCCESS != ret) return ret;
    
    if(mode == MCA_PML_BASE_SEND_SYNCHRONOUS) { 
        
#if 0
        printf("issend bits: 0x%016llx\n", match_bits);
#endif
        mx_return = mx_issend( ompi_mtl_mx.mx_endpoint, 
                               mtl_mx_request->mx_segment, 
                               1,
                               mx_endpoint->mx_peer_addr, 
                               match_bits, 
                               mtl_mx_request, 
                               &mtl_mx_request->mx_request
                               );
        if(mx_return != MX_SUCCESS ) { 
            char peer_name[MX_MAX_HOSTNAME_LEN];
            if(MX_SUCCESS != mx_nic_id_to_hostname( mx_endpoint->mx_peer->nic_id, peer_name)) { 
                sprintf( peer_name, "unknown %lx nic_id", (long)mx_endpoint->mx_peer->nic_id ); 
            }
            opal_output(0, "Error in mx_issend (error %s) sending to %s\n", mx_strerror(mx_return), peer_name);
        }
    } else { 
#if 0
        printf("isend bits:  0x%016llx\n", match_bits);
#endif        
        mx_return = mx_isend( ompi_mtl_mx.mx_endpoint, 
                              mtl_mx_request->mx_segment,
                              1,
                              mx_endpoint->mx_peer_addr,
                              match_bits,
                              mtl_mx_request,
                              &mtl_mx_request->mx_request
                              );
        
        if(mx_return != MX_SUCCESS ) { 
            char peer_name[MX_MAX_HOSTNAME_LEN];
            if(MX_SUCCESS != mx_nic_id_to_hostname( mx_endpoint->mx_peer->nic_id, peer_name)) { 
                sprintf( peer_name, "unknown %lx nic_id", (long)mx_endpoint->mx_peer->nic_id ); 
            }
            opal_output(0, "Error in mx_isend (error %s) sending to %s\n", mx_strerror(mx_return), peer_name);
        }
  
    }
    
    return mx_return == MX_SUCCESS ? OMPI_SUCCESS : OMPI_ERROR;
}
