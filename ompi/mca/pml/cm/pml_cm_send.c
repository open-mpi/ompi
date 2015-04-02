/*
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2013      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/prefetch.h"

#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"

#include "pml_cm.h"
#include "pml_cm_sendreq.h"

int
mca_pml_cm_isend_init(void* buf,
                        size_t count,
                        ompi_datatype_t* datatype,
                        int dst,
                        int tag,
                        mca_pml_base_send_mode_t sendmode,
                        ompi_communicator_t* comm,
                        ompi_request_t** request)
{
    mca_pml_cm_hvy_send_request_t *sendreq;
    ompi_proc_t* ompi_proc;
    
    MCA_PML_CM_HVY_SEND_REQUEST_ALLOC(sendreq, comm, dst, ompi_proc);
    if (OPAL_UNLIKELY(NULL == sendreq)) return OMPI_ERR_OUT_OF_RESOURCE;
    
    MCA_PML_CM_HVY_SEND_REQUEST_INIT(sendreq, ompi_proc, comm, tag, dst, 
                                     datatype, sendmode, true, false, buf, count);
    
    *request = (ompi_request_t*) sendreq;

    return OMPI_SUCCESS;
}


int
mca_pml_cm_isend(void* buf,
                   size_t count,
                   ompi_datatype_t* datatype,
                   int dst,
                   int tag,
                   mca_pml_base_send_mode_t sendmode,
                   ompi_communicator_t* comm,
                   ompi_request_t** request)
{
    int ret;
  
    if(sendmode == MCA_PML_BASE_SEND_BUFFERED ) { 
        mca_pml_cm_hvy_send_request_t* sendreq;
        ompi_proc_t* ompi_proc;
        
        MCA_PML_CM_HVY_SEND_REQUEST_ALLOC(sendreq, comm, dst, ompi_proc);
        if (OPAL_UNLIKELY(NULL == sendreq)) return OMPI_ERR_OUT_OF_RESOURCE;
        
        MCA_PML_CM_HVY_SEND_REQUEST_INIT(sendreq, 
                                         ompi_proc, 
                                         comm, 
                                         tag, 
                                         dst, 
                                         datatype,
                                         sendmode,
                                         false,
                                         false,
                                         buf, 
                                         count);
        
        MCA_PML_CM_HVY_SEND_REQUEST_START( sendreq, ret);
        
        if (OPAL_LIKELY(OMPI_SUCCESS == ret)) *request = (ompi_request_t*) sendreq;

    } else { 
        mca_pml_cm_thin_send_request_t* sendreq;
        ompi_proc_t* ompi_proc;
        MCA_PML_CM_THIN_SEND_REQUEST_ALLOC(sendreq, comm, dst, ompi_proc);
        if (OPAL_UNLIKELY(NULL == sendreq)) return OMPI_ERR_OUT_OF_RESOURCE;
        
        MCA_PML_CM_THIN_SEND_REQUEST_INIT(sendreq, 
                                          ompi_proc, 
                                          comm, 
                                          tag, 
                                          dst, 
                                          datatype,
                                          sendmode,
                                          buf, 
                                          count);
        
        MCA_PML_CM_THIN_SEND_REQUEST_START(
                                           sendreq, 
                                           comm,
                                           tag,
                                           dst,
                                           sendmode,
                                           false, 
                                           ret);
        
        if (OPAL_LIKELY(OMPI_SUCCESS == ret)) *request = (ompi_request_t*) sendreq;
        
    }
       
    return ret;
}


int
mca_pml_cm_send(void *buf,
                size_t count,
                ompi_datatype_t* datatype,
                int dst,
                int tag,
                mca_pml_base_send_mode_t sendmode,
                ompi_communicator_t* comm)
{
    int ret = OMPI_ERROR;

    if(sendmode == MCA_PML_BASE_SEND_BUFFERED) { 
        mca_pml_cm_hvy_send_request_t *sendreq;
        ompi_proc_t * ompi_proc;
        MCA_PML_CM_HVY_SEND_REQUEST_ALLOC(sendreq, comm, dst, ompi_proc);
        if (OPAL_UNLIKELY(NULL == sendreq)) return OMPI_ERR_OUT_OF_RESOURCE;
        
        MCA_PML_CM_HVY_SEND_REQUEST_INIT(sendreq,
                                         ompi_proc,
                                         comm,
                                         tag,
                                         dst, 
                                         datatype,
                                         sendmode,
                                         false,
                                         false,
                                         buf,
                                         count);
        MCA_PML_CM_HVY_SEND_REQUEST_START(sendreq, ret);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            MCA_PML_CM_HVY_SEND_REQUEST_RETURN(sendreq);
            return ret;
        }
        
        ompi_request_free( (ompi_request_t**)&sendreq );
    } else { 
        opal_convertor_t convertor;
        ompi_proc_t *ompi_proc = ompi_comm_peer_lookup(comm, dst);

        opal_convertor_copy_and_prepare_for_send(
		ompi_proc->super.proc_convertor,
                &datatype->super, count, buf, 0,
                &convertor);

        ret = OMPI_MTL_CALL(send(ompi_mtl,                             
                                 comm, 
                                 dst, 
                                 tag,  
                                 &convertor,
                                 sendmode));
    }
    
    return ret;
}
