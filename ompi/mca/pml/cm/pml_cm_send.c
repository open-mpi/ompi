/*
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
    int ret;
    mca_pml_cm_hvy_send_request_t *sendreq;
    ompi_proc_t* ompi_proc;
    
    MCA_PML_CM_HVY_SEND_REQUEST_ALLOC(sendreq, comm, dst, ompi_proc, ret);
    if (NULL == sendreq || OMPI_SUCCESS != ret) return ret;
    
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
        
        MCA_PML_CM_HVY_SEND_REQUEST_ALLOC(sendreq, comm, dst, ompi_proc, ret);
        if (NULL == sendreq || OMPI_SUCCESS != ret) return ret;
        
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
        
        if (OMPI_SUCCESS == ret) *request = (ompi_request_t*) sendreq;
       
        
    } else { 
        mca_pml_cm_thin_send_request_t* sendreq;
        ompi_proc_t* ompi_proc;
        MCA_PML_CM_THIN_SEND_REQUEST_ALLOC(sendreq, comm, dst, ompi_proc, ret);
        if (NULL == sendreq || OMPI_SUCCESS != ret) return ret;
        
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
        
        if (OMPI_SUCCESS == ret) *request = (ompi_request_t*) sendreq;
        
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
    int ret;
    if(sendmode == MCA_PML_BASE_SEND_BUFFERED) { 
        mca_pml_cm_hvy_send_request_t *sendreq;
        ompi_proc_t * ompi_proc;
        MCA_PML_CM_HVY_SEND_REQUEST_ALLOC(sendreq, comm, dst, ompi_proc, ret);
        if (NULL == sendreq || OMPI_SUCCESS != ret) return ret;
        
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
        if (OMPI_SUCCESS != ret) {
            MCA_PML_CM_HVY_SEND_REQUEST_RETURN(sendreq);
            return ret;
        }
        
        ompi_request_free( (ompi_request_t**)&sendreq );

        return ret;
    } else { 
        mca_pml_cm_thin_send_request_t *sendreq;
        ompi_proc_t * ompi_proc;
        MCA_PML_CM_THIN_SEND_REQUEST_ALLOC(sendreq, comm, dst, ompi_proc, ret);
        if (NULL == sendreq || OMPI_SUCCESS != ret) return ret;
        
        MCA_PML_CM_THIN_SEND_REQUEST_INIT(sendreq,
                                          ompi_proc,
                                          comm,
                                          tag,
                                          dst, 
                                          datatype,
                                          sendmode,
                                          buf,
                                          count);
        if (NULL == ompi_mtl->mtl_send) {
            MCA_PML_CM_THIN_SEND_REQUEST_START(sendreq,
                                               comm,
                                               tag, 
                                               dst,
                                               sendmode,
                                               false,
                                               ret);
            if (OMPI_SUCCESS != ret) {
                MCA_PML_CM_THIN_SEND_REQUEST_RETURN(sendreq);
                return ret;
            }
            
            if (sendreq->req_send.req_base.req_ompi.req_complete == false) {
                /* give up and sleep until completion */
                if (opal_using_threads()) {
                    opal_mutex_lock(&ompi_request_lock);
                    ompi_request_waiting++;
                    while (sendreq->req_send.req_base.req_ompi.req_complete == false)
                        opal_condition_wait(&ompi_request_cond, &ompi_request_lock);
                    ompi_request_waiting--;
                    opal_mutex_unlock(&ompi_request_lock);
                } else {
                    ompi_request_waiting++;
                    while (sendreq->req_send.req_base.req_ompi.req_complete == false)
                        opal_condition_wait(&ompi_request_cond, &ompi_request_lock);
                    ompi_request_waiting--;
                }
            }
        } else {
            MCA_PML_CM_SEND_REQUEST_START_SETUP((&sendreq->req_send));
            if (OMPI_SUCCESS != ret) {
                MCA_PML_CM_THIN_SEND_REQUEST_RETURN(sendreq);
                return ret;
            }
            
            ret = OMPI_MTL_CALL(send(ompi_mtl,                             
                                     comm, 
                                     dst, 
                                     tag,  
                                     &sendreq->req_send.req_base.req_convertor,
                                     sendmode));
            MCA_PML_CM_THIN_SEND_REQUEST_PML_COMPLETE(sendreq);
        }
        
        ompi_request_free( (ompi_request_t**)&sendreq );
        
        return ret;
    }
    return OMPI_ERROR;
}
