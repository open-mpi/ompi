#include "ompi_config.h"
#include "vprotocol_pessimist.h"

OMPI_DECLSPEC int mca_vprotocol_pessimist_start(size_t count, ompi_request_t **requests)
{ 
  int ret;
#if 0
  size_t i;


  for(i = 0; i < count; i++)
  {
    mca_pml_base_request_t *pml_request = (mca_pml_base_request_t *) requests[i];
    if(NULL == pml_request) continue;
    
     switch(pml_request->req_type)
    {
      case MCA_PML_REQUEST_RECV :
        V_OUTPUT_VERBOSE(90, "pessimist:\tstart\trecv\t%d:%llx\tfrom %d\ttag %d\tsize %d", pml_request->req_comm->c_contextid, mca_vprotocol_pessimist.rclock[pml_request->req_comm->c_contextid], pml_request->req_peer, pml_request->req_tag, pml_request->req_count);
        /* It's a persistent recv request, first, see if we have to enforce matching order */
        VPROTOCOL_PESSIMIST_MATCHING_REPLAY(pml_request->req_peer, pml_request->req_comm);
        break;
        
      case MCA_PML_REQUEST_SEND :
        V_OUTPUT_VERBOSE(90, "pessimist:\tstart\tsend\t%d:%llx\tto %d\ttag %d\tsize %d", pml_request->req_comm->c_contextid, mca_vprotocol_pessimist.rclock[pml_request->req_comm->c_contextid], pml_request->req_peer, pml_request->req_tag, pml_request->req_count);
        /* It's a persistent send request, first, check if we are waiting ack 
         * for some older events */ 
        VPROTOCOL_PESSIMIST_POSTPONE(&request[i]);
        break;
        
      default:
         V_OUTPUT_VERBOSE(90, "pessimist:\tstart\trequest\t%d:%ld\tfrom/to %d\ttag %d\tsize %d", pml_request->req_comm->c_contextid, mca_vprotocol_pessimist.rclock[pml_request->req_comm->c_contextid], pml_request->req_peer, pml_request->req_tag, pml_request->req_count);
        return OMPI_ERR_REQUEST;
    }
  }
#endif
  ret = mca_pml_v.host_pml.pml_start(count, requests);
  
  /* restore requests status */
  return ret;
}
