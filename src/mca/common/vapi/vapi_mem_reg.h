#ifndef MCA_COMMON_
#include <vapi.h> 
#include <vapi_common.h> 


struct mca_common_vapi_mem_reg_t{
  VAPI_hca_hndl_t hca;   /* the hca (nic) */ 
  VAPI_pd_hndl_t pd_tag; /* the protection domain */ 
}; typedef struct mca_common_vapi_mem_reg_t mca_common_vapi_mem_reg_t;  

struct vapi_memhandle_t {
  VAPI_mr_hndl_t                  hndl;
  /* Memory region handle */
  
  VAPI_lkey_t                     l_key;
  /* Local key to registered memory, needed for
   * posting send/recv requests */

  VAPI_rkey_t                     r_key;
  /* Remote key to registered memory, need to send this
   * to remote processes for incoming RDMA ops */
};
typedef struct mca_common_vapi_memhandle_t mca_common_vapi_memhandle_t;










