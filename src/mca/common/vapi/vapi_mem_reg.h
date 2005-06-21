/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#ifndef MCA_COMMON_VAPI_MEM_REG_H
#define MCA_COMMON_VAPI_MEM_REG_H

#include <vapi.h> 
#include <vapi_common.h> 


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


struct mca_bmi_base_resources_t {
  VAPI_hca_hndl_t hca;   /* the hca (nic) */ 
  VAPI_pd_hndl_t pd_tag; /* the protection domain */ 
}; 
typedef struct mca_bmi_base_resources_t mca_bmi_base_resources_t;  


struct mca_bmi_base_registration_t {
  VAPI_mr_hndl_t                  hndl;
  /* Memory region handle */
  
  VAPI_lkey_t                     l_key;
  /* Local key to registered memory, needed for
   * posting send/recv requests */

  VAPI_rkey_t                     r_key;
  /* Remote key to registered memory, need to send this
   * to remote processes for incoming RDMA ops */
};
typedef struct mca_bmi_base_registration_t mca_bmi_base_registration_t;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif









