/*
 * $HEADER$
 *   */
/**
 *  * @file
 *   */

#ifndef PTL_PROF_H_HAS_BEEN_INCLUDED
#define PTL_PROF_H_HAS_BEEN_INCLUDED

#include "event/event.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "mca/pml/base/pml_base_recvreq.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
typedef struct mca_ptl_prof mca_ptl_prof_t;
typedef struct mca_ptl_prof_module_1_0_0 mca_ptl_prof_module_1_0_0_t;

/**
 * PROF PTL module.
 */
struct mca_ptl_prof_module_1_0_0 {
   mca_ptl_base_component_t super;          /**< base PTL module */
   mca_ptl_prof_t**         prof_ptls;      /**< array of available PTLs */
   uint32_t                 prof_num_ptls;  /**< number of ptls actually used */
   uint32_t                 prof_max_ptls;  /**< maximum number of ptls - available kernel ifs */
   uint32_t                 prof_buf_size;  /**< the size of the internal buffer used to profile each PTL */
};
/**
 * Profiling module.
 */
struct mca_ptl_prof {
   mca_ptl_base_module_t super;
   uint32_t              prof_create_id;
   uint32_t              prof_start_send_id;
   uint32_t              prof_start_recv_id;
   uint32_t              prof_complete_id;
};
OBJ_CLASS_DECLARATION(mca_ptl_prof_t);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif  /* PTL_PROF_H_HAS_BEEN_INCLUDED */
