/*
 *  *$HEADER$
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

typedef struct mca_ptl_prof mca_ptl_prof_t;
typedef struct mca_ptl_prof_module_1_0_0 mca_ptl_prof_module_1_0_0_t;

/**
 * PROF PTL module.
 */
struct mca_ptl_prof_module_1_0_0 {
   mca_ptl_base_component_t super;          /**< base PTL module */
   mca_ptl_prof_t**         prof_ptls;      /**< array of available PTLs */
   u_int32_t                prof_num_ptls;  /**< number of ptls actually used */
   u_int32_t                prof_max_ptls;  /**< maximum number of ptls - available kernel ifs */
   u_int32_t                prof_buf_size;  /**< the size of the internal buffer used to profile each PTL */
};
/**
 * Profiling module.
 */
struct mca_ptl_prof {
   mca_ptl_base_module_t super;
   u_int32_t             prof_create_id;
   u_int32_t             prof_start_send_id;
   u_int32_t             prof_start_recv_id;
   u_int32_t             prof_complete_id;
};
OBJ_CLASS_DECLARATION(mca_ptl_prof_t);

#endif  /* PTL_PROF_H_HAS_BEEN_INCLUDED */
