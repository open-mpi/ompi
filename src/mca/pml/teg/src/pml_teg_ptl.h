#ifndef _MCA_PML_BASE_PTL_
#define _MCA_PML_BASE_PTL_

#include "mca/ptl/ptl.h"

struct mca_pml_base_ptl_t {
    ompi_list_t       ptl_cache;       /**< cache of send requests */
    size_t            ptl_cache_size;  /**< maximum size of cache */
    ompi_mutex_t      ptl_cache_lock;  /**< lock for queue access */
    struct mca_ptl_t* ptl;             /**< back pointer to ptl */
};
typedef struct mca_pml_base_ptl_t mca_pml_base_ptl_t;

OBJ_CLASS_DECLARATION(mca_pml_base_ptl_t);


#endif

