/*
 * $HEADER
 */

#ifndef MCA_IO_H
#define MCA_IO_H

#include "mca/mca.h"

/*
 * Macro for use in modules that are of type pml v1.0.0
 */
#define MCA_IO_BASE_VERSION_1_0_0 \
  MCA_BASE_VERSION_1_0_0, \
  "io", 1, 0, 0

#define MCA_IO_BASE_VERSION_2_0_0 \
  MCA_BASE_VERSION_1_0_0, \
  "io", 2, 0, 0


struct mca_io_1_0_0_t;
typedef struct mca_io_1_0_0_t * (*mca_io_base_module_init_fn_t)(
    int* priority, int* min_thread, int* max_thread);



/* IO module version and interface functions. */
struct mca_io_base_module_1_0_0_t {
   mca_base_module_t version;
   mca_base_module_data_1_0_0_t data;
   mca_io_base_module_init_fn_t init;
};
typedef struct mca_io_base_module_1_0_0_t mca_io_base_module_1_0_0_t;



struct mca_io_1_0_0_t {
};
typedef struct mca_io_1_0_0_t mca_io_1_0_0_t;







#endif /* MCA_IO_H */




