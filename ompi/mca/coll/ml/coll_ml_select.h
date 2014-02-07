#ifndef MCA_COLL_ML_SELECT_H
#define MCA_COLL_ML_SELECT_H


#include "ompi_config.h"

#include <math.h>
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/op/op.h"
#include "ompi/mca/bcol/bcol.h"
#include "coll_ml.h"
#include "coll_ml_inlines.h"



/* Forward declaration */
struct mca_coll_ml_module_t;

int mca_select_bcol_function(mca_bcol_base_module_t *bcol_module,
                int bcoll_type,
                bcol_function_args_t *bcol_fn_arguments,
                mca_bcol_base_function_t *ml_fn_arguments );
/* 
 *  Goes through the function table and filters the collectives functions
 *  based on comm-time attributes.
 */ 
int mca_coll_ml_build_filtered_fn_table(struct mca_coll_ml_module_t *ml_module);

#endif /* MCA_COLL_ML_SELECT_H */
