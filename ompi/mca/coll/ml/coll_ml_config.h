#ifndef COLL_ML_CONFIG_H_
#define COLL_ML_CONFIG_H_

#include "opal_config.h"
#include <stdio.h>

BEGIN_C_DECLS

#define ML_UNDEFINED -1

struct per_collective_configuration_t {
    int topology_id;
    int threshold;
    int algorithm_id;
    int fragmentation_enabled;
};
typedef struct per_collective_configuration_t per_collective_configuration_t; 

void mca_coll_ml_reset_config(per_collective_configuration_t *config);
int mca_coll_ml_config_file_init(void);

END_C_DECLS
#endif
