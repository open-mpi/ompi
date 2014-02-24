/*
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef ALFG_H
#define ALFG_H


#include "opal_config.h"

struct opal_rng_buff_t {
    uint32_t alfg[127];
    int tap1;
    int tap2;
};
typedef struct opal_rng_buff_t opal_rng_buff_t;


int opal_srand(opal_rng_buff_t *buff, uint32_t seed); 

uint32_t opal_rand(opal_rng_buff_t *buff);

#endif /* ALFG_H */
