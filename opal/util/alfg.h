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

struct rng_buff_t {
    unsigned int alfg[127];
    int tap1;
    int tap2;
};
typedef struct rng_buff_t rng_buff_t;


int opal_srand(rng_buff_t *buff, uint32_t seed); 

uint32_t opal_rand(rng_buff_t *buff);

#endif /* ALFG_H */
