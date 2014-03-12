/*
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OPAL_ALFG_H
#define OPAL_ALFG_H

#include "opal_config.h"

#include "opal_stdint.h"


struct opal_rng_buff_t {
    uint32_t alfg[127];
    int tap1;
    int tap2;
};
typedef struct opal_rng_buff_t opal_rng_buff_t;


OPAL_DECLSPEC int opal_srand(opal_rng_buff_t *buff, uint32_t seed); 

OPAL_DECLSPEC uint32_t opal_rand(opal_rng_buff_t *buff);

#endif /* OPAL_ALFG_H */
