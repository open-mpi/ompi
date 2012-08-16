/*
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_COMMON_NETPATTERNS_MACROS_H
#define OMPI_COMMON_NETPATTERNS_MACROS_H

#include "ompi_config.h"

BEGIN_C_DECLS

/* function to decompose an interger into it's representation in base K */
/*
 * input_value - value to translate (input)
 * base - base of representation (input)
 * highest_power - the highest power that may have a non-zero entry (input)
 *    the assumption is that this will be called in the critical path
 *    to compute communication patterns, so will precompute such values
 *    and pass the in.
 * base_to_power_i - array of base to ith power (input)
 * cum_base_to_power_i - array of cummulative base to ith power (input)
 * base_k_rep - representation in base "base".  Space is pre-allocated. (out)
 */
static inline  __opal_attribute_always_inline__ void
common_netpatterns_obtain_rep_base_k (int input_value, int base,
        int highest_power, int *base_to_power_i,
        int *base_k_rep
        )
{
    /* local variables */
    int lvl, work_value;

    /* loop over all possible powers */
    work_value=input_value;
    for( lvl=highest_power ; lvl >= 0 ; lvl-- ) {
        /* still need to compute the actual coefficient */
        base_k_rep[lvl]=work_value/base_to_power_i[lvl];
        work_value-=(base_k_rep[lvl]*base_to_power_i[lvl]);

    }

}

END_C_DECLS

#endif /* OMPI_COMMON_NETPATTERNS_MACROS_H */
