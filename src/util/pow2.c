/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "util/pow2.h"


/**
 *  This routine takes in an interger, and returns the nearest
 *  power of 2 integer, same or greater than the input.
 *
 *  @param input_integer input value
 *
 *  @returnvalue power of 2 integer same or greater than the input
 *               parameter.
 */
int ompi_round_up_to_nearest_pow2(int input_integer)
{   
    int pop_count, highest_used_bit, tmp_input_integer, return_value;

    /* init counters */
    pop_count=0;
    highest_used_bit=-1;

    /* get population count, and highest non-zero bit */
    tmp_input_integer=input_integer;
    while ( tmp_input_integer > 0 ){
        pop_count+=(tmp_input_integer&1);
        highest_used_bit++;
        tmp_input_integer>>=1;
    };
    if( 1 < pop_count ) {
        /* "round" up */
        highest_used_bit++;
    }
    
    /* generate return value */
    return_value=1<<highest_used_bit;

    return return_value;

}

