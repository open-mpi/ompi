/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "bcol_basesmuma_utils.h"

/*
 *  Return closet power of K that is either greater than
 *  or equal to the group size. 
 */
int pow_sm_k(int k, int number, int *pow_k)
{
    int power = 0;
    int n = 1;

    if( 2 == k){
        while(n <= number){
            power++;
            n <<= 1;
        }
        *pow_k = n >> 1;

    } else {
        while (n <= number) {
            n *= k;
            power++;
        }
        *pow_k = n/k;
    }

    
    return (power-1);
}



int get_k_nomial_src_list(int group_size, 
                          int radix, int my_index, 
                          int *src_list) { 

    /* local variables */
    int radix_power;
    int offset;
    int kount = 0;
    int src_temp;
    
    radix_power = 1;
    offset = 1;
    while(offset < group_size) {
        if( offset % (radix * radix_power) ) {
            src_temp = my_index - offset;
            /* wrap around */
            if ( src_temp < 0 ) {
                src_temp += group_size;
            }
            /* don't probe ghost nodes */
            if( src_temp < group_size ) {
                src_list[kount] = src_temp;
                kount++;
            }
            offset+=radix_power;
        } else {

            radix_power *= radix;
        }

    }
    /* return the actual number of nodes to poll on */
    return kount;
}

int get_k_nomial_dst_size(int group_size, int radix, int my_index)
{
	int dst_count = 0;
	int radix_mask;
	int k;
    radix_mask = 1;
    while (radix_mask < group_size) {
        if (0 != my_index % (radix * radix_mask)) {
            /* I found my level in tree */
            break;
        }
        radix_mask *= radix;
    }
	radix_mask /= radix;

	while(radix_mask > 0) {                                                                
        /* For each level of tree, do sends */                                             
        for (k = 1;                                                                        
                k < radix && my_index + radix_mask * k < group_size;              
                ++k) {                                                                     
            dst_count +=  1 ;                                                                
        }                                                                            
        radix_mask /= radix;                                                               
    }                                                                                

	return dst_count;
}
