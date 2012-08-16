/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "ompi_config.h"

#include "bcol_ptpcoll.h"
#include "bcol_ptpcoll_utils.h"

/*
 *  Return closet power of K, for the number, and the number
 */
int pow_k_calc(int k, int number, int *out_number)
{
    int power = 0;
    int n = 1;

    while (n < number) {
        n *= k;
        ++power;
    }
    
    if (n > number) {
        n /= k;
        --power;
    }
    if (NULL != out_number) {
        *out_number = n;
    }

    return power;
}

/*
 * Communicator rank to group index conversion function for K-nomial tree.
 * Complexity: (K-1) Log _base_K N
 *
 * Input:
 * my_group_index       - my process index in the group
 * comm_source          - the communicator rank of the source of data
 * radix                - radix of K-nomial tree
 * group_size           - the size of my group
 * group_array[]        - one to one map from group index to communicator rank
 *
 * Output:
 * Group index for comm_source.
 */

int get_group_index_and_distance_for_binomial(int my_group_index, int comm_source,
        int group_size, int *group_array, int *pow_distance)
{
    int group_index;
    int i;
    *pow_distance = 0;

    for (i = 1; i < group_size; i<<=1, (*pow_distance)++) {
        group_index = my_group_index ^ i;
        if (comm_source == group_array[group_index]) {
            return group_index;
        }
    }

    *pow_distance = -1;
    return -1;
}

int get_group_index_and_distance_for_k_nomial(int my_group_index, int comm_source, int radix, 
        int group_size, int *group_array, int *pow_distance)
{
    int group_index;
    int offset = 1;      /* offset equal to 1 (radix_power) */
    int radix_power = 1; /* radix power 0 */
    *pow_distance = 0;

    /* 
     *  Go trough range of possible offsets from my rank,
     *  for each offset we calculate k-nomial tree root.
     */
    while(offset < group_size) {
        /* K-nomial tree root calculation for the offset */
        if (offset % (radix * radix_power)) {
            group_index = my_group_index - offset; 
            /* wrap around if the group is negative */
            if (group_index < 0) {
                group_index += group_size;
            }   
            PTPCOLL_VERBOSE(10, ("Checking %d", group_index));
            if (comm_source == group_array[group_index]) {
                return group_index;
            }
            offset += radix_power;
        } else {
            /* we done with the section of the tree, go to next one */
            radix_power *= radix;
            (*pow_distance)++;
        }   
    }   

    /* No source was found, return -1 */
    *pow_distance = -1;
    return -1;
}

int get_group_index_for_k_nomial(int my_group_index, int comm_source, int radix, int group_size, int *group_array)
{
    int group_index;
    int radix_power = 1; /* radix power 0 */
    int offset = 1;      /* offset equal to 1 (radix_power) */

    /* 
     *  Go trough range of possible offsets from my rank,
     *  for each offset we calculate k-nomial tree root.
     */
    while(offset < group_size) {
        /* K-nomial tree root calculation for the offset */
        if (offset % (radix * radix_power)) {
            group_index = my_group_index - offset; 
            /* wrap around if the group is negative */
            if (group_index < 0) {
                group_index += group_size;
            }   
            if (comm_source == group_array[group_index]) {
                return group_index;
            }
            offset += radix_power;
        } else {
            /* we done with the section of the tree, go to next one */
            radix_power *= radix;
        }   
    }   

    /* No source was found, return -1 */
    return -1;
}
