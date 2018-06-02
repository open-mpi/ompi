/*
 * Copyright © 2016 Inria.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 * See COPYING in top-level directory.
 *
 * $HEADER$
 */

#define _GNU_SOURCE         /* See feature_test_macros(7) */
#include <stdio.h>
#include <stdlib.h>

#include <netloc.h>
#include <private/netloc.h>

static int compareint(void const *a, void const *b)
{
   const int *int_a = (const int *)a;
   const int *int_b = (const int *)b;
   return *int_a-*int_b;
}


int main(int argc, char **argv)
{
    int ret;
    /* First we need to get the topology of the whole machine */
    netloc_arch_t arch;
    ret = netloc_arch_build(&arch, 0);
    if( NETLOC_SUCCESS != ret ) {
        return ret;
    }

    /* Set the current nodes and slots in the arch */
    ret = netloc_arch_set_current_resources(&arch);
    if( NETLOC_SUCCESS != ret ) {
        return ret;
    }
    int num_nodes = arch.num_current_hosts;

    /* Order the idx_list to have the nodes sorted */
    qsort(arch.current_hosts, num_nodes, sizeof(*arch.current_hosts), compareint);

    /* Show the list */
    for (int n = 0; n < num_nodes; n++) {
        netloc_arch_node_t *arch_node = arch.node_slot_by_idx[arch.current_hosts[n]].node;
        qsort(arch_node->current_slots, arch_node->num_current_slots,
                sizeof(*arch_node->current_slots), compareint);
        
        for (int s = 0; s < arch_node->num_current_slots; s++) {
            int slot_idx = arch_node->current_slots[s];
            int slot = arch_node->slot_os_idx[slot_idx];
            printf("%s %d", arch_node->name, slot);
        }
    }

    return NETLOC_SUCCESS;
}
