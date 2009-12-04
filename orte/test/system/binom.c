/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of MPI applications
 */

#include "orte_config.h"

#include <stdio.h>
#include <unistd.h>



#include "opal/util/bit_ops.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_bitmap.h"

#include "orte/util/proc_info.h"
#include "orte/runtime/runtime.h"

typedef struct {
    opal_list_item_t super;
    orte_vpid_t vpid;
    opal_bitmap_t relatives;
} orte_routed_tree_t;

static void construct(orte_routed_tree_t *rt)
{
    rt->vpid = ORTE_VPID_INVALID;
    OBJ_CONSTRUCT(&rt->relatives, opal_bitmap_t);
}
static destruct(orte_routed_tree_t *rt)
{
    OBJ_DESTRUCT(&rt->relatives);
}
OBJ_CLASS_INSTANCE(orte_routed_tree_t, opal_list_item_t,
                   construct, destruct);


int down_search(int rank, int parent, int me, int num_procs,
                int *num_children, opal_list_t *children, opal_bitmap_t *relatives)
{
    int i, bitmap, peer, hibit, mask, found;
    orte_routed_tree_t *child;
    opal_bitmap_t *relations;
    
    /* is this me? */
    if (me == rank) {
        bitmap = opal_cube_dim(num_procs);
        
        hibit = opal_hibit(rank, bitmap);
        --bitmap;
        
        for (i = hibit + 1, mask = 1 << i; i <= bitmap; ++i, mask <<= 1) {
            peer = rank | mask;
            if (peer < num_procs) {
                child = OBJ_NEW(orte_routed_tree_t);
                child->vpid = peer;
                if (NULL != children) {
                    /* this is a direct child - add it to my list */
                    opal_list_append(children, &child->super);
                    (*num_children)++;
                    /* setup the relatives bitmap */
                    opal_bitmap_init(&child->relatives, num_procs);
                    /* point to the relatives */
                    relations = &child->relatives;
                } else {
                    /* we are recording someone's relatives - set the bit */
                    opal_bitmap_set_bit(relatives, peer);
                    /* point to this relations */
                    relations = relatives;
                }
                /* search for this child's relatives */
                down_search(0, 0, peer, num_procs, NULL, NULL, relations);
            }
        }
        return parent;
    }
    
    /* find the children of this rank */
    bitmap = opal_cube_dim(num_procs);
    
    hibit = opal_hibit(rank, bitmap);
    --bitmap;
    
    for (i = hibit + 1, mask = 1 << i; i <= bitmap; ++i, mask <<= 1) {
        peer = rank | mask;
        if (peer < num_procs) {
            /* execute compute on this child */
            if (0 <= (found = down_search(peer, rank, me, num_procs, num_children, children, relatives))) {
                return found;
            }
        }
    }
    return -1;
}

int main(int argc, char* argv[])
{
    int i, j;
    int found;
    opal_list_t children;
    opal_list_item_t *item;
    int num_children;
    int num_procs;
    orte_routed_tree_t *child;
    opal_bitmap_t *relations;
    
    if (2 != argc) {
        printf("usage: binom x, where x=number of procs\n");
        exit(1);
    }
    
    orte_init(&argc, &argv, ORTE_PROC_TOOL);
        
    num_procs = atoi(argv[1]);
    
    for (i=0; i < num_procs; i++) {
        OBJ_CONSTRUCT(&children, opal_list_t);
        num_children = 0;
        printf("i am %d:", i);
        found = down_search(0, 0, i, num_procs, &num_children, &children, NULL);
        printf("\tparent %d num_children %d\n", found, num_children);
        while (NULL != (item = opal_list_remove_first(&children))) {
            child = (orte_routed_tree_t*)item;
            printf("\tchild %d\n", child->vpid);
            for (j=0; j < num_procs; j++) {
                if (opal_bitmap_is_set_bit(&child->relatives, j)) {
                    printf("\t\trelation %d\n", j);
                }
            }
            OBJ_RELEASE(item);
        }
        OBJ_DESTRUCT(&children);
    }
    
    orte_finalize();
}
