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

#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/runtime.h"

int down_search(int rank, int parent, int me, int num_procs,
                int *num_children, opal_list_t *children)
{
    int i, bitmap, peer, hibit, mask, found;
    orte_namelist_t *child;
    
    /* is this me? */
    if (me == rank) {
        bitmap = opal_cube_dim(num_procs);
        
        hibit = opal_hibit(rank, bitmap);
        --bitmap;
        
        for (i = hibit + 1, mask = 1 << i; i <= bitmap; ++i, mask <<= 1) {
            peer = rank | mask;
            if (peer < num_procs) {
                if (NULL != children) {
                    child = OBJ_NEW(orte_namelist_t);
                    child->name.jobid = ORTE_PROC_MY_NAME->jobid;
                    child->name.vpid = peer;
                    opal_list_append(children, &child->item);
                }
                (*num_children)++;
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
            if (0 <= (found = down_search(peer, rank, me, num_procs, num_children, children))) {
                return found;
            }
        }
    }
    return -1;
}

#if 0
int down_search(int rank, int parent, int me)
{
    int i, bitmap, peer, hibit, mask, found;
    
    /* is this me? */
    if (me == rank) {
        bitmap = opal_cube_dim(num_procs);
        
        hibit = opal_hibit(rank, bitmap);
        --bitmap;
        
        printf("\tfound parent %d\n", parent);
        for (i = hibit + 1, mask = 1 << i; i <= bitmap; ++i, mask <<= 1) {
            peer = rank | mask;
            if (peer < num_procs) {
                printf("\tchild: %d\n", peer);
                num_children++;
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
            if (0 <= (found = down_search(peer, rank, me))) {
                return found;
            }
        }
    }
    return -1;
}
#endif

int main(int argc, char* argv[])
{
    int i;
    int found;
    opal_list_t children;
    opal_list_item_t *item;
    int num_children;
    int num_procs;
    orte_namelist_t *child;
    
    orte_init(ORTE_TOOL);
        
    num_procs = 32;
    
    for (i=0; i < num_procs; i++) {
        OBJ_CONSTRUCT(&children, opal_list_t);
        num_children = 0;
        printf("i am %d:", i);
        found = down_search(0, 0, i, num_procs, &num_children, &children);
        printf("\tparent %d num_children %d\n", found, num_children);
        while (NULL != (item = opal_list_remove_first(&children))) {
            child = (orte_namelist_t*)item;
            printf("\t\tchild %d\n", child->name.vpid);
            OBJ_RELEASE(item);
        }
        OBJ_DESTRUCT(&children);
    }
    
    orte_finalize();
}
