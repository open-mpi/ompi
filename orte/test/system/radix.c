/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of MPI applications
 */

#include "orte_config.h"

#include <stdio.h>
#include <unistd.h>



#include "opal/class/opal_list.h"
#include "opal/class/opal_bitmap.h"

#include "orte/util/proc_info.h"
#include "orte/mca/routed/base/base.h"
#include "orte/runtime/runtime.h"

static int Radix;

int down_search(int me, int num_procs,
                int *num_children, opal_list_t *children, opal_bitmap_t *relatives)
{
    int i, peer, Sum, NInLevel, rc;
    orte_routed_tree_t *child;
    opal_bitmap_t *relations;
    
    /* compute how many procs are at my level */
    Sum=1;
    NInLevel=1;
    
    while ( Sum < (me+1) ) {
        NInLevel *= Radix;
        Sum += NInLevel;
    }
   /* printf("\trank %d inlevel %d\n", me, NInLevel); */
    
    /* our children start at our rank + num_in_level */
    peer = me + NInLevel;
    for (i = 0; i < Radix; i++) {
        if (peer < num_procs) {
            child = OBJ_NEW(orte_routed_tree_t);
            child->vpid = peer;
            if (NULL != children) {
               /* printf("\t\tadding child rank %d\n", peer); */
                /* this is a direct child - add it to my list */
                opal_list_append(children, &child->super);
                (*num_children)++;
                /* setup the relatives bitmap */
                opal_bitmap_init(&child->relatives, num_procs);
                /* point to the relatives */
                relations = &child->relatives;
            } else {
              /*  printf("\t\tsetting bit for rank %d\n", peer); */
                /* we are recording someone's relatives - set the bit */
                if (OPAL_SUCCESS != (rc = opal_bitmap_set_bit(relatives, peer))) {
                    printf("\t\t\tbit not set!\n");
                }
                /* point to this relations */
                relations = relatives;
            }
            /* printf("\tdownsearching peer %d\n", peer); */
            /* search for this child's relatives */
            down_search(peer, num_procs, NULL, NULL, relations);
        }
        peer += NInLevel;
    }
}

main(int argc, char **argv)
{
    opal_list_t children;
    opal_list_item_t *item;
    int num_children;
    orte_routed_tree_t *child;
    int j;
    int NProcs;
    int Level,Sum,NInLevel,Ii;
    int Parent,NInPrevLevel;
    
    
    if (3 != argc) {
        printf("usage: radix r x, where r=radix and x=number of procs\n");
        exit(1);
    }
    
    orte_init(&argc, &argv, ORTE_PROC_NON_MPI);
    
    Radix = atoi(argv[1]);
    NProcs = atoi(argv[2]);
    
    for(Ii = 0 ; Ii < NProcs ; Ii++) {
        OBJ_CONSTRUCT(&children, opal_list_t);
        num_children = 0;
        Level=0;
        Sum=1;
        NInLevel=1;
        
        while ( Sum < (Ii+1) ) {
            Level++;
            NInLevel*=Radix;
            Sum+=NInLevel;
        }
        Sum-=NInLevel;
        
        NInPrevLevel=NInLevel/Radix;
        
        if( 0 == Ii ) {
            Parent=-1;
        }  else {
            Parent=(Ii-Sum) % NInPrevLevel;
            Parent+=(Sum - NInPrevLevel);
        }
        
        fprintf(stderr," I am %d: Parent %d\n",
                Ii,Parent);

    /* compute children and relatives */
      down_search(Ii, NProcs, &num_children, &children, NULL);
        while (NULL != (item = opal_list_remove_first(&children))) {
            child = (orte_routed_tree_t*)item;
            fprintf(stderr, "\tchild %d\n", child->vpid);
            for (j=0; j < NProcs; j++) {
                if (opal_bitmap_is_set_bit(&child->relatives, j)) {
                    fprintf(stderr, "\t\trelation %d\n", j);
                }
            }
            OBJ_RELEASE(item);
        }
        OBJ_DESTRUCT(&children);
    }
    
    orte_finalize();
}
