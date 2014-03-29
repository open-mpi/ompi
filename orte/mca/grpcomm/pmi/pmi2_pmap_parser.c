/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */
#ifdef STANDALONE_TEST
#define WANT_PMI2_SUPPORT 1
#else
#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include "grpcomm_pmi.h"
#endif

/**
 pmi2 process mapping is returned as a 
 comma separated list of tuples:
ex: (vector,(0,4,4),(0,4,1))
slurm cyclic distro of 4 ranks over 2 nodes:
(vector,(0,2,1),(0,2,1))
slurm block distro of 4 ranks over 2 nodes:
(vector,(0,2,2))


 Format of each tuple is (base, H, L), where
 H is number of nodes spawned by tuple,
 L is number of ranks per node,
 base is offset from node 0. 

 Tuple can be visualized as a rectangle on two
 dimensional (Hosts, Local Ranks) plane:

           ------------------------------------ Hosts ->
           |              H
           |           +--------+
           |<- base -->|        |
           |           |        | L
           |           +--------+
        Local Ranks
           V 

Note that ranks increase by column. Tuple (0,2,3) looks like:
0 3
1 4
2 5

*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
static int find_my_node(char *map, int me)
{
    int abs_rank;
    int base, H, L;
    char *p;

    p = map;
    abs_rank = 0;
    while (NULL != (p = strstr(p+1, ",("))) {
        if (3 != sscanf(p, ",(%d,%d,%d)", &base, &H, &L)) {
            return -1;
        }
        if (me >= abs_rank && me < abs_rank + H*L) {
            /* found my rectangle, compute node */
            return base + (me - abs_rank)/L;
        }
        abs_rank += H*L;
    }
    return -1;
}

static int *find_lrs(char *map, int my_node, int *nlrs)
{
    int abs_rank;
    int base, H, L;
    char *p;
    int *lrs;
    int max_lr;
    int i;

    p = map;
    abs_rank = 0;
    *nlrs = 0;
    max_lr = 16;
    lrs = malloc(max_lr * sizeof(int));
    while (NULL != (p = strstr(p+1, ",("))) {
        if (3 != sscanf(p, ",(%d,%d,%d)", &base, &H, &L)) {
            free(lrs);
            return NULL;
        }
        if (base <= my_node && my_node < base + H) {
            if (*nlrs + L >= max_lr) {
                lrs = realloc(lrs, (max_lr + L) * sizeof(int));
                if (NULL == lrs) {
                    *nlrs = 0;
                    free(lrs);
                    return NULL;
                }
                max_lr += L;
            }
            /* skip (my_node - base) columns of L elems,
             * numbers in my column are local to me
             */
            for (i = 0; i < L; i++) {
                lrs[*nlrs] = (my_node - base) * L + i + abs_rank;
                (*nlrs) ++;
            }
        }
        abs_rank += H*L;
    }

    if (0 == *nlrs) {
        free(lrs);
        lrs = 0;
    }
    return lrs;
}

/**
 * @param pmap process map as returned by PMI_process_mapping
 *             attribute
 * @param my_rank 
 * @param node set to my node id
 * @param nlrs set to the number of local ranks returned
 *
 * @return array that contains ranks local to my_rank or NULL
 * on failure. Array must be freed by the caller. 
 */ 
int *orte_grpcomm_pmi2_parse_pmap(char *pmap, int my_rank,
                                  int *node, int *nlrs)
{
    char *p;

    p = strstr(pmap, "(vector");
    if (NULL == p) {
        return NULL;
    }

    *node = find_my_node(p, my_rank);
    if (0 > *node) {
       return NULL;
    }

    return find_lrs(p, *node, nlrs);
}


#ifdef STANDALONE_TEST
#include <assert.h>
static void dump_lrs(int *lrs, int me, int node, int n)
{
    int i;

    printf("Total %d ranks/node, node %d me %d\n", n, node, me);
    for (i = 0; i < n; i++) {
        printf("%d ", lrs[i]);
    }
    printf("\n");
    free(lrs);
}

int main(int argc, char **argv)
{
    int me, n, node;
    int *lrs;
    char *pmap;
    int a1[] = {0, 1};
    int a2[] = {2, 3};
    int a3[] = {0, 2};
    int a4[] = {1, 3};
    int a5[] = {0,1,3,2,16,17};
    int a6[] = {8,9,10,11,19};


    if (argc == 3) {
        me = atoi(argv[1]);
        lrs = orte_grpcomm_pmi2_parse_pmap(argv[2], me, &node, &n);
        if (NULL == lrs) {
            printf("can not parse pmap\n");
            exit(1);
        }
        dump_lrs(lrs, me, node, n);
        exit(0);
    }
    /* built in cases */

    pmap = "(vector,(0,2,2))";
    me = 1;
    lrs = orte_grpcomm_pmi2_parse_pmap(pmap, me, &node, &n);
    assert(lrs);
    assert(n == 2);
    assert(memcmp(lrs, a1, 2) == 0);
    free(lrs);


    pmap = "(vector,(0,2,2))";
    me = 2;
    lrs = orte_grpcomm_pmi2_parse_pmap(pmap, me, &node, &n);
    assert(lrs);
    assert(n == 2);
    assert(memcmp(lrs, a2, 2) == 0);
    free(lrs);

            
    /* cyclic distro which skips node 0 */
    pmap = "(vector,(1,2,1),(1,2,1))";
    me = 0;
    lrs = orte_grpcomm_pmi2_parse_pmap(pmap, me, &node, &n);
    assert(lrs);
    assert(n == 2);
    assert(memcmp(lrs, a3, n) == 0);
    free(lrs);

    pmap = "(vector,(1,2,1),(1,2,1))";
    me = 3;
    lrs = orte_grpcomm_pmi2_parse_pmap(pmap, me, &node, &n);
    assert(lrs);
    assert(n == 2);
    assert(memcmp(lrs, a4, n) == 0);
    free(lrs);

    pmap = "(vector,(0,4,4),(0,1,2),(1,3,1))";
    me = 3;
    lrs = orte_grpcomm_pmi2_parse_pmap(pmap, me, &node, &n);
    assert(lrs);
    assert(n == 6);
    assert(memcmp(lrs, a5, n) == 0);
    free(lrs);

    pmap = "(vector,(0,4,4),(0,1,2),(1,3,1))";
    me = 10;
    lrs = orte_grpcomm_pmi2_parse_pmap(pmap, me, &node, &n);
    assert(lrs);
    assert(n == 5);
    assert(memcmp(lrs, a6, n) == 0);
    free(lrs);
    return 0;
}
#endif
