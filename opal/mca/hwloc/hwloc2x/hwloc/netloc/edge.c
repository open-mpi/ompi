/*
 * Copyright © 2013-2014 University of Wisconsin-La Crosse.
 *                         All rights reserved.
 * Copyright © 2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright © 2015-2016 Inria.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 * See COPYING in top-level directory.
 *
 * $HEADER$
 */

#define _GNU_SOURCE         /* See feature_test_macros(7) */
#include <stdlib.h>

#include <private/autogen/config.h>
#include <private/netloc.h>

static int cur_uid = 0;

netloc_edge_t * netloc_edge_construct(void)
{

    netloc_edge_t *edge = NULL;

    edge = (netloc_edge_t*)malloc(sizeof(netloc_edge_t));
    if( NULL == edge ) {
        return NULL;
    }

    edge->id = cur_uid;
    cur_uid++;

    edge->dest = NULL;
    edge->node = NULL;

    utarray_new(edge->physical_links, &ut_ptr_icd);

    edge->total_gbits = 0;

    utarray_new(edge->partitions, &ut_int_icd);

    utarray_new(edge->subnode_edges, &ut_ptr_icd);

    edge->userdata = NULL;

    return edge;
}

char * netloc_edge_pretty_print(netloc_edge_t* edge)
{
    // TODO
    return "TODO";
}

int netloc_edge_destruct(netloc_edge_t * edge)
{
    utarray_free(edge->physical_links);
    utarray_free(edge->partitions);

    for (unsigned int e = 0; e < netloc_edge_get_num_subedges(edge); e++) {
        netloc_edge_t *subedge;
        subedge = netloc_edge_get_subedge(edge, e);
        netloc_edge_destruct(subedge);
    }
    utarray_free(edge->subnode_edges);
    free(edge);
    return NETLOC_SUCCESS;
}

void netloc_edge_reset_uid(void)
{
    cur_uid = 0;
}

int netloc_edge_is_in_partition(netloc_edge_t *edge, int partition)
{
    for (unsigned int i = 0; i < netloc_get_num_partitions(edge); i++) {
        if (netloc_get_partition(edge, i) == partition)
            return 1;
    }
    return NETLOC_SUCCESS;
}


