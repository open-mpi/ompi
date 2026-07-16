/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2014 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Unit test for opal_graph_t and its full public API.
 *
 * Graph under test (directed, weighted):
 *
 *   A --(1)--> B --(2)--> C --(1)--> D --(3)--> E
 *   A --(4)--> C          B --(5)--> D
 *
 * order (vertices) = 5, size (edges) = 6
 *
 * Hand-computed Dijkstra shortest paths FROM A:
 *   dist(A,B) = 1  (direct)
 *   dist(A,C) = 3  (A->B->C; direct A->C costs 4)
 *   dist(A,D) = 4  (A->B->C->D)
 *   dist(A,E) = 7  (A->B->C->D->E)
 *   dist(E,A) = INFINITY  (E has no out-edges)
 *   adjacent(A,C) = 4  (direct edge weight, != spf 3)
 *
 * NOTE: -DNDEBUG is set; assert() is a no-op. All verification uses
 * test_verify().  Do NOT use assert().
 */

#include "opal_config.h"

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "support.h"

#include "opal/class/opal_graph.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/class/opal_value_array.h"
#include "opal/constants.h"
#include "opal/runtime/opal.h"

/* --------------------------------------------------------------------------
 * Vertex data: each vertex carries a heap-allocated int label (A=0..E=4).
 * The hooks let duplicate() / find_vertex() / print work.
 * -------------------------------------------------------------------------- */

static void my_copy_vertex_data(void **dst, void *src)
{
    if (NULL == *dst) {
        *dst = malloc(sizeof(int));
    }
    *((int *) (*dst)) = *((int *) src);
}

static void my_free_vertex_data(void *data)
{
    free(data);
}

static void *my_alloc_vertex_data(void)
{
    return malloc(sizeof(int));
}

static int my_compare_vertex_data(void *data1, void *data2)
{
    int v1 = *((int *) data1);
    int v2 = *((int *) data2);
    if (v1 > v2) {
        return 1;
    }
    if (v1 < v2) {
        return -1;
    }
    return 0;
}

/* print_vertex MUST return malloc'd memory; opal_graph_print calls free(). */
static char *my_print_vertex(void *data)
{
    char *buf = malloc(8);
    if (NULL != buf) {
        snprintf(buf, 8, "%c", 'A' + *((int *) data));
    }
    return buf;
}

/* Helper: allocate and populate a vertex with label idx (0=A..4=E). */
static opal_graph_vertex_t *make_vertex(int idx)
{
    opal_graph_vertex_t *v = OBJ_NEW(opal_graph_vertex_t);
    int *data = (int *) my_alloc_vertex_data();
    *data = idx;
    v->vertex_data = data;
    v->alloc_vertex_data = my_alloc_vertex_data;
    v->copy_vertex_data = my_copy_vertex_data;
    v->free_vertex_data = my_free_vertex_data;
    v->compare_vertex = my_compare_vertex_data;
    v->print_vertex = my_print_vertex;
    return v;
}

/* Helper: build an edge on the heap (weight w, start -> end). */
static opal_graph_edge_t *make_edge(opal_graph_vertex_t *start,
                                    opal_graph_vertex_t *end,
                                    uint32_t w)
{
    opal_graph_edge_t *e = OBJ_NEW(opal_graph_edge_t);
    e->start = start;
    e->end = end;
    e->weight = w;
    return e;
}

/*
 * Build the canonical test graph into *g.
 * Vertices stored in vA..vE (all OBJ_NEW, owned by the graph).
 * Edges: A->B:1, A->C:4, B->C:2, B->D:5, C->D:1, D->E:3
 *        (all OBJ_NEW, owned by the graph after successful add_edge).
 */
static void build_graph(opal_graph_t **g,
                        opal_graph_vertex_t **vA, opal_graph_vertex_t **vB,
                        opal_graph_vertex_t **vC, opal_graph_vertex_t **vD,
                        opal_graph_vertex_t **vE)
{
    *g = OBJ_NEW(opal_graph_t);
    *vA = make_vertex(0);  /* A */
    *vB = make_vertex(1);  /* B */
    *vC = make_vertex(2);  /* C */
    *vD = make_vertex(3);  /* D */
    *vE = make_vertex(4);  /* E */

    opal_graph_add_vertex(*g, *vA);
    opal_graph_add_vertex(*g, *vB);
    opal_graph_add_vertex(*g, *vC);
    opal_graph_add_vertex(*g, *vD);
    opal_graph_add_vertex(*g, *vE);

    opal_graph_add_edge(*g, make_edge(*vA, *vB, 1));
    opal_graph_add_edge(*g, make_edge(*vA, *vC, 4));
    opal_graph_add_edge(*g, make_edge(*vB, *vC, 2));
    opal_graph_add_edge(*g, make_edge(*vB, *vD, 5));
    opal_graph_add_edge(*g, make_edge(*vC, *vD, 1));
    opal_graph_add_edge(*g, make_edge(*vD, *vE, 3));
}

/*
 * Destroy a graph and release the vertices that were OBJ_NEW'd.
 * After OBJ_RELEASE(graph) the adjacency structure and edges are gone,
 * but the vertices themselves are NOT freed by the graph — release them.
 */
static void destroy_graph(opal_graph_t *g,
                           opal_graph_vertex_t *vA, opal_graph_vertex_t *vB,
                           opal_graph_vertex_t *vC, opal_graph_vertex_t *vD,
                           opal_graph_vertex_t *vE)
{
    OBJ_RELEASE(g);
    OBJ_RELEASE(vA);
    OBJ_RELEASE(vB);
    OBJ_RELEASE(vC);
    OBJ_RELEASE(vD);
    OBJ_RELEASE(vE);
}

/* --------------------------------------------------------------------------
 * Test sections
 * -------------------------------------------------------------------------- */

static void test_order_and_size(void)
{
    opal_graph_t *g;
    opal_graph_vertex_t *vA, *vB, *vC, *vD, *vE;

    build_graph(&g, &vA, &vB, &vC, &vD, &vE);

    test_verify("order == 5", 5 == opal_graph_get_order(g));
    test_verify("size == 6",  6 == opal_graph_get_size(g));

    /* Adding the same vertex twice must be a no-op */
    opal_graph_add_vertex(g, vA);
    test_verify("add duplicate vertex -> order still 5", 5 == opal_graph_get_order(g));

    destroy_graph(g, vA, vB, vC, vD, vE);
}

static void test_add_edge_error(void)
{
    /* add_edge returns OPAL_ERROR when a vertex is not in the graph */
    opal_graph_t *g;
    opal_graph_vertex_t *vA, *vB, *vC, *vD, *vE;
    opal_graph_vertex_t *orphan;
    opal_graph_edge_t *bad_edge;
    int rc;

    build_graph(&g, &vA, &vB, &vC, &vD, &vE);

    orphan = make_vertex(9); /* not added to any graph */
    bad_edge = make_edge(vA, orphan, 99);
    rc = opal_graph_add_edge(g, bad_edge);
    test_verify("add_edge with unknown end vertex returns OPAL_ERROR",
                OPAL_ERROR == rc);
    /* bad_edge was not adopted by the graph; release it ourselves. */
    OBJ_RELEASE(bad_edge);
    OBJ_RELEASE(orphan);

    destroy_graph(g, vA, vB, vC, vD, vE);
}

static void test_adjacent(void)
{
    opal_graph_t *g;
    opal_graph_vertex_t *vA, *vB, *vC, *vD, *vE;
    uint32_t w;

    build_graph(&g, &vA, &vB, &vC, &vD, &vE);

    /* Direct edges */
    w = opal_graph_adjacent(g, vA, vB);
    test_verify("adjacent(A,B) == 1", 1 == w);

    w = opal_graph_adjacent(g, vA, vC);
    test_verify("adjacent(A,C) == 4 (direct edge, not shortest path)", 4 == w);

    w = opal_graph_adjacent(g, vC, vD);
    test_verify("adjacent(C,D) == 1", 1 == w);

    w = opal_graph_adjacent(g, vD, vE);
    test_verify("adjacent(D,E) == 3", 3 == w);

    /* No direct edge from A to D */
    w = opal_graph_adjacent(g, vA, vD);
    test_verify("adjacent(A,D) == INFINITY (no direct edge)",
                DISTANCE_INFINITY == w);

    /* No out-edges from E */
    w = opal_graph_adjacent(g, vE, vA);
    test_verify("adjacent(E,A) == INFINITY", DISTANCE_INFINITY == w);

    /* Self-loop: adjacent(v,v) == 0 per the source */
    w = opal_graph_adjacent(g, vA, vA);
    test_verify("adjacent(A,A) == 0", 0 == w);

    destroy_graph(g, vA, vB, vC, vD, vE);
}

static void test_find_vertex(void)
{
    opal_graph_t *g;
    opal_graph_vertex_t *vA, *vB, *vC, *vD, *vE;
    opal_graph_vertex_t *found;
    int key;

    build_graph(&g, &vA, &vB, &vC, &vD, &vE);

    key = 0; /* A */
    found = opal_graph_find_vertex(g, &key);
    test_verify("find_vertex A by data returns vA", vA == found);

    key = 4; /* E */
    found = opal_graph_find_vertex(g, &key);
    test_verify("find_vertex E by data returns vE", vE == found);

    key = 99; /* not in graph */
    found = opal_graph_find_vertex(g, &key);
    test_verify("find_vertex unknown returns NULL", NULL == found);

    destroy_graph(g, vA, vB, vC, vD, vE);
}

static void test_get_graph_vertices(void)
{
    opal_graph_t *g;
    opal_graph_vertex_t *vA, *vB, *vC, *vD, *vE;
    opal_pointer_array_t *arr;
    int n;

    build_graph(&g, &vA, &vB, &vC, &vD, &vE);

    arr = OBJ_NEW(opal_pointer_array_t);
    opal_pointer_array_init(arr, 8, INT_MAX, 8);
    n = opal_graph_get_graph_vertices(g, arr);
    test_verify("get_graph_vertices returns order", 5 == n);

    /* Every vertex pointer should appear exactly once somewhere in arr. */
    {
        int i;
        int found_A = 0, found_E = 0;
        for (i = 0; i < n; i++) {
            void *p = opal_pointer_array_get_item(arr, i);
            if (p == (void *) vA) {
                found_A = 1;
            }
            if (p == (void *) vE) {
                found_E = 1;
            }
        }
        test_verify("get_graph_vertices contains vA", 1 == found_A);
        test_verify("get_graph_vertices contains vE", 1 == found_E);
    }
    OBJ_RELEASE(arr);

    /* Empty graph returns 0 */
    {
        opal_graph_t *empty = OBJ_NEW(opal_graph_t);
        opal_pointer_array_t *arr2 = OBJ_NEW(opal_pointer_array_t);
        opal_pointer_array_init(arr2, 4, INT_MAX, 4);
        n = opal_graph_get_graph_vertices(empty, arr2);
        test_verify("get_graph_vertices on empty graph == 0", 0 == n);
        OBJ_RELEASE(arr2);
        OBJ_RELEASE(empty);
    }

    destroy_graph(g, vA, vB, vC, vD, vE);
}

static void test_get_adjacent_vertices(void)
{
    opal_graph_t *g;
    opal_graph_vertex_t *vA, *vB, *vC, *vD, *vE;
    opal_value_array_t adj;
    int n;
    size_t i;

    build_graph(&g, &vA, &vB, &vC, &vD, &vE);

    OBJ_CONSTRUCT(&adj, opal_value_array_t);
    opal_value_array_init(&adj, sizeof(vertex_distance_from_t));

    /* A has out-edges to B(1) and C(4) */
    n = opal_graph_get_adjacent_vertices(g, vA, &adj);
    test_verify("get_adjacent_vertices(A) == 2", 2 == n);
    {
        int found_B = 0, found_C = 0;
        for (i = 0; i < opal_value_array_get_size(&adj); i++) {
            vertex_distance_from_t *vd = (vertex_distance_from_t *)
                opal_value_array_get_item(&adj, i);
            if (NULL == vd) {
                continue;
            }
            if (vd->vertex == vB && 1 == vd->weight) {
                found_B = 1;
            }
            if (vd->vertex == vC && 4 == vd->weight) {
                found_C = 1;
            }
        }
        test_verify("adjacent_vertices(A) contains B with weight 1", 1 == found_B);
        test_verify("adjacent_vertices(A) contains C with weight 4", 1 == found_C);
    }

    /* E has no out-edges */
    opal_value_array_set_size(&adj, 0);
    n = opal_graph_get_adjacent_vertices(g, vE, &adj);
    test_verify("get_adjacent_vertices(E) == 0 (no out-edges)", 0 == n);

    OBJ_DESTRUCT(&adj);
    destroy_graph(g, vA, vB, vC, vD, vE);
}

static void test_dijkstra(void)
{
    opal_graph_t *g;
    opal_graph_vertex_t *vA, *vB, *vC, *vD, *vE;
    opal_value_array_t dist;
    uint32_t n;
    size_t i;
    int found_B = 0, found_C = 0, found_D = 0, found_E = 0;

    build_graph(&g, &vA, &vB, &vC, &vD, &vE);

    OBJ_CONSTRUCT(&dist, opal_value_array_t);
    opal_value_array_init(&dist, sizeof(vertex_distance_from_t));
    opal_value_array_reserve(&dist, 10);

    /* Dijkstra from A: output has order-1 == 4 entries (source excluded). */
    n = opal_graph_dijkstra(g, vA, &dist);
    test_verify("dijkstra from A returns order-1 == 4 entries", 4 == n);

    /*
     * Hand-computed shortest distances from A:
     *   B=1, C=3 (via A->B->C), D=4 (via A->B->C->D), E=7 (via A->B->C->D->E)
     * Note: adjacent(A,C)=4 but shortest path is 3.
     */
    for (i = 0; i < opal_value_array_get_size(&dist); i++) {
        vertex_distance_from_t *vd = (vertex_distance_from_t *)
            opal_value_array_get_item(&dist, i);
        if (NULL == vd) {
            continue;
        }
        if (vd->vertex == vB) {
            found_B = 1;
            test_verify("dijkstra dist(A,B) == 1", 1 == vd->weight);
        }
        if (vd->vertex == vC) {
            found_C = 1;
            /* Shortest via B->C (1+2=3), not direct A->C (4). */
            test_verify("dijkstra dist(A,C) == 3 (via B, not direct)", 3 == vd->weight);
        }
        if (vd->vertex == vD) {
            found_D = 1;
            test_verify("dijkstra dist(A,D) == 4", 4 == vd->weight);
        }
        if (vd->vertex == vE) {
            found_E = 1;
            test_verify("dijkstra dist(A,E) == 7", 7 == vd->weight);
        }
    }
    test_verify("dijkstra output contained B", 1 == found_B);
    test_verify("dijkstra output contained C", 1 == found_C);
    test_verify("dijkstra output contained D", 1 == found_D);
    test_verify("dijkstra output contained E", 1 == found_E);

    OBJ_DESTRUCT(&dist);
    destroy_graph(g, vA, vB, vC, vD, vE);
}

static void test_spf(void)
{
    opal_graph_t *g;
    opal_graph_vertex_t *vA, *vB, *vC, *vD, *vE;
    uint32_t d;

    build_graph(&g, &vA, &vB, &vC, &vD, &vE);

    d = opal_graph_spf(g, vA, vB);
    test_verify("spf(A,B) == 1", 1 == d);

    d = opal_graph_spf(g, vA, vC);
    test_verify("spf(A,C) == 3 (NOT 4; shortest is via B)", 3 == d);

    d = opal_graph_spf(g, vA, vD);
    test_verify("spf(A,D) == 4", 4 == d);

    d = opal_graph_spf(g, vA, vE);
    test_verify("spf(A,E) == 7", 7 == d);

    /* No path from E back to A */
    d = opal_graph_spf(g, vE, vA);
    test_verify("spf(E,A) == INFINITY (no out-edges from E)",
                DISTANCE_INFINITY == d);

    /*
     * Note: opal_graph_spf(v, v) (the trivial self path) currently returns
     * DISTANCE_INFINITY rather than 0, because opal_graph_dijkstra()
     * excludes the source vertex (Q[0]) from its output array, so spf()
     * never finds vertex2 == vertex1.  Whether that ought to be 0 is
     * arguably a defect, but "fixing" it would change dijkstra()'s
     * published output for every caller, so we only exercise the path here
     * (for coverage) without asserting a specific self-distance value.
     */
    d = opal_graph_spf(g, vA, vA);
    (void) d;

    destroy_graph(g, vA, vB, vC, vD, vE);
}

static void test_remove_edge(void)
{
    opal_graph_t *g;
    opal_graph_vertex_t *vA, *vB, *vC, *vD, *vE;
    opal_graph_edge_t *e_extra;
    uint32_t w;

    build_graph(&g, &vA, &vB, &vC, &vD, &vE);

    /* Add a second edge A->B with weight 10, then remove it. */
    e_extra = make_edge(vA, vB, 10);
    opal_graph_add_edge(g, e_extra);
    test_verify("size after add extra edge == 7", 7 == opal_graph_get_size(g));

    /* remove_edge un-lists the edge but does NOT free it. */
    opal_graph_remove_edge(g, e_extra);
    test_verify("size after remove_edge == 6", 6 == opal_graph_get_size(g));

    /* The removed edge is no longer found via adjacent -- only the 1-weight
       edge should remain, so adjacent(A,B) is back to 1. */
    w = opal_graph_adjacent(g, vA, vB);
    test_verify("adjacent(A,B) == 1 after removing weight-10 edge", 1 == w);

    /* Must free the removed edge ourselves (per API comment). */
    OBJ_RELEASE(e_extra);

    destroy_graph(g, vA, vB, vC, vD, vE);
}

static void test_remove_vertex(void)
{
    opal_graph_t *g;
    opal_graph_vertex_t *vA, *vB, *vC, *vD, *vE;

    build_graph(&g, &vA, &vB, &vC, &vD, &vE);

    /*
     * Remove C.  Edges removed: A->C(4), B->C(2), C->D(1) — three edges.
     * remove_vertex() also frees vC via OBJ_RELEASE, so we must NOT
     * release vC ourselves after this point.
     */
    opal_graph_remove_vertex(g, vC);
    vC = NULL; /* freed inside remove_vertex */

    test_verify("order after removing C == 4", 4 == opal_graph_get_order(g));

    /*
     * remove_vertex() frees the three edges touching C (A->C, B->C, C->D)
     * and keeps the graph's edge count in sync, so get_size() drops from
     * 6 to 3.
     */
    test_verify("size after removing C == 3", 3 == opal_graph_get_size(g));

    /* Remaining edges: A->B:1, B->D:5, D->E:3 */
    test_verify("adjacent(A,B) still 1 after removing C", 1 ==
                opal_graph_adjacent(g, vA, vB));
    test_verify("adjacent(B,D) still 5 after removing C", 5 ==
                opal_graph_adjacent(g, vB, vD));
    test_verify("adjacent(D,E) still 3 after removing C", 3 ==
                opal_graph_adjacent(g, vD, vE));

    /* A->C no longer valid (C is gone) -- release graph then remaining vertices. */
    OBJ_RELEASE(g);
    OBJ_RELEASE(vA);
    OBJ_RELEASE(vB);
    /* vC already freed */
    OBJ_RELEASE(vD);
    OBJ_RELEASE(vE);
}

static void test_duplicate(void)
{
    opal_graph_t *src, *dst;
    opal_graph_vertex_t *vA, *vB, *vC, *vD, *vE;
    opal_pointer_array_t *arr;
    int n;

    build_graph(&src, &vA, &vB, &vC, &vD, &vE);

    opal_graph_duplicate(&dst, src);

    test_verify("duplicate order matches source", opal_graph_get_order(src) ==
                opal_graph_get_order(dst));
    test_verify("duplicate size matches source", opal_graph_get_size(src) ==
                opal_graph_get_size(dst));
    test_verify("duplicate order == 5", 5 == opal_graph_get_order(dst));
    test_verify("duplicate size == 6",  6 == opal_graph_get_size(dst));

    /* Verify the duplicate's vertices are distinct objects from the source's. */
    arr = OBJ_NEW(opal_pointer_array_t);
    opal_pointer_array_init(arr, 8, INT_MAX, 8);
    n = opal_graph_get_graph_vertices(dst, arr);
    test_verify("duplicate get_graph_vertices == 5", 5 == n);
    {
        int i;
        int distinct = 1;
        for (i = 0; i < n; i++) {
            void *p = opal_pointer_array_get_item(arr, i);
            if (p == (void *) vA || p == (void *) vB ||
                p == (void *) vC || p == (void *) vD ||
                p == (void *) vE) {
                distinct = 0;
                break;
            }
        }
        test_verify("duplicate vertices are new objects (not the originals)", 1 == distinct);
    }
    OBJ_RELEASE(arr);

    /*
     * Verify adjacency is preserved in the duplicate.
     * We can't use the original vertex pointers against dst; use find_vertex
     * (which relies on compare_vertex being set, which duplicate copies).
     */
    {
        int key_a = 0, key_b = 1, key_c = 2;
        opal_graph_vertex_t *da, *db, *dc;
        uint32_t w;

        da = opal_graph_find_vertex(dst, &key_a);
        db = opal_graph_find_vertex(dst, &key_b);
        dc = opal_graph_find_vertex(dst, &key_c);

        test_verify("find_vertex A in duplicate non-NULL", NULL != da);
        test_verify("find_vertex B in duplicate non-NULL", NULL != db);
        test_verify("find_vertex C in duplicate non-NULL", NULL != dc);

        if (NULL != da && NULL != db) {
            w = opal_graph_adjacent(dst, da, db);
            test_verify("duplicate adjacent(A,B) == 1", 1 == w);
        }
        if (NULL != da && NULL != dc) {
            w = opal_graph_adjacent(dst, da, dc);
            test_verify("duplicate adjacent(A,C) == 4", 4 == w);
        }
    }

    /*
     * Release the duplicate.  opal_graph_duplicate creates its own vertices
     * via OBJ_NEW, adds them, and the duplicate graph owns them via the
     * adjacency list.  However, the graph destructor does NOT free vertices.
     * Collect them first, then release the graph, then release each vertex.
     */
    {
        opal_pointer_array_t *dst_verts = OBJ_NEW(opal_pointer_array_t);
        int i, cnt;
        opal_pointer_array_init(dst_verts, 8, INT_MAX, 8);
        cnt = opal_graph_get_graph_vertices(dst, dst_verts);
        OBJ_RELEASE(dst);
        for (i = 0; i < cnt; i++) {
            opal_graph_vertex_t *v = (opal_graph_vertex_t *)
                opal_pointer_array_get_item(dst_verts, i);
            if (NULL != v) {
                OBJ_RELEASE(v);
            }
        }
        OBJ_RELEASE(dst_verts);
    }

    destroy_graph(src, vA, vB, vC, vD, vE);
}

static void test_print(void)
{
    opal_graph_t *g;
    opal_graph_vertex_t *vA, *vB, *vC, *vD, *vE;

    build_graph(&g, &vA, &vB, &vC, &vD, &vE);

    /*
     * opal_graph_print() calls opal_output() to print the graph to
     * stream 0. There is no return value; we just verify it does not
     * crash, which is covered by reaching this point.
     */
    opal_graph_print(g);
    test_verify("opal_graph_print completes without crash", 1);

    /* Also exercise with NULL print_vertex hooks (should print empty strings). */
    {
        opal_graph_t *g2 = OBJ_NEW(opal_graph_t);
        opal_graph_vertex_t *v1 = OBJ_NEW(opal_graph_vertex_t);
        opal_graph_vertex_t *v2 = OBJ_NEW(opal_graph_vertex_t);
        opal_graph_edge_t *e = make_edge(v1, v2, 7);

        /* No print_vertex hook; print_vertex == NULL on construct. */
        opal_graph_add_vertex(g2, v1);
        opal_graph_add_vertex(g2, v2);
        opal_graph_add_edge(g2, e);

        opal_graph_print(g2);
        test_verify("opal_graph_print with NULL hooks completes", 1);

        OBJ_RELEASE(g2);
        OBJ_RELEASE(v1);
        OBJ_RELEASE(v2);
    }

    destroy_graph(g, vA, vB, vC, vD, vE);
}

/* --------------------------------------------------------------------------
 * main
 * -------------------------------------------------------------------------- */

int main(int argc, char *argv[])
{
    int rc;

    test_init("opal_graph_t");

    rc = opal_init_util(&argc, &argv);
    test_verify("opal_init_util succeeds", OPAL_SUCCESS == rc);
    if (OPAL_SUCCESS != rc) {
        return test_finalize();
    }

    test_order_and_size();
    test_add_edge_error();
    test_adjacent();
    test_find_vertex();
    test_get_graph_vertices();
    test_get_adjacent_vertices();
    test_dijkstra();
    test_spf();
    test_remove_edge();
    test_remove_vertex();
    test_duplicate();
    test_print();

    opal_finalize_util();

    return test_finalize();
}
