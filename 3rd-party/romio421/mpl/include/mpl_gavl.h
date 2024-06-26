/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#ifndef MPL_GAVL_H_INCLUDED
#define MPL_GAVL_H_INCLUDED

typedef void *MPL_gavl_tree_t;

int MPL_gavl_tree_create(void (*free_fn) (void *), MPL_gavl_tree_t * gavl_tree);
int MPL_gavl_tree_insert(MPL_gavl_tree_t gavl_tree, const void *addr, uintptr_t len,
                         const void *val);
int MPL_gavl_tree_destory(MPL_gavl_tree_t gavl_tree);
int MPL_gavl_tree_delete_range(MPL_gavl_tree_t gavl_tree, const void *addr, uintptr_t len);
int MPL_gavl_tree_delete_start_addr(MPL_gavl_tree_t gavl_tree, const void *addr);
MPL_STATIC_INLINE_PREFIX int MPL_gavl_tree_search(MPL_gavl_tree_t gavl_tree, const void *addr,
                                                  uintptr_t len, void **val);

/*
 * We assume AVL tree height will not exceed 64. AVL tree with 64 height in worst case
 * can contain 27777890035287 nodes which is far enough for current applications.
 * The idea to compute worse case nodes is as follows:
 * In worse case, AVL tree with height h_p should have h_p - 1 height left child and
 * h_p - 2 height right child; therefore, the worse case nodes N(h_p) = N(h_p - 1) + N(h_p - 2) + 1.
 * Since we know N(1) = 1 and N(2) = 2, we can use iteration to compute N(64) = 27777890035287.
 */
#define MPLI_GAVL_MAX_STACK_SIZE 64

enum {
    MPLI_GAVL_SEARCH_LEFT,
    MPLI_GAVL_SEARCH_RIGHT,
    MPLI_GAVL_BUFFER_MATCH,
    MPLI_GAVL_NO_BUFFER_MATCH
};

enum {
    /* range search */
    MPLI_GAVL_SUBSET_SEARCH,
    MPLI_GAVL_INTERSECTION_SEARCH,
    /* address search */
    MPLI_GAVL_START_ADDR_SEARCH
};

typedef struct MPLI_gavl_tree_node {
    union {
        struct {
            struct MPLI_gavl_tree_node *parent;
            struct MPLI_gavl_tree_node *left;
            struct MPLI_gavl_tree_node *right;
        } s;
        struct MPLI_gavl_tree_node *next;
    } u;
    uintptr_t height;
    uintptr_t addr;
    uintptr_t len;
    const void *val;
} MPLI_gavl_tree_node_s;

typedef struct MPLI_gavl_tree {
    MPLI_gavl_tree_node_s *root;
    void (*gavl_free_fn) (void *);
    /* internal stack structure. used to track the traverse trace for
     * tree rebalance at node insertion or deletion */
    MPLI_gavl_tree_node_s *stack[MPLI_GAVL_MAX_STACK_SIZE];
    int stack_sp;
    /* cur_node points to the starting node of tree rebalance */
    MPLI_gavl_tree_node_s *cur_node;
    /* store nodes that are removed from tree but haven't been freed */
    MPLI_gavl_tree_node_s *remove_list;
} MPLI_gavl_tree_s;

MPL_STATIC_INLINE_PREFIX int MPLI_gavl_subset_cmp_func(MPLI_gavl_tree_node_s * tnode,
                                                       uintptr_t ustart, uintptr_t len)
{
    int cmp_ret;
    uintptr_t uend = ustart + len;
    uintptr_t tstart = tnode->addr;
    uintptr_t tend = tnode->addr + tnode->len;

    if (tstart <= ustart && uend <= tend)
        cmp_ret = MPLI_GAVL_BUFFER_MATCH;
    else if (ustart < tstart)
        cmp_ret = MPLI_GAVL_SEARCH_LEFT;
    else
        cmp_ret = MPLI_GAVL_SEARCH_RIGHT;

    return cmp_ret;
}

MPL_STATIC_INLINE_PREFIX int MPLI_gavl_intersect_cmp_func(MPLI_gavl_tree_node_s * tnode,
                                                          uintptr_t ustart, uintptr_t len)
{
    int cmp_ret;
    uintptr_t uend = ustart + len;
    uintptr_t tstart = tnode->addr;
    uintptr_t tend = tnode->addr + tnode->len;

    if (uend <= tstart)
        cmp_ret = MPLI_GAVL_SEARCH_LEFT;
    else if (tend <= ustart)
        cmp_ret = MPLI_GAVL_SEARCH_RIGHT;
    else
        cmp_ret = MPLI_GAVL_BUFFER_MATCH;

    return cmp_ret;
}

MPL_STATIC_INLINE_PREFIX int MPLI_gavl_start_addr_cmp_func(MPLI_gavl_tree_node_s * tnode,
                                                           uintptr_t ustart)
{
    int cmp_ret;
    uintptr_t tstart = tnode->addr;

    if (tstart == ustart)
        cmp_ret = MPLI_GAVL_BUFFER_MATCH;
    else if (ustart < tstart)
        cmp_ret = MPLI_GAVL_SEARCH_LEFT;
    else
        cmp_ret = MPLI_GAVL_SEARCH_RIGHT;

    return cmp_ret;
}

/*
 * MPL_gavl_tree_search
 * Description: search a node that matches input key (addr, len) and return corresponding
 *              buffer object. This function is not thread-safe.
 * Parameters:
 * gavl_tree        - (IN) gavl tree object
 * addr             - (IN) input buffer starting addr
 * len              - (IN) input buffer length
 * val              - (OUT) matched buffer object
 */
MPL_STATIC_INLINE_PREFIX int MPL_gavl_tree_search(MPL_gavl_tree_t gavl_tree, const void *addr,
                                                  uintptr_t len, void **val)
{
    int mpl_err = MPL_SUCCESS;
    MPLI_gavl_tree_node_s *cur_node;
    MPLI_gavl_tree_s *tree_ptr = (MPLI_gavl_tree_s *) gavl_tree;

    *val = NULL;
    cur_node = tree_ptr->root;
    while (cur_node) {
        int cmp_ret = MPLI_gavl_subset_cmp_func(cur_node, (uintptr_t) addr, len);
        if (cmp_ret == MPLI_GAVL_BUFFER_MATCH) {
            *val = (void *) cur_node->val;
            break;
        } else if (cmp_ret == MPLI_GAVL_SEARCH_LEFT) {
            cur_node = cur_node->u.s.left;
        } else {
            cur_node = cur_node->u.s.right;
        }
    }

    return mpl_err;
}

#endif /* MPL_GAVL_H_INCLUDED  */
