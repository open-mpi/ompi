/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "mpl.h"
#include <assert.h>

#define GAVL_TREE_NODE_INIT(node_ptr, addr, len, val)   \
    do {                                                \
        (node_ptr)->height = 1;                         \
        (node_ptr)->addr = (uintptr_t) addr;            \
        (node_ptr)->len = len;                          \
        (node_ptr)->val = val;                          \
    } while (0)

#define GAVL_TREE_NODE_CMP(node_ptr, addr, len, mode, ret)      \
    do {                                                        \
        switch(mode) {                                                  \
            case MPLI_GAVL_SUBSET_SEARCH:                               \
                ret = MPLI_gavl_subset_cmp_func(node_ptr, addr, len);   \
                break;                                                  \
            case MPLI_GAVL_INTERSECTION_SEARCH:                         \
                ret = MPLI_gavl_intersect_cmp_func(node_ptr, addr, len);\
                break;                                                  \
            case MPLI_GAVL_START_ADDR_SEARCH:                           \
                ret = MPLI_gavl_start_addr_cmp_func(node_ptr, addr);    \
                break;                                                  \
        }                                                               \
    } while (0)

/* STACK is needed to rebalance the tree */
#define TREE_STACK_PUSH(tree_ptr, value)               \
    do {                                               \
        assert(tree_ptr->stack_sp < MPLI_GAVL_MAX_STACK_SIZE);   \
        tree_ptr->stack[tree_ptr->stack_sp++] = value; \
    } while (0)

#define TREE_STACK_POP(tree_ptr, value)                \
    do {                                               \
        assert(tree_ptr->stack_sp > 0);                \
        value = tree_ptr->stack[--tree_ptr->stack_sp]; \
    } while (0)

#define TREE_STACK_START(tree_ptr) (tree_ptr)->stack_sp = 0
#define TREE_STACK_IS_EMPTY(tree_ptr) (!(tree_ptr)->stack_sp)

static void gavl_tree_remove_nodes(MPLI_gavl_tree_s * tree_ptr, uintptr_t addr, uintptr_t len,
                                   int mode);
static void gavl_tree_delete_removed_nodes(MPLI_gavl_tree_s * tree_ptr, uintptr_t addr,
                                           uintptr_t len, int mode);

static void gavl_update_node_info(MPLI_gavl_tree_node_s * node_iptr)
{
    int lheight = node_iptr->u.s.left == NULL ? 0 : node_iptr->u.s.left->height;
    int rheight = node_iptr->u.s.right == NULL ? 0 : node_iptr->u.s.right->height;
    node_iptr->height = (lheight < rheight ? rheight : lheight) + 1;
    return;
}

static void gavl_right_rotation(MPLI_gavl_tree_node_s * parent_ptr, MPLI_gavl_tree_node_s * lchild)
{
    parent_ptr->u.s.left = lchild->u.s.right;
    lchild->u.s.right = parent_ptr;
    lchild->u.s.parent = parent_ptr->u.s.parent;
    if (lchild->u.s.parent != NULL) {
        if (lchild->u.s.parent->u.s.left == parent_ptr)
            lchild->u.s.parent->u.s.left = lchild;
        else
            lchild->u.s.parent->u.s.right = lchild;
    }

    parent_ptr->u.s.parent = lchild;
    if (parent_ptr->u.s.left != NULL)
        parent_ptr->u.s.left->u.s.parent = parent_ptr;

    gavl_update_node_info(parent_ptr);
    gavl_update_node_info(lchild);
    return;
}

static void gavl_left_rotation(MPLI_gavl_tree_node_s * parent_ptr, MPLI_gavl_tree_node_s * rchild)
{
    parent_ptr->u.s.right = rchild->u.s.left;
    rchild->u.s.left = parent_ptr;
    rchild->u.s.parent = parent_ptr->u.s.parent;
    if (rchild->u.s.parent != NULL) {
        if (rchild->u.s.parent->u.s.left == parent_ptr)
            rchild->u.s.parent->u.s.left = rchild;
        else
            rchild->u.s.parent->u.s.right = rchild;
    }

    parent_ptr->u.s.parent = rchild;
    if (parent_ptr->u.s.right != NULL)
        parent_ptr->u.s.right->u.s.parent = parent_ptr;

    gavl_update_node_info(parent_ptr);
    gavl_update_node_info(rchild);
    return;
}

static void gavl_left_right_rotation(MPLI_gavl_tree_node_s * parent_ptr,
                                     MPLI_gavl_tree_node_s * lchild)
{
    MPLI_gavl_tree_node_s *rlchild = lchild->u.s.right;
    gavl_left_rotation(lchild, rlchild);
    gavl_right_rotation(parent_ptr, rlchild);
    return;
}

static void gavl_right_left_rotation(MPLI_gavl_tree_node_s * parent_ptr,
                                     MPLI_gavl_tree_node_s * rchild)
{
    MPLI_gavl_tree_node_s *lrchild = rchild->u.s.left;
    gavl_right_rotation(rchild, lrchild);
    gavl_left_rotation(parent_ptr, lrchild);
    return;
}

/*
 * MPL_gavl_tree_create
 * Description: create a gavl tree
 * Parameters:
 * free_fn        - (IN) user free function to free buffer object
 * gavl_tree      - (OUT) created gavl tree
 */
int MPL_gavl_tree_create(void (*free_fn) (void *), MPL_gavl_tree_t * gavl_tree)
{
    int mpl_err = MPL_SUCCESS;
    MPLI_gavl_tree_s *tree_ptr;

    tree_ptr = (MPLI_gavl_tree_s *) MPL_calloc(1, sizeof(MPLI_gavl_tree_s), MPL_MEM_OTHER);
    if (tree_ptr == NULL) {
        mpl_err = MPL_ERR_NOMEM;
        goto fn_fail;
    }

    tree_ptr->gavl_free_fn = free_fn;
    *gavl_tree = (MPL_gavl_tree_t) tree_ptr;

  fn_exit:
    return mpl_err;
  fn_fail:
    goto fn_exit;
}

static MPLI_gavl_tree_node_s *gavl_tree_search_internal(MPLI_gavl_tree_s * tree_ptr, uintptr_t addr,
                                                        uintptr_t len, int mode, int *cmp_ret_ptr)
{
    /* this function assumes there is at least one node in the tree */
    int cmp_ret = MPLI_GAVL_NO_BUFFER_MATCH;
    MPLI_gavl_tree_node_s *cur_node = tree_ptr->root;

    TREE_STACK_START(tree_ptr);
    do {
        GAVL_TREE_NODE_CMP(cur_node, addr, len, mode, cmp_ret);
        if (cmp_ret == MPLI_GAVL_SEARCH_LEFT) {
            if (cur_node->u.s.left != NULL) {
                TREE_STACK_PUSH(tree_ptr, cur_node);
                cur_node = cur_node->u.s.left;
                continue;
            } else {
                break;
            }
        } else if (cmp_ret == MPLI_GAVL_SEARCH_RIGHT) {
            if (cur_node->u.s.right != NULL) {
                TREE_STACK_PUSH(tree_ptr, cur_node);
                cur_node = cur_node->u.s.right;
                continue;
            } else {
                break;
            }
        } else {
            /* node match */
            break;
        }
    } while (1);

    *cmp_ret_ptr = cmp_ret;
    tree_ptr->cur_node = cur_node;
    return cur_node;
}

/* if avl tree is possibly unbalanced, gavl_tree_rebalance should be called to rebalance
 * it. In unbalanced avl tree, the height difference between left and right child is at
 * most 2; gavl_tree_rebalance takes it as a premise in order to rebalance tree correcly */
static void gavl_tree_rebalance(MPLI_gavl_tree_s * tree_ptr)
{
    MPLI_gavl_tree_node_s *cur_node = tree_ptr->cur_node;

    if (cur_node) {
        do {
            gavl_update_node_info(cur_node);

            int lheight = cur_node->u.s.left == NULL ? 0 : cur_node->u.s.left->height;
            int rheight = cur_node->u.s.right == NULL ? 0 : cur_node->u.s.right->height;
            if (lheight - rheight > 1) {
                /* find imbalance: left child is 2 level higher than right child */
                MPLI_gavl_tree_node_s *lnode = cur_node->u.s.left;
                int llheight = lnode->u.s.left == NULL ? 0 : lnode->u.s.left->height;
                /* if left child's (lnode's) left child causes this imbalance, we need to perform right
                 * rotation to reduce the height of lnode's left child;
                 * right rotation sets left child (lnode) as central node, moves lnode to parent (cur_node)
                 * position, assigns cur_node to right child of lnode and then lnode's right child to left
                 * child of cur_node.
                 * else we need to perform left-right rotation for rebalance; left-right rotation first
                 * moves right child (rlnode) of left child (lnode) to lnode position and assigns left
                 * child of rlnode to right child of lnode; then set rlnode as central node and perform
                 * right rotation as mentioned above */
                if (llheight + 1 == lheight)
                    gavl_right_rotation(cur_node, lnode);
                else
                    gavl_left_right_rotation(cur_node, lnode);
            } else if (rheight - lheight > 1) {
                /* find imbalance: right child is 2 level higher than left child */
                MPLI_gavl_tree_node_s *rnode = cur_node->u.s.right;
                int rlheight = rnode->u.s.left == NULL ? 0 : rnode->u.s.left->height;
                /* the purpose of gavl_right_left_rotation and gavl_left_rotation is similar to
                 * gavl_right_rotation and gavl_left_right_rotation mention above; the difference
                 * is just doing rotation for right child here*/
                if (rlheight + 1 == rheight)
                    gavl_right_left_rotation(cur_node, rnode);
                else
                    gavl_left_rotation(cur_node, rnode);
            }

            /* rebalance the previous nodes in traverse trace */
            if (!TREE_STACK_IS_EMPTY(tree_ptr)) {
                TREE_STACK_POP(tree_ptr, cur_node);
                continue;
            } else {
                break;
            }
        } while (1);

        /* after rebalance, we need to update root because it might be changed after rebalance */
        while (tree_ptr->root && tree_ptr->root->u.s.parent != NULL)
            tree_ptr->root = tree_ptr->root->u.s.parent;
    }

    return;
}

/*
 * MPL_gavl_tree_insert
 * Description: insert a node with key (addr, len) into gavl tree. If new node is duplicate,
 *              we should not insert it and need to free the node and return. This function
 *              is not thread-safe.
 * Parameters:
 * gavl_tree        - (IN) gavl tree object
 * addr             - (IN) input buffer starting addr
 * len              - (IN) input buffer length
 * val              - (IN) buffer object
 */
int MPL_gavl_tree_insert(MPL_gavl_tree_t gavl_tree, const void *addr, uintptr_t len,
                         const void *val)
{
    int mpl_err = MPL_SUCCESS;
    MPLI_gavl_tree_node_s *node_ptr;
    MPLI_gavl_tree_s *tree_ptr = (MPLI_gavl_tree_s *) gavl_tree;

    /* we remove all nodes that are subset of input key (addr, len) from the tree and add them
     * into tree remove_list */
    gavl_tree_remove_nodes(tree_ptr, (uintptr_t) addr, len, MPLI_GAVL_SUBSET_SEARCH);

    node_ptr = (MPLI_gavl_tree_node_s *) MPL_calloc(1,
                                                    sizeof(MPLI_gavl_tree_node_s), MPL_MEM_OTHER);
    if (node_ptr == NULL) {
        mpl_err = MPL_ERR_NOMEM;
        goto fn_fail;
    }

    GAVL_TREE_NODE_INIT(node_ptr, addr, len, val);

    if (tree_ptr->root == NULL) {
        tree_ptr->root = node_ptr;
    } else {
        MPLI_gavl_tree_node_s *pnode;
        int cmp_ret;

        /* search the node which will become the parent of new node */
        pnode = gavl_tree_search_internal(tree_ptr, (uintptr_t) node_ptr->addr, node_ptr->len,
                                          MPLI_GAVL_SUBSET_SEARCH, &cmp_ret);

        /* find which side the new node should be inserted */
        if (cmp_ret == MPLI_GAVL_BUFFER_MATCH) {
            /* new node is duplicate, we need to delete new node and exit */
            tree_ptr->gavl_free_fn((void *) node_ptr->val);
            MPL_free(node_ptr);
            goto fn_exit;
        }

        /* insert new node into pnode */
        if (cmp_ret == MPLI_GAVL_SEARCH_LEFT)
            pnode->u.s.left = node_ptr;
        else
            pnode->u.s.right = node_ptr;
        node_ptr->u.s.parent = pnode;

        /* after insertion, the tree could be imbalanced, so rebalance is required here */
        gavl_tree_rebalance(tree_ptr);
    }

  fn_exit:
    return mpl_err;
  fn_fail:
    goto fn_exit;
}

/*
 * MPL_gavl_tree_destory
 * Description: free all nodes and buffer objects in the tree and tree itself.
 * Parameters:
 * gavl_tree        - (IN)  gavl tree object
 */
int MPL_gavl_tree_destory(MPL_gavl_tree_t gavl_tree)
{
    int mpl_err = MPL_SUCCESS;
    MPLI_gavl_tree_s *tree_ptr = (MPLI_gavl_tree_s *) gavl_tree;
    MPLI_gavl_tree_node_s *cur_node = tree_ptr->root;
    MPLI_gavl_tree_node_s *dnode = NULL;
    while (cur_node) {
        if (cur_node->u.s.left) {
            cur_node = cur_node->u.s.left;
        } else if (cur_node->u.s.right) {
            cur_node = cur_node->u.s.right;
        } else {
            dnode = cur_node;
            cur_node = cur_node->u.s.parent;
            if (cur_node) {
                if (cur_node->u.s.left == dnode)
                    cur_node->u.s.left = NULL;
                else
                    cur_node->u.s.right = NULL;
            }
            if (tree_ptr->gavl_free_fn)
                tree_ptr->gavl_free_fn((void *) dnode->val);
            MPL_free(dnode);
        }
    }
    MPL_free(tree_ptr);
    return mpl_err;
}

static void gavl_tree_remove_node_internal(MPLI_gavl_tree_s * tree_ptr,
                                           MPLI_gavl_tree_node_s * dnode)
{
    MPLI_gavl_tree_node_s *inorder_node;

    if (dnode->u.s.right == NULL) {
        /* no right child, next inorder node is parent */
        if (dnode->u.s.parent == NULL) {
            /* delete root node; if it has left child, set left child as root node;
             * if not, set tree root as NULL */
            if (dnode->u.s.left) {
                tree_ptr->root = dnode->u.s.left;
                tree_ptr->root->u.s.parent = NULL;
            } else {
                tree_ptr->root = NULL;
            }
        } else {
            /* assign deleted node's left child to its parent */
            inorder_node = dnode->u.s.parent;
            if (inorder_node->u.s.left == dnode)
                inorder_node->u.s.left = dnode->u.s.left;
            else
                inorder_node->u.s.right = dnode->u.s.left;

            if (dnode->u.s.left)
                dnode->u.s.left->u.s.parent = inorder_node;

            TREE_STACK_PUSH(tree_ptr, inorder_node);
        }
    } else {
        const void *tmp_val;
        uintptr_t tmp_addr, tmp_len;

        /* find the next inorder node and move its buffer objects to dnode;
         * the original buffer object in dnode is freed */
        inorder_node = dnode->u.s.right;
        TREE_STACK_PUSH(tree_ptr, dnode);

        /* search left most node of right child of dnode for next inorder node */
        while (inorder_node->u.s.left) {
            TREE_STACK_PUSH(tree_ptr, inorder_node);
            inorder_node = inorder_node->u.s.left;
        }

        /* remove inorder_node from the tree. */
        if (inorder_node->u.s.parent != dnode) {
            if (inorder_node->u.s.right)
                inorder_node->u.s.right->u.s.parent = inorder_node->u.s.parent;
            inorder_node->u.s.parent->u.s.left = inorder_node->u.s.right;
        } else {
            dnode->u.s.right = NULL;
        }

        /* exchange inorder_node with dnode and then add dnode into remove_list */
        tmp_val = dnode->val;
        tmp_addr = dnode->addr;
        tmp_len = dnode->len;
        dnode->addr = inorder_node->addr;
        dnode->len = inorder_node->len;
        dnode->val = inorder_node->val;
        inorder_node->addr = tmp_addr;
        inorder_node->len = tmp_len;
        inorder_node->val = tmp_val;
        dnode = inorder_node;
    }

    /* add removed node into remove list */
    dnode->u.next = tree_ptr->remove_list;
    tree_ptr->remove_list = dnode;

    /* update stack for the consequent rebalance which will start from the top of
     * the stack (i.e., tree_ptr->cur_node). */
    if (TREE_STACK_IS_EMPTY(tree_ptr)) {
        tree_ptr->cur_node = NULL;
    } else {
        TREE_STACK_POP(tree_ptr, tree_ptr->cur_node);
    }
    return;
}

/*
 * MPL_gavl_tree_delete_range
 * Description: delete all nodes containing the address range intersecting with the
 *              input buffer in gavl tree and free corresponding buffer objects
 *              using user-provided free function. This function is not thread-safe.
 * Parameters:
 * gavl_tree        - (IN) gavl tree object
 * addr             - (IN) input buffer starting addr
 * len              - (IN) input buffer length
 */
int MPL_gavl_tree_delete_range(MPL_gavl_tree_t gavl_tree, const void *addr, uintptr_t len)
{
    int mpl_err = MPL_SUCCESS;
    MPLI_gavl_tree_s *tree_ptr = (MPLI_gavl_tree_s *) gavl_tree;

    /* move all nodes intersecting input buffer (addr, len) to remove_list */
    gavl_tree_remove_nodes(tree_ptr, (uintptr_t) addr, len, MPLI_GAVL_INTERSECTION_SEARCH);

    /* free nodes and buffer objects from remove list */
    gavl_tree_delete_removed_nodes(tree_ptr, (uintptr_t) addr, len, MPLI_GAVL_INTERSECTION_SEARCH);

  fn_exit:
    return mpl_err;
  fn_fail:
    goto fn_exit;
}


/* MPL_gavl_tree_delete_start_addr
 * Description: delete all nodes containing the starting address strictly matching
 *              the input address. This function is not thread-safe.
 * Parameters:
 * gavl_tree        - (IN) gavl tree object
 * addr             - (IN) input buffer starting addr
 */
int MPL_gavl_tree_delete_start_addr(MPL_gavl_tree_t gavl_tree, const void *addr)
{
    int mpl_err = MPL_SUCCESS;
    MPLI_gavl_tree_s *tree_ptr = (MPLI_gavl_tree_s *) gavl_tree;

    /* move all nodes intersecting input buffer (addr, len) to remove_list */
    gavl_tree_remove_nodes(tree_ptr, (uintptr_t) addr, 0, MPLI_GAVL_START_ADDR_SEARCH);

    /* free nodes and buffer objects from remove list */
    gavl_tree_delete_removed_nodes(tree_ptr, (uintptr_t) addr, 0, MPLI_GAVL_START_ADDR_SEARCH);

  fn_exit:
    return mpl_err;
  fn_fail:
    goto fn_exit;
}

static void gavl_tree_remove_nodes(MPLI_gavl_tree_s * tree_ptr, uintptr_t addr, uintptr_t len,
                                   int mode)
{
    int cmp_ret;
    MPLI_gavl_tree_node_s *dnode;

    while (tree_ptr->root) {
        /* search and return the node to be deleted */
        dnode = gavl_tree_search_internal(tree_ptr, (uintptr_t) addr, len, mode, &cmp_ret);

        /* check whether dnode matches (addr, len) */
        if (cmp_ret != MPLI_GAVL_BUFFER_MATCH) {
            /* we didn't find deleted node and exit */
            goto fn_exit;
        }

        /* detach the matched node from tree and add removed node into remove list */
        gavl_tree_remove_node_internal(tree_ptr, dnode);

        /* we perform rebalance after every internal deletion in order to ensure
         * lightweight rebalance that rotates left and right childs with at most
         * 2 height difference. */
        gavl_tree_rebalance(tree_ptr);
    };

  fn_exit:
    return;
}

/* gavl_tree_delete_removed_nodes searches all nodes that match with the
 * input key (addr, len) and search mode in remove_list and delete them.*/
static void gavl_tree_delete_removed_nodes(MPLI_gavl_tree_s * tree_ptr, uintptr_t addr,
                                           uintptr_t len, int mode)
{
    int cmp_ret;
    MPLI_gavl_tree_node_s *prev, *cur, *dnode;

    cur = tree_ptr->remove_list;
    prev = NULL;
    while (cur) {
        GAVL_TREE_NODE_CMP(cur, addr, len, mode, cmp_ret);
        if (cmp_ret == MPLI_GAVL_BUFFER_MATCH) {
            if (prev)
                prev->u.next = cur->u.next;
            else
                tree_ptr->remove_list = cur->u.next;

            dnode = cur;
            cur = cur->u.next;
            if (tree_ptr->gavl_free_fn)
                tree_ptr->gavl_free_fn((void *) dnode->val);
            MPL_free(dnode);
        } else {
            prev = cur;
            cur = cur->u.next;
        }
    }
}
