/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 *
 * Copyright (c) 2006      Voltaire. All rights reserved.
 * Copyright (c) 2007      Mellanox Technologies. All rights reserved.
 * Copyright (c) 2009      IBM Corporation.  All rights reserved.
 * Copyright (c) 2013      NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015-2017 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal/util/output.h"
#include "rcache_base_vma_tree.h"

OBJ_CLASS_INSTANCE(mca_rcache_base_vma_reg_list_item_t, opal_list_item_t, NULL, NULL);

static void mca_rcache_base_vma_item_construct (mca_rcache_base_vma_item_t *vma_item)
{
    OBJ_CONSTRUCT(&vma_item->reg_list, opal_list_t);
}

static void mca_rcache_base_vma_item_destruct (mca_rcache_base_vma_item_t *vma_item)
{
    OPAL_LIST_DESTRUCT(&vma_item->reg_list);
}

OBJ_CLASS_INSTANCE(mca_rcache_base_vma_item_t, opal_list_item_t,
                   mca_rcache_base_vma_item_construct,
                   mca_rcache_base_vma_item_destruct);


/**
 * Function for the red black tree to compare 2 keys
 *
 * @param key1 a pointer to the 1st key
 * @param key2 a pointer to the second key
 *
 * @retval -1 if key1 is below key2
 * @retval 1 if key 1 is above key2
 * @retval 0 if the keys are the same
 */

static int mca_rcache_base_vma_tree_node_compare(void *key1, void *key2)
{
    mca_rcache_base_vma_item_t *vma1 = (mca_rcache_base_vma_item_t *) key1,
        *vma2 = (mca_rcache_base_vma_item_t *) key2;

    if (vma1->start < vma2->start) {
        return -1;
    }

    if (vma1->start > vma2->start) {
        return 1;
    }

    return 0;
}

static int mca_rcache_base_vma_tree_node_compare_search(void *key1, void *key2)
{
    mca_rcache_base_vma_item_t *vma = (mca_rcache_base_vma_item_t *) key2;
    uintptr_t addr = (uintptr_t) key1;

    if (vma->end < addr) {
        return 1;
    }

    if (vma->start <= addr) {
        return 0;
    }

    return -1;
}

static int mca_rcache_base_vma_tree_node_compare_closest(void *key1, void *key2)
{
    mca_rcache_base_vma_item_t *vma = (mca_rcache_base_vma_item_t *) key2, *prev_vma;
    uintptr_t addr = (uintptr_t) key1;

    if (vma->end < addr) {
        return 1;
    }

    if (vma->start <= addr) {
        return 0;
    }

    prev_vma = (mca_rcache_base_vma_item_t *) opal_list_get_prev (&vma->super);
    if (prev_vma == (mca_rcache_base_vma_item_t *) opal_list_get_end (&vma->vma_module->vma_list)
        || prev_vma->end < addr) {
        return 0;
    }

    return -1;
}

static inline
mca_rcache_base_vma_item_t *mca_rcache_base_vma_new (mca_rcache_base_vma_module_t *vma_module,
                                                     uintptr_t start, uintptr_t end)
{
    mca_rcache_base_vma_item_t *vma_item = OBJ_NEW(mca_rcache_base_vma_item_t);

    if (NULL == vma_item) {
        return NULL;
    }

    vma_item->start = start;
    vma_item->end = end;
    vma_item->vma_module = vma_module;

    (void) opal_rb_tree_insert (&vma_module->rb_tree, vma_item, vma_item);

    return vma_item;
}

static inline int mca_rcache_base_vma_compare_regs (mca_rcache_base_registration_t *reg1,
                                                    mca_rcache_base_registration_t *reg2)
{
    /* persisten registration are on top */
    if ((reg1->flags & MCA_RCACHE_FLAGS_PERSIST) &&
        !(reg2->flags & MCA_RCACHE_FLAGS_PERSIST)) {
        return 1;
    }

    if (!(reg1->flags & MCA_RCACHE_FLAGS_PERSIST) &&
        (reg2->flags & MCA_RCACHE_FLAGS_PERSIST)) {
        return -1;
    }

    if (reg1->bound != reg2->bound) {
        return (int)(reg1->bound - reg2->bound);
    }

    /* tie breaker */
    return (int)((intptr_t)reg1 - (intptr_t)reg2);
}

static inline int mca_rcache_base_vma_add_reg (mca_rcache_base_vma_item_t *vma_item,
                                               struct mca_rcache_base_registration_t *reg)
{
    mca_rcache_base_vma_reg_list_item_t *item, *entry;

    entry = OBJ_NEW(mca_rcache_base_vma_reg_list_item_t);

    if (!entry) {
        return -1;
    }

    entry->reg = reg;

    OPAL_LIST_FOREACH(item, &vma_item->reg_list, mca_rcache_base_vma_reg_list_item_t) {
        if (mca_rcache_base_vma_compare_regs(item->reg, reg) > 0) {
            continue;
        }

        opal_list_insert_pos (&vma_item->reg_list, &item->super, &entry->super);
        return 0;
    }

    opal_list_append (&vma_item->reg_list, &entry->super);

    return 0;
}

static inline void mca_rcache_base_vma_remove_reg (mca_rcache_base_vma_item_t *vma_item,
                                                   struct mca_rcache_base_registration_t *reg)
{
    mca_rcache_base_vma_reg_list_item_t *item;

    OPAL_LIST_FOREACH(item, &vma_item->reg_list, mca_rcache_base_vma_reg_list_item_t) {
        if(item->reg == reg) {
            opal_list_remove_item(&vma_item->reg_list, &item->super);
            OBJ_RELEASE(item);
            break;
        }
    }
}

static inline int mca_rcache_base_vma_copy_reg_list (mca_rcache_base_vma_item_t *to,
                                                     mca_rcache_base_vma_item_t *from)
{

    mca_rcache_base_vma_reg_list_item_t *item_f, *item_t;
    OPAL_LIST_FOREACH(item_f, &from->reg_list, mca_rcache_base_vma_reg_list_item_t) {
        item_t = OBJ_NEW(mca_rcache_base_vma_reg_list_item_t);

        if (NULL == item_t) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }

        item_t->reg = item_f->reg;

        opal_list_append (&to->reg_list, &item_t->super);
    }

    return OPAL_SUCCESS;
}

/* returns 1 iff two lists contain the same entries */
static inline int mca_rcache_base_vma_compare_reg_lists (mca_rcache_base_vma_item_t *vma1,
                                                         mca_rcache_base_vma_item_t *vma2)
{
    mca_rcache_base_vma_reg_list_item_t *i1, *i2;

    if (!vma1 || !vma2 || opal_list_get_size (&vma1->reg_list) != opal_list_get_size (&vma2->reg_list)) {
        return 0;
    }

    i2 = (mca_rcache_base_vma_reg_list_item_t *) opal_list_get_first(&vma2->reg_list);

    OPAL_LIST_FOREACH(i1, &vma1->reg_list, mca_rcache_base_vma_reg_list_item_t) {
        if ((void *) i2 == (void *) opal_list_get_end (&vma2->reg_list) || i1->reg != i2->reg) {
            return 0;
        }

        i2 = (mca_rcache_base_vma_reg_list_item_t *) opal_list_get_next (&i2->super);
    }

    return 1;
}

int mca_rcache_base_vma_tree_init (mca_rcache_base_vma_module_t *vma_module)
{
    OBJ_CONSTRUCT(&vma_module->rb_tree, opal_rb_tree_t);
    OBJ_CONSTRUCT(&vma_module->vma_list, opal_list_t);
    OBJ_CONSTRUCT(&vma_module->vma_gc_list, opal_list_t);
    vma_module->reg_cur_cache_size = 0;
    return opal_rb_tree_init (&vma_module->rb_tree, mca_rcache_base_vma_tree_node_compare);
}

void mca_rcache_base_vma_tree_finalize (mca_rcache_base_vma_module_t *vma_module)
{
    opal_rb_tree_init(&vma_module->rb_tree,  mca_rcache_base_vma_tree_node_compare);
    OBJ_DESTRUCT(&vma_module->vma_list);
    OBJ_DESTRUCT(&vma_module->vma_gc_list);
    OBJ_DESTRUCT(&vma_module->rb_tree);
}

mca_rcache_base_registration_t *mca_rcache_base_vma_tree_find (mca_rcache_base_vma_module_t *vma_module,
                                                              unsigned char *base, unsigned char *bound)
{
    mca_rcache_base_vma_item_t *vma;
    mca_rcache_base_vma_reg_list_item_t *item;

    opal_mutex_lock (&vma_module->vma_lock);

    vma = (mca_rcache_base_vma_item_t *) opal_rb_tree_find_with (&vma_module->rb_tree, base,
                                                                 mca_rcache_base_vma_tree_node_compare_search);
    if (!vma) {
        opal_mutex_unlock (&vma_module->vma_lock);
        return NULL;
    }

    OPAL_LIST_FOREACH(item, &vma->reg_list, mca_rcache_base_vma_reg_list_item_t) {
        if(item->reg->flags & MCA_RCACHE_FLAGS_INVALID) {
            continue;
        }

        if(item->reg->bound >= bound) {
            opal_mutex_unlock (&vma_module->vma_lock);
            return item->reg;
        }

        if(!(item->reg->flags & MCA_RCACHE_FLAGS_PERSIST)) {
            break;
        }
    }

    opal_mutex_unlock (&vma_module->vma_lock);

    return NULL;
}

static inline bool is_reg_in_array (mca_rcache_base_registration_t **regs,
                                    int cnt, mca_rcache_base_registration_t *p)
{
    for (int i = 0 ; i < cnt ; ++i) {
        if (regs[i] == p) {
            return true;
        }
    }

    return false;
}

int mca_rcache_base_vma_tree_find_all (mca_rcache_base_vma_module_t *vma_module, unsigned char *base,
                                       unsigned char *bound, mca_rcache_base_registration_t **regs,
                                       int reg_cnt)
{
    int cnt = 0;

    if(opal_list_get_size(&vma_module->vma_list) == 0)
        return cnt;

    opal_mutex_lock (&vma_module->vma_lock);

    do {
        mca_rcache_base_vma_item_t *vma;
        mca_rcache_base_vma_reg_list_item_t *vma_item;
        vma = (mca_rcache_base_vma_item_t *) opal_rb_tree_find_with (&vma_module->rb_tree, base,
                                                                     mca_rcache_base_vma_tree_node_compare_closest);

        if (NULL == vma) {
            /* base is bigger than any registered memory */
            break;
        }

        if (base < (unsigned char *) vma->start) {
            base = (unsigned char *) vma->start;
            continue;
        }

        OPAL_LIST_FOREACH(vma_item, &vma->reg_list, mca_rcache_base_vma_reg_list_item_t) {
            if (vma_item->reg->flags & MCA_RCACHE_FLAGS_INVALID ||
                is_reg_in_array (regs, cnt, vma_item->reg)) {
                continue;
            }
            regs[cnt++] = vma_item->reg;
            if (cnt == reg_cnt) {
                opal_mutex_unlock (&vma_module->vma_lock);
                return cnt; /* no space left in the provided array */
            }
        }

        base = (unsigned char *)vma->end + 1;
    } while (bound >= base);

    opal_mutex_unlock (&vma_module->vma_lock);

    return cnt;
}

static inline void mca_rcache_base_vma_update_byte_count (mca_rcache_base_vma_module_t *vma_module,
                                                     size_t nbytes)
{
    vma_module->reg_cur_cache_size += nbytes;
}

int mca_rcache_base_vma_tree_iterate (mca_rcache_base_vma_module_t *vma_module, unsigned char *base,
                                      size_t size, int (*callback_fn) (struct mca_rcache_base_registration_t *, void *),
                                      void *ctx)
{
    unsigned char *bound = base + size - 1;
    mca_rcache_base_vma_item_t *vma;
    int rc = OPAL_SUCCESS;

    if (opal_list_get_size(&vma_module->vma_list) == 0) {
        /* nothin to do */
        return OPAL_SUCCESS;
    }

    opal_mutex_lock (&vma_module->vma_lock);

    do {
        mca_rcache_base_vma_reg_list_item_t *vma_item, *next;
        vma = (mca_rcache_base_vma_item_t *) opal_rb_tree_find_with (&vma_module->rb_tree, base,
                                                                     mca_rcache_base_vma_tree_node_compare_closest);

        if (NULL == vma) {
            /* base is bigger than any registered memory */
            break;
        }

        if (base < (unsigned char *) vma->start) {
            base = (unsigned char *) vma->start;
            continue;
        }

        base = (unsigned char *)vma->end + 1;

        /* all the registrations in the vma may be deleted by the callback so keep a
         * reference until we are done with it. */
        OBJ_RETAIN(vma);

        OPAL_LIST_FOREACH_SAFE(vma_item, next, &vma->reg_list, mca_rcache_base_vma_reg_list_item_t) {
            rc = callback_fn (vma_item->reg, ctx);
            if (OPAL_SUCCESS != rc) {
                break;
            }
        }

        OBJ_RELEASE(vma);

        if (OPAL_SUCCESS != rc) {
            break;
        }
    } while (bound >= base);

    opal_mutex_unlock (&vma_module->vma_lock);

    return rc;
}

static inline int mca_rcache_base_vma_can_insert (mca_rcache_base_vma_module_t *vma_module, size_t nbytes, size_t limit)
{
    return (0 == limit || vma_module->reg_cur_cache_size + nbytes <= limit);
}

/**
 * Free deleted vmas. This can not be done when they are deleted without running
 * into deadlock problems with some libc versions. The caller MUST hold the vma_lock
 * when calling this function.
 */
static void mca_rcache_base_vma_cleanup (mca_rcache_base_vma_module_t *vma_module)
{
    opal_list_item_t *item;

    while (NULL != (item = opal_list_remove_first (&vma_module->vma_gc_list))) {
        OBJ_RELEASE(item);
    }
}

int mca_rcache_base_vma_tree_insert (mca_rcache_base_vma_module_t *vma_module,
                                     mca_rcache_base_registration_t *reg, size_t limit)
{
    mca_rcache_base_vma_item_t *i;
    uintptr_t begin = (uintptr_t)reg->base, end = (uintptr_t)reg->bound;

    opal_mutex_lock (&vma_module->vma_lock);

    mca_rcache_base_vma_cleanup (vma_module);

    i = (mca_rcache_base_vma_item_t *) opal_rb_tree_find_with (&vma_module->rb_tree,
            (void *) begin, mca_rcache_base_vma_tree_node_compare_closest);

    if (!i) {
        i = (mca_rcache_base_vma_item_t *) opal_list_get_end (&vma_module->vma_list);
    }

    while (begin <= end) {
        mca_rcache_base_vma_item_t *vma = NULL;

        if (opal_list_get_end (&vma_module->vma_list) == &i->super) {
            if (mca_rcache_base_vma_can_insert (vma_module, end - begin + 1, limit)) {
                vma = mca_rcache_base_vma_new(vma_module, begin, end);
            }

            if (!vma) {
                goto remove;
            }

            mca_rcache_base_vma_update_byte_count (vma_module, end - begin + 1);

            opal_list_append(&vma_module->vma_list, &vma->super);
            begin = vma->end + 1;
            mca_rcache_base_vma_add_reg (vma, reg);
            opal_mutex_unlock (&vma_module->vma_lock);
            return OPAL_SUCCESS;
        }

        if (i->start > begin) {
            uintptr_t tend = (i->start <= end) ? (i->start - 1) : end;
            if (mca_rcache_base_vma_can_insert(vma_module, tend - begin + 1, limit)) {
                vma = mca_rcache_base_vma_new(vma_module, begin, tend);
            }

            if (!vma) {
                goto remove;
            }

            mca_rcache_base_vma_update_byte_count (vma_module, tend - begin + 1);

            /* insert before */
            opal_list_insert_pos(&vma_module->vma_list, &i->super, &vma->super);
            i = vma;
            begin = vma->end + 1;
            mca_rcache_base_vma_add_reg (vma, reg);
        } else if(i->start == begin) {
            if (i->end > end) {
                vma = mca_rcache_base_vma_new (vma_module, end + 1, i->end);
                if (!vma) {
                    goto remove;
                }

                i->end = end;

                mca_rcache_base_vma_copy_reg_list (vma, i);

                /* add after */
                opal_list_insert_pos (&vma_module->vma_list,
                                      opal_list_get_next (&i->super),
                                      &vma->super);
                mca_rcache_base_vma_add_reg (i, reg);
                begin = end + 1;
            } else {
                mca_rcache_base_vma_add_reg(i, reg);
                begin = i->end + 1;
            }
        } else {
                vma = mca_rcache_base_vma_new (vma_module, begin, i->end);

                if (!vma) {
                    goto remove;
                }

                i->end = begin - 1;

                mca_rcache_base_vma_copy_reg_list (vma, i);

                /* add after */
                opal_list_insert_pos (&vma_module->vma_list,
                                      opal_list_get_next (&i->super),
                                      &vma->super);
        }

        i = (mca_rcache_base_vma_item_t *) opal_list_get_next (&i->super);
    }

    opal_mutex_unlock (&vma_module->vma_lock);

    return OPAL_SUCCESS;

remove:
    mca_rcache_base_vma_tree_delete (vma_module, reg);
    opal_mutex_unlock (&vma_module->vma_lock);

    return OPAL_ERR_TEMP_OUT_OF_RESOURCE;
}

/**
 * Function to remove previously memory from the tree without freeing it
 *
 * @param base pointer to the memory to free
 *
 * @retval OPAL_SUCCESS
 * @retval OPAL_ERR_BAD_PARAM if the passed base pointer was invalid
 */
int mca_rcache_base_vma_tree_delete (mca_rcache_base_vma_module_t *vma_module,
                                     mca_rcache_base_registration_t *reg)
{
    mca_rcache_base_vma_item_t *vma;

    opal_mutex_lock (&vma_module->vma_lock);

    vma = (mca_rcache_base_vma_item_t *)
        opal_rb_tree_find_with (&vma_module->rb_tree, reg->base,
                                mca_rcache_base_vma_tree_node_compare_search);

    if (!vma) {
        opal_mutex_unlock (&vma_module->vma_lock);
        return OPAL_ERROR;
    }

    while (vma != (mca_rcache_base_vma_item_t *) opal_list_get_end (&vma_module->vma_list)
           && vma->start <= (uintptr_t) reg->bound) {
        mca_rcache_base_vma_remove_reg(vma, reg);

        if(opal_list_is_empty(&vma->reg_list)) {
            mca_rcache_base_vma_item_t *next =
                (mca_rcache_base_vma_item_t *) opal_list_get_next (&vma->super);
            opal_rb_tree_delete (&vma_module->rb_tree, vma);
            mca_rcache_base_vma_update_byte_count (vma_module,
                                                   vma->start - vma->end - 1);
            opal_list_remove_item (&vma_module->vma_list, &vma->super);
            opal_list_append (&vma_module->vma_gc_list, &vma->super);
            vma = next;
        } else {
            int merged;

            do {
                mca_rcache_base_vma_item_t *prev = NULL, *next = NULL;
                if (opal_list_get_first (&vma_module->vma_list) != &vma->super) {
                    prev = (mca_rcache_base_vma_item_t *) opal_list_get_prev(vma);
                }

                merged = 0;

                if (prev && vma->start == prev->end + 1 &&
                    mca_rcache_base_vma_compare_reg_lists(vma, prev)) {
                    prev->end = vma->end;
                    opal_list_remove_item(&vma_module->vma_list, &vma->super);
                    opal_rb_tree_delete(&vma_module->rb_tree, vma);
                    opal_list_append (&vma_module->vma_gc_list, &vma->super);
                    vma = prev;
                    merged = 1;
                }

                if (opal_list_get_last (&vma_module->vma_list) != &vma->super) {
                    next = (mca_rcache_base_vma_item_t *) opal_list_get_next (vma);
                }

                if (next && vma->end + 1 == next->start &&
                    mca_rcache_base_vma_compare_reg_lists (vma, next)) {
                    vma->end = next->end;
                    opal_list_remove_item(&vma_module->vma_list, &next->super);
                    opal_rb_tree_delete(&vma_module->rb_tree, next);
                    opal_list_append (&vma_module->vma_gc_list, &next->super);
                    merged = 1;
                }
            } while (merged);

            vma = (mca_rcache_base_vma_item_t *) opal_list_get_next (vma);
        }
    }

    opal_mutex_unlock (&vma_module->vma_lock);

    return 0;
}

/* Dump out rcache entries within a range of memory.  Useful for debugging. */
void mca_rcache_base_vma_tree_dump_range (mca_rcache_base_vma_module_t *vma_module,
                                          unsigned char *base, size_t size, char *msg)
{
    unsigned char * bound = base + size -1;
    mca_rcache_base_registration_t *reg;

    if (NULL == msg) {
        msg = "";
    }

    opal_output(0, "Dumping rcache entries: %s", msg);

    if(opal_list_is_empty(&vma_module->vma_list)) {
        opal_output(0, "  rcache is empty");
        return;
    }

    do {
        mca_rcache_base_vma_item_t *vma;
        mca_rcache_base_vma_reg_list_item_t *vma_item;
        vma = (mca_rcache_base_vma_item_t *)
            opal_rb_tree_find_with (&vma_module->rb_tree, base,
                                    mca_rcache_base_vma_tree_node_compare_closest);

        if (NULL == vma) {
            /* base is bigger than any registered memory */
            break;
        }

        if (base < (unsigned char *) vma->start) {
            base = (unsigned char *) vma->start;
            continue;
        }

        opal_output(0, "  vma: base=%p, bound=%p, size=%lu, number of registrations=%d",
                    (void *)vma->start, (void *)vma->end, vma->end - vma->start + 1,
                    (int) opal_list_get_size(&vma->reg_list));
        OPAL_LIST_FOREACH(vma_item, &vma->reg_list, mca_rcache_base_vma_reg_list_item_t) {
            reg = vma_item->reg;
            opal_output(0, "    reg: base=%p, bound=%p, ref_count=%d, flags=0x%x",
                        (void *) reg->base, (void *) reg->bound, reg->ref_count, reg->flags);
        }
        base = (unsigned char *)vma->end + 1;
    } while (bound >= base);
}
