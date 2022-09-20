/*
 * hb_tree.h
 *
 * Interface for height balanced tree.
 * Copyright (C) 2001 Farooq Mela.
 * Copyright (c) 2022 IBM Corporation.  All rights reserved.
 *
 * $Id: hb_tree.h,v 1.2 2001/09/10 06:46:41 farooq Exp $
 */

#ifndef _HB_TREE_H_
#define _HB_TREE_H_

#include "dict.h"

BEGIN_DECL

struct hb_tree;
typedef struct hb_tree hb_tree;

hb_tree *hb_tree_new __P((dict_cmp_func key_cmp, dict_del_func key_del,
						  dict_del_func dat_del));

int hb_tree_insert __P((hb_tree *tree, void *key, void *dat, int overwrite));
void *hb_tree_search __P((hb_tree *tree, const void *key));
int hb_tree_remove __P((hb_tree *tree, const void *key, int del));

struct hb_itor;
typedef struct hb_itor hb_itor;

hb_itor *hb_itor_new __P((hb_tree *tree));
void hb_itor_destroy __P((hb_itor *tree));

int hb_itor_valid __P((const hb_itor *itor));
int hb_itor_next __P((hb_itor *itor));
const void *hb_itor_key __P((const hb_itor *itor));
int hb_itor_remove __P((hb_itor *itor, int del));

END_DECL

#endif /* !_HB_TREE_H_ */
