/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2024-2025 Google, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "btl_uct_types.h"

#if !defined(BTL_UCT_INCLUDE_LIST_H)
#define BTL_UCT_INCLUDE_LIST_H

/**
 * @brief Parse `value` to create an include list.
 *
 * @param[in]     value  Comma-delimeted string to parse.
 * @param[in,out] list   Include list object, must already be constructed.
 */
void mca_btl_uct_include_list_parse (const char *value, mca_btl_uct_include_list_t *list);

/**
 * @brief Find the rank of `name` in the include list `list`.
 *
 * @param[in] name   name to find
 * @param[in] list   list to search
 *
 * A negative result means the name is not present or the list is negated.
 */
int mca_btl_uct_include_list_rank (const char *name, const mca_btl_uct_include_list_t *list);

#endif /* !defined(BTL_UCT_INCLUDE_LIST_H) */
