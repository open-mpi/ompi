/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2024-2025 Google, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <regex.h>

#include "opal_config.h"

#include "btl_uct_include_list.h"
#include "btl_uct_types.h"
#include "opal/class/opal_object.h"
#include "opal/mca/btl/base/btl_base_error.h"
#include "opal/util/argv.h"

void mca_btl_uct_include_list_parse (const char *value, mca_btl_uct_include_list_t *list) {
    list->list = NULL;
    list->include = true;

    if (value == NULL) {
        return;
    }

    if (value[0] == '^') {
        list->include = false;
        value++;
    }

    list->list = opal_argv_split(value, ',');
}

int mca_btl_uct_include_list_rank (const char *name, const mca_btl_uct_include_list_t *list) {
    if (list->list == NULL) {
        return -1;
    }

    for (int i = 0; list->list[i]; ++i) {
        regex_t preg;
 
        BTL_VERBOSE(("evaluating %s vs %s-list item %s", name, list->include ? "include" : "exclude", list->list[i]));
        int rc = regcomp(&preg, list->list[i], REG_ICASE);
        if (0 != rc) {
            char errbuf[256];
            regerror(rc, &preg, errbuf, sizeof(errbuf));
            BTL_ERROR(("when matching name, could not parse regular expression: %s, error: %s", list->list[i], errbuf));
            continue;
        }

        int result = regexec(&preg, name, /*nmatch=*/0, /*pmatch=*/NULL, /*eflags=*/0);
        regfree(&preg);
        if (0 == result) {
            return list->include ? i + 1 : -(i + 1);
        }
    }

    return list->include ? -1 : 1;
}

static void mca_btl_uct_include_list_construct (mca_btl_uct_include_list_t *list)
{
    list->list = NULL;
}

static void mca_btl_uct_include_list_destruct (mca_btl_uct_include_list_t *list)
{
    opal_argv_free (list->list);
    list->list = NULL;
}

OBJ_CLASS_INSTANCE(mca_btl_uct_include_list_t, opal_object_t, mca_btl_uct_include_list_construct,
                   mca_btl_uct_include_list_destruct);


