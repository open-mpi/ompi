/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2024      Google, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#if !defined(OPAL_INCLUDE_LIST_H)
#define OPAL_INCLUDE_LIST_H

#include "opal_config.h"
#include "opal/class/opal_object.h"
#include "opal/class/opal_serializable.h"

/**
 * An include list. These are strings of the form:
 *   foo,bar,baz    (include)
 *   ^foo,bar,baz   (exclude)
 */
struct opal_include_list {
    opal_serializable_t super;
    /** argv array of items */
    char **items;
    /** is this an exclude list */
    bool is_exclude;
};
typedef struct opal_include_list opal_include_list_t;

OPAL_DECLSPEC OBJ_CLASS_DECLARATION(opal_include_list_t);

/**
 * Match a string against the include list and return the list rank.
 *
 * @param[in]  include_list   Include list
 * @param[in]  value          Value to match
 * @param[in]  regex_match    Treat the entries in the include list as regular expressions
 * @param[in]  case_sensitive Make matching case sensitive
 *
 * This method searches the include list for value. If an entry matches then the rank of the
 * include list match is returned. A negative number is returned if the list is an exclude
 * list.
 */
OPAL_DECLSPEC int opal_include_list_match(opal_include_list_t *include_list, const char *value,
                                          bool regex_match, bool case_sensitive);


#endif /* OPAL_INCLUDE_LIST_H */
