/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "opal_config.h"


#include "opal/constants.h"
#include "opal/util/show_help.h"
#include "opal/util/output.h"
#include "opal/mca/pool/base/base.h"
#include "opal/mca/pool/pool.h"
#include "pool_strings.h"

const char *opal_pool_strings_component_version_string =
    "OPAL pool strings MCA component version " OPAL_VERSION;

/* local functions */
static int strings_query(int *priority);
static int strings_register (void);
static int strings_open(void);
static int strings_close(void);
static int initialized = 0;
static int total = 0;
static int created = 0;

// local object
typedef struct {
    opal_list_item_t super;
    char *str;
} opal_string_t;

static void opal_string_construct(opal_string_t *);
static void opal_string_destruct(opal_string_t *);

static opal_list_t pool_head;

static OBJ_CLASS_INSTANCE(opal_string_t,
                          opal_list_item_t,
                          opal_string_construct, opal_string_destruct);

static void opal_string_construct(opal_string_t *str)
{
    str->str = NULL;
}

static void opal_string_destruct(opal_string_t *str)
{
    if (NULL != str->str)
    {
        opal_list_remove_item(&pool_head, &str->super);
        free(str->str);
    }
}

static char * strings_get (void *handle)
{
    assert(initialized);
    if (NULL == handle) {
        return NULL;
    }
    return ((opal_string_t *)handle)->str;
}

static void * strings_put (char * s)
{
    opal_string_t *str;
    assert(initialized);
    if (NULL == s) {
        return NULL;
    }
    total++;
    OPAL_LIST_FOREACH(str, &pool_head, opal_string_t) {
        if (0 == strcmp(str->str, s)) {
            OBJ_RETAIN(str);
            free(s);
            return str;
        }
    }
    str = OBJ_NEW(opal_string_t);
    created++;
    opal_list_append(&pool_head, &str->super);
    str->str = s;
    return str;
}

static void strings_free (void *handle)
{
    opal_string_t *str = (opal_string_t *)handle;
    assert(initialized);
    OBJ_RELEASE(str);
}

opal_pool_strings_component_t mca_pool_strings_component = {
    .super = {
        .base_version = {
            OPAL_POOL_BASE_VERSION_2_0_0,

            /* Component name and version */
            .mca_component_name = "strings",
            MCA_BASE_MAKE_VERSION(component, OPAL_MAJOR_VERSION, OPAL_MINOR_VERSION,
                                  OPAL_RELEASE_VERSION),

            /* Component open and close functions */
            .mca_open_component = strings_open,
            .mca_close_component = strings_close,
            .mca_register_component_params = strings_register,
        },
        .data = {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        /* Memory framework functions. */
        .query = strings_query,
        .get = strings_get,
        .put = strings_put,
        .free = strings_free,
    },
};

static int
strings_register(void)
{
    mca_pool_strings_component.priority = 40;
    (void) mca_base_component_var_register(&mca_pool_strings_component.super.base_version,
                                           "priority", "Priority for the pool strings "
                                           "component (default: 40)", MCA_BASE_VAR_TYPE_INT,
                                           NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_ALL_EQ,
                                           &mca_pool_strings_component.priority);
    mca_pool_strings_component.stats = false;
    (void) mca_base_component_var_register(&mca_pool_strings_component.super.base_version,
                                           "stats", "Stats for the pool strings "
                                           "component (default: false)", MCA_BASE_VAR_TYPE_BOOL,
                                           NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_ALL_EQ,
                                           &mca_pool_strings_component.stats);
    return OPAL_SUCCESS;
}

static int
strings_open(void)
{
    OBJ_CONSTRUCT(&pool_head, opal_list_t);
    return OPAL_SUCCESS;
}

static int
strings_close(void)
{
    opal_string_t * str, * next;
    if (mca_pool_strings_component.stats)
        printf ( "There were up to %d / %d strings in the pool\n", created, total);
    OPAL_LIST_FOREACH_SAFE(str, next, &pool_head, opal_string_t) {
        OBJ_RELEASE(str);
    }
    OBJ_DESTRUCT(&pool_head);
    return OPAL_SUCCESS;
}

static int
strings_query(int *priority)
{
    *priority = mca_pool_strings_component.priority;
    initialized = 1;
    return OPAL_SUCCESS;
}

