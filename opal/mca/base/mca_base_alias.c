/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2020      Google, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "mca_base_alias.h"
#include "opal/class/opal_hash_table.h"
#include "opal/runtime/opal.h"

static void mca_base_alias_init(mca_base_alias_t *alias)
{
    OBJ_CONSTRUCT(&alias->component_aliases, opal_list_t);
}

static void mca_base_alias_fini(mca_base_alias_t *alias)
{
    OPAL_LIST_DESTRUCT(&alias->component_aliases);
}

OBJ_CLASS_INSTANCE(mca_base_alias_t, opal_object_t, mca_base_alias_init, mca_base_alias_fini);

static void mca_base_alias_item_init(mca_base_alias_item_t *alias_item)
{
    alias_item->component_alias = NULL;
}

static void mca_base_alias_item_fini(mca_base_alias_item_t *alias_item)
{
    free(alias_item->component_alias);
}

OBJ_CLASS_INSTANCE(mca_base_alias_item_t, opal_list_item_t, mca_base_alias_item_init,
                   mca_base_alias_item_fini);

/*
 * local variables
 */
static opal_hash_table_t *alias_hash_table;

static void mca_base_alias_cleanup(void)
{
    if (!alias_hash_table) {
        return;
    }

    void *key;
    opal_object_t *value;
    OPAL_HASH_TABLE_FOREACH_PTR (key, value, alias_hash_table, { OBJ_RELEASE(value); })
        ;

    OBJ_RELEASE(alias_hash_table);
    alias_hash_table = NULL;
}

static int mca_base_alias_setup(void)
{
    if (NULL != alias_hash_table) {
        return OPAL_SUCCESS;
    }

    opal_finalize_register_cleanup(mca_base_alias_cleanup);

    alias_hash_table = OBJ_NEW(opal_hash_table_t);
    if (NULL == alias_hash_table) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    int ret = opal_hash_table_init(alias_hash_table, 32);
    if (OPAL_SUCCESS != ret) {
        OBJ_RELEASE(alias_hash_table);
        alias_hash_table = NULL;
        return ret;
    }

    return OPAL_SUCCESS;
}

static char *mca_base_alias_generate_name(const char *project, const char *framework,
                                          const char *component_name)
{
    size_t project_length = project ? strlen(project) : 0;
    size_t framework_length = framework ? strlen(framework) : 0;
    size_t component_name_length = strlen(component_name);
    size_t length = project_length + framework_length + component_name_length + 2;
    char *tmp = calloc(1, length + 1);
    if (NULL == tmp) {
        return tmp;
    }

    if (project_length) {
        strncat(tmp, project, length);
        // NOTE: We use strcat() here (and not strncat()) because
        // we're appending a constant string with a fixed/hard-coded
        // length.  It's the exact equivalent of strncat(tmp, "_", 1),
        // but strncat() would actually be more overhead.  Indeed, GCC
        // 10 emits a warning if we use strncatd() with a compile-time
        // constant string as the source and a hard-coded length that
        // is equivalent to the length of that compile-time constant
        // string.  So avoid the warning and use strcat().
        strcat(tmp, "_");
        length -= project_length + 1;
    }

    if (framework_length) {
        strncat(tmp, framework, length);
        // Use strcat() here instead of strncat(); see the comment
        // above for an explanation.
        strcat(tmp, "_");
        length -= framework_length + 1;
    }

    strncat(tmp, component_name, length);

    return tmp;
}

static mca_base_alias_t *mca_base_alias_lookup_internal(const char *name)
{
    mca_base_alias_t *alias = NULL;
    if (NULL == alias_hash_table) {
        return NULL;
    }

    (void) opal_hash_table_get_value_ptr(alias_hash_table, name, strlen(name), (void **) &alias);
    return alias;
}

int mca_base_alias_register(const char *project, const char *framework, const char *component_name,
                            const char *component_alias, uint32_t alias_flags)
{
    if (NULL == component_name) {
        return OPAL_ERR_BAD_PARAM;
    }

    int ret = mca_base_alias_setup();
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    char *name = mca_base_alias_generate_name(project, framework, component_name);
    assert(NULL != name);

    mca_base_alias_t *alias = mca_base_alias_lookup_internal(name);
    if (NULL == alias) {
        alias = OBJ_NEW(mca_base_alias_t);
        if (NULL == alias) {
            free(name);
            return OPAL_ERR_OUT_OF_RESOURCE;
        }

        opal_hash_table_set_value_ptr(alias_hash_table, name, strlen(name), alias);
        free(name);
        name = NULL;
    }

    mca_base_alias_item_t *alias_item = OBJ_NEW(mca_base_alias_item_t);
    if (NULL == alias_item) {
        free(name);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    alias_item->component_alias = strdup(component_alias);
    alias_item->alias_flags = alias_flags;

    opal_list_append(&alias->component_aliases, &alias_item->super);

    free(name);
    return OPAL_SUCCESS;
}

const mca_base_alias_t *mca_base_alias_lookup(const char *project, const char *framework,
                                              const char *component_name)
{
    if (NULL == component_name) {
        return NULL;
    }

    char *name = mca_base_alias_generate_name(project, framework, component_name);
    assert(NULL != name);
    const mca_base_alias_t *alias = mca_base_alias_lookup_internal(name);
    free(name);

    return alias;
}
