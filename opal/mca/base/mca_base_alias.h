/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2020      Google, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_MCA_BASE_ALIAS_H
#define OPAL_MCA_BASE_ALIAS_H

#include "opal_config.h"
#include "opal/class/opal_list.h"

BEGIN_C_DECLS

enum mca_base_alias_flags_t {
    MCA_BASE_ALIAS_FLAG_NONE = 0,
    /** The aliased name has been deprecated. */
    MCA_BASE_ALIAS_FLAG_DEPRECATED = 1,
};

typedef enum mca_base_alias_flags_t mca_base_alias_flags_t;

struct mca_base_alias_item_t {
    opal_list_item_t super;
    /** Name aias. */
    char *component_alias;
    /** Alias flags. */
    uint32_t alias_flags;
};

typedef struct mca_base_alias_item_t mca_base_alias_item_t;

OBJ_CLASS_DECLARATION(mca_base_alias_item_t);

struct mca_base_alias_t {
    opal_object_t super;
    /** List of name aliases. */
    opal_list_t component_aliases;
};

typedef struct mca_base_alias_t mca_base_alias_t;

OBJ_CLASS_DECLARATION(mca_base_alias_t);

/**
 * @brief Create a alias for a component name.
 *
 * @param[in] project         Project name (may be NULL)
 * @param[in] framework       Framework name (may be NULL)
 * @param[in] component_name  Name of component to alias (may not be NULL)
 * @param[in] component_alias Aliased name (may not be NULL)
 * @param[in] alias_flags     Flags (see mca_base_alias_flags_t)
 *
 * This function aliases one component name to another. When aliased
 * any variable registered with this project, framework, and
 * component_name will have synonyms created. For example, if
 * opal_btl_vader is aliased to sm then registers a variable
 * named btl_vader_flags then a synonym will be created with the
 * name btl_sm_flags. If an alias is registered early enough
 * (during framework registration for example) then the alias can
 * also be used for component selection. In the previous example
 * --mca btl vader and --mca btl sm would select the same
 * component if the synonym is registered in the btl framework
 * registration function.
 */
OPAL_DECLSPEC int mca_base_alias_register(const char *project, const char *framework,
                                          const char *component_name, const char *component_alias,
                                          uint32_t alias_flags);

/**
 * @brief Check for aliases for a component.
 *
 * @param[in] project        Project name (may be NULL)
 * @param[in] frameworek     Framework name (may be NULL)
 * @param[in] component_name Component name (may not be NULL)
 */
OPAL_DECLSPEC const mca_base_alias_t *
mca_base_alias_lookup(const char *project, const char *framework, const char *component_name);

#endif /* OPAL_MCA_BASE_ALIAS_H */
