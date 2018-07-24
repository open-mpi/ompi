/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2018      Los Alamos National Security, LLC. All rights
 *                         reserved.
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#if !defined(MCA_BASE_SOURCE_H)
#define MCA_BASE_SOURCE_H

#include "opal/datatype/opal_datatype.h"
#include "mca_base_pvar.h"

enum {
    /* default source */
    MCA_BASE_SOURCE_DEFAULT_SOURCE,
};

typedef double (*mca_base_source_time_fn_t) (void);

typedef struct mca_base_source_t {
    /** Make this an opal object */
    opal_object_t super;

    /** Source index */
    int source_index;

    /** Source ordering */
    bool source_ordered;

    /** Full name of the variable: form is framework_component_name */
    char *source_name;

    /** Description of this performance variable */
    char *source_description;

    /** Time source (never NULL) */
    mca_base_source_time_fn_t source_time;
} mca_base_source_t;

OBJ_CLASS_DECLARATION(mca_base_source_t);

extern int mca_base_source_default_source;

OPAL_DECLSPEC int mca_base_source_init (void);
OPAL_DECLSPEC int mca_base_source_finalize (void);

OPAL_DECLSPEC int mca_base_source_get_count (int *count);

OPAL_DECLSPEC int mca_base_source_dump(int index, char ***out, mca_base_var_dump_type_t output_type);

OPAL_DECLSPEC int mca_base_source_register (const char *project, const char *framework, const char *component, const char *name,
                                            const char *description, bool ordered, mca_base_source_time_fn_t source_time);

OPAL_DECLSPEC int mca_base_component_source_register (const mca_base_component_t *component, const char *name, const char *description,
                                                      bool ordered, mca_base_source_time_fn_t source_time);

OPAL_DECLSPEC int mca_base_source_set_time_source (int source_index, mca_base_source_time_fn_t time_source);

OPAL_DECLSPEC mca_base_source_t *mca_base_source_get (int source_index);

#endif /* !defined(MCA_BASE_SOURCE_H) */
