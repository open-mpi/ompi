/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2024      Google, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#if !defined(OPAL_SERIALIZABLE_H)
#define OPAL_SERIALIZABLE_H

#include "opal_config.h"
#include "opal/class/opal_object.h"

struct opal_serializable;
typedef struct opal_serializable opal_serializable_t;

typedef int (*opal_serializable_deserialize_fn_t)(opal_serializable_t *object, const char *value);
typedef char *(*opal_serializable_serialize_fn_t)(const opal_serializable_t *object);
typedef bool (*opal_serializable_is_set_fn_t)(const opal_serializable_t *object);

struct opal_serializable {
    opal_object_t super;
    /** de-serialize the object from a string */
    opal_serializable_deserialize_fn_t deserialize;
    /** serialize the object into a string */
    opal_serializable_serialize_fn_t serialize;
    /** object has a value set */
    opal_serializable_is_set_fn_t is_set;
};

OPAL_DECLSPEC OBJ_CLASS_DECLARATION(opal_serializable_t);

#endif /* OPAL_SERIALIZABLE_H */
