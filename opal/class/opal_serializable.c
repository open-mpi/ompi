/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2024      Google, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <regex.h>

#include "opal_config.h"

#include "opal/class/opal_include_list.h"
#include "opal/class/opal_object.h"
#include "opal/include/opal/constants.h"
#include "opal/mca/base/base.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"

static int opal_serializable_deserialize(opal_serializable_t *object, const char *value)
{
    (void)object;
    (void)value;
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static char *opal_serializable_serialize(const opal_serializable_t *object)
{
    (void)object;
    return NULL;
}

static bool opal_serializable_is_set(const opal_serializable_t *object)
{
    (void)object;
    return false;
}

static void opal_serializable_constructor(opal_serializable_t *p)
{
    p->deserialize = opal_serializable_deserialize;
    p->serialize = opal_serializable_serialize;
    p->is_set = opal_serializable_is_set;
}

static void opal_serializable_destructor(opal_serializable_t *p)
{
}

OBJ_CLASS_INSTANCE(opal_serializable_t, opal_object_t, opal_serializable_constructor,
                   opal_serializable_destructor);
