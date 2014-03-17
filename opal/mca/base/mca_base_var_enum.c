/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2012 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012-2014 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/mca/base/mca_base_var_enum.h"

#include <stdio.h>
#include <string.h>
#include <errno.h>

static void mca_base_var_enum_constructor (mca_base_var_enum_t *enumerator);
static void mca_base_var_enum_destructor (mca_base_var_enum_t *enumerator);
OBJ_CLASS_INSTANCE(mca_base_var_enum_t, opal_object_t, mca_base_var_enum_constructor,
                   mca_base_var_enum_destructor);

static int mca_base_var_enum_bool_get_count (mca_base_var_enum_t *enumerator, int *count)
{
    *count = 2;
    return OPAL_SUCCESS;
}

static int mca_base_var_enum_bool_get_value (mca_base_var_enum_t *self, int index,
                                             int *value, const char **string_value)
{
    if (1 < index) {
        return OPAL_ERR_VALUE_OUT_OF_BOUNDS;
    }

    *value = index ? 1 : 0;
    *string_value = index ? "true" : "false";

    return OPAL_SUCCESS;
}

static int mca_base_var_enum_bool_vfs (mca_base_var_enum_t *self, const char *string_value,
                                       int *value)
{
    char *tmp;
    int v;

    v = strtol (string_value, &tmp, 10);
    if (*tmp != '\0') {
        if (0 == strcmp (string_value, "true") || 0 == strcmp (string_value, "t") ||
            0 == strcmp (string_value, "enabled")) {
            v = 1;
        } else if (0 == strcmp (string_value, "false") || 0 == strcmp (string_value, "f") ||
                   0 == strcmp (string_value, "disabled")) {
            v = 0;
        } else {
            return OPAL_ERR_VALUE_OUT_OF_BOUNDS;
        }
    }

    *value = !!v;

    return OPAL_SUCCESS;
}

static int mca_base_var_enum_bool_sfv (mca_base_var_enum_t *self, const int value,
                                       const char **string_value)
{
    if (string_value) {
        *string_value = value ? "true" : "false";
    }

    return OPAL_SUCCESS;
}

static int mca_base_var_enum_bool_dump (mca_base_var_enum_t *self, char **out)
{
    *out = strdup ("0: f|false|disabled, 1: t|true|enabled");
    return *out ? OPAL_SUCCESS : OPAL_ERR_OUT_OF_RESOURCE;
}

mca_base_var_enum_t mca_base_var_enum_bool = {
    .super     = OPAL_OBJ_STATIC_INIT(opal_object_t),
    .enum_name = "boolean",
    .get_count = mca_base_var_enum_bool_get_count,
    .get_value = mca_base_var_enum_bool_get_value,
    .value_from_string = mca_base_var_enum_bool_vfs,
    .string_from_value = mca_base_var_enum_bool_sfv,
    .dump      = mca_base_var_enum_bool_dump
};


int mca_base_var_enum_create (const char *name, const mca_base_var_enum_value_t *values, mca_base_var_enum_t **enumerator)
{
    mca_base_var_enum_t *new_enum;
    int i;

    *enumerator = NULL;

    new_enum = OBJ_NEW(mca_base_var_enum_t);
    if (NULL == new_enum) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    new_enum->enum_name = strdup (name);
    if (NULL == new_enum->enum_name) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    for (i = 0 ; values[i].string ; ++i);
    new_enum->enum_value_count = i;

    /* make a copy of the values */
    new_enum->enum_values = calloc (new_enum->enum_value_count + 1, sizeof (*new_enum->enum_values));
    if (NULL == new_enum->enum_values) {
        OBJ_DESTRUCT(new_enum);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    for (i = 0 ; i < new_enum->enum_value_count ; ++i) {
        new_enum->enum_values[i].value = values[i].value;
        new_enum->enum_values[i].string = strdup (values[i].string);
    }

    *enumerator = new_enum;

    return OPAL_SUCCESS;
}

static int enum_dump (mca_base_var_enum_t *self, char **out)
{
    int i;
    char *tmp;
    int ret;

    *out = NULL;

    if (NULL == self) {
        return OPAL_ERROR;
    }

    tmp = NULL;
    for (i = 0; i < self->enum_value_count && self->enum_values[i].string ; ++i) {
        ret = asprintf (out, "%s%s%d:\"%s\"", tmp ? tmp : "", tmp ? ", " : "", self->enum_values[i].value,
                        self->enum_values[i].string);
        if (tmp) free (tmp);
        if (0 > ret) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
        tmp = *out;
    }

    return OPAL_SUCCESS;
}

static int enum_get_count (mca_base_var_enum_t *self, int *count)
{
    *count = self->enum_value_count;
    return OPAL_SUCCESS;
}

static int enum_get_value (mca_base_var_enum_t *self, int index, int *value, const char **string_value)
{
    int count, ret;

    ret = self->get_count(self, &count);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    if (index >= count) {
        return OPAL_ERR_VALUE_OUT_OF_BOUNDS;
    }

    if (value) {
        *value = self->enum_values[index].value;
    }

    if (string_value) {
        *string_value = self->enum_values[index].string;
    }

    return OPAL_SUCCESS;
}

static int enum_value_from_string(mca_base_var_enum_t *self, const char *string_value, int *value_out) {
    int value, count, ret, i;
    bool is_int;
    char *tmp;

    ret = self->get_count(self, &count);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    value = strtol(string_value, &tmp, 0);

    /* Check if the string is an integer */
    is_int = tmp[0] == '\0';

    for (i = 0 ; i < count ; ++i) {
        if ((is_int && value == self->enum_values[i].value) ||
            0 == strcasecmp (string_value, self->enum_values[i].string)) {
            break;
        }
    }

    if (i == count) {
        return OPAL_ERR_VALUE_OUT_OF_BOUNDS;
    }

    *value_out = self->enum_values[i].value;

    return OPAL_SUCCESS;
}

static int enum_string_from_value(mca_base_var_enum_t *self, const int value, const char **string_value) {
    int count, ret, i;

    ret = self->get_count(self, &count);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    for (i = 0 ; i < count ; ++i) {
        if (value == self->enum_values[i].value) {
            break;
        }
    }

    if (i == count) {
        return OPAL_ERR_VALUE_OUT_OF_BOUNDS;
    }

    if (string_value) {
        *string_value = self->enum_values[i].string;
    }

    return OPAL_SUCCESS;
}

static void mca_base_var_enum_constructor (mca_base_var_enum_t *enumerator)
{
    memset ((char *) enumerator + sizeof (enumerator->super), 0 , sizeof(*enumerator) - sizeof(enumerator->super));

    enumerator->get_value = enum_get_value;
    enumerator->get_count = enum_get_count;
    enumerator->value_from_string = enum_value_from_string;
    enumerator->string_from_value = enum_string_from_value;
    enumerator->dump      = enum_dump;
}

static void mca_base_var_enum_destructor (mca_base_var_enum_t *enumerator)
{
    if (enumerator->enum_name) {
        free (enumerator->enum_name);
    }

    /* release the copy of the values */
    if (enumerator->enum_values) {
        for (int i = 0 ; i < enumerator->enum_value_count ; ++i) {
            free ((void *) enumerator->enum_values[i].string);
        }
        free (enumerator->enum_values);
    }
}
