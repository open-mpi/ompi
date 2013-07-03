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
 * Copyright (c) 2008-2011 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC. All rights
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
OBJ_CLASS_INSTANCE(mca_base_var_enum_t, opal_object_t,
                   mca_base_var_enum_constructor,
                   mca_base_var_enum_destructor);

int mca_base_var_enum_create (char *name, mca_base_var_enum_value_t *values, mca_base_var_enum_t **enumerator)
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

    new_enum->enum_values = values;

    for (i = 0 ; new_enum->enum_values[i].string ; ++i);
    new_enum->enum_value_count = i;

    *enumerator = new_enum;

    return OPAL_SUCCESS;
}

static int enum_dump (mca_base_var_enum_t *self, char **out)
{
    size_t len, i;
    char *tmp;
    int ret;

    *out = NULL;

    if (NULL == self) {
        return OPAL_ERROR;
    }

    tmp = NULL;
    for (i = 0, len = 0 ; self->enum_values[i].string ; ++i) {
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
}
