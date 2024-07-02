/*
 * Copyright (c) 2024      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal/util/json/opal_json.h"
#include "opal/constants.h"
#include "opal/util/json/3rd-party/json.h"
#include "opal/util/show_help.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define CHECK_OBJ_TYPE(obj, expected)                                                           \
    do {                                                                                        \
        if ((obj)->type != (expected)) {                                                        \
            opal_show_help("help-json.txt", "Invalid argument type", true, __func__, #expected, \
                           expected, (obj)->type);                                              \
            return OPAL_ERROR;                                                                  \
        }                                                                                       \
    } while (0)

struct opal_json_internal_t {
    struct opal_json_t parent;
    json_value *value;
};
typedef struct opal_json_internal_t opal_json_internal_t;

static void opal_json_internal_free(opal_json_internal_t *json)
{
    if (NULL == json) {
        return;
    }

    /* The root JSON object will release the memory of its children */
    if (json->value && NULL == json->value->parent) {
        json_value_free(json->value);
    }

    free(json);
}

static inline int opal_json_internal_translate_type(json_type type, opal_json_type *out)
{
    int ret = OPAL_SUCCESS;
    switch (type) {
    case json_null:
        *out = OPAL_JSON_NULL;
        break;
    case json_array:
        *out = OPAL_JSON_ARRAY;
        break;
    case json_boolean:
        *out = OPAL_JSON_BOOL;
        break;
    case json_integer:
        *out = OPAL_JSON_INTEGER;
        break;
    case json_double:
        *out = OPAL_JSON_DOUBLE;
        break;
    case json_string:
        *out = OPAL_JSON_STRING;
        break;
    case json_object:
        *out = OPAL_JSON_OBJECT;
        break;
    default:
        opal_show_help("help-json.txt", "Unknown JSON value type", true, type);
        *out = OPAL_JSON_TYPE_COUNT;
        ret = OPAL_ERROR;
    }

    return ret;
}

static int opal_json_internal_new(const json_value *in, opal_json_internal_t **out)
{
    *out = malloc(sizeof(opal_json_internal_t));
    if (NULL == *out) {
        return OPAL_ERROR;
    }

    (*out)->value = (json_value *) in;
    return opal_json_internal_translate_type(in->type, &(*out)->parent.type);
}

int opal_json_load(const char *str, const size_t len, const opal_json_t **json)
{
    opal_json_internal_t *out = NULL;
    int ret = OPAL_SUCCESS;

    json_value *value = json_parse(str, len);
    if (!value) {
        opal_show_help("help-json.txt", "Invalid JSON string", true);
        ret = OPAL_ERROR;
        goto out;
    }

    ret = opal_json_internal_new(value, &out);

out:
    if (OPAL_SUCCESS == ret) {
        *json = (opal_json_t *) out;
    } else if (out) {
        opal_json_internal_free(out);
    }

    return ret;
}

int opal_json_load_file(const char *filename, const opal_json_t **json)
{
    FILE *fp = NULL;
    size_t file_size;
    char *file_contents = NULL;
    int ret = OPAL_SUCCESS;

    fp = fopen(filename, "r");
    if (fp == NULL) {
        opal_show_help("help-json.txt", "Unable to open file", true, filename);
        ret = OPAL_ERROR;
        goto out;
    }

    (void) fseek(fp, 0L, SEEK_END);
    file_size = ftell(fp);
    rewind(fp);

    file_contents = (char *) malloc(file_size);
    if (!file_contents) {
        opal_show_help("help-json.txt", "Memory allocation failure", true);
        ret = OPAL_ERROR;
        goto out;
    }

    if (file_size > fread(file_contents, 1, file_size, fp)) {
        opal_show_help("help-json.txt", "Unable to read file", true, filename);
        ret = OPAL_ERROR;
        goto out;
    }

    ret = opal_json_load(file_contents, file_size, json);

out:
    if (fp) {
        fclose(fp);
    }
    if (file_contents) {
        free(file_contents);
    }

    return ret;
}

int opal_json_get_key(const opal_json_t *json, const char *key, const opal_json_t **out)
{
    int ret = OPAL_ERROR;

    CHECK_OBJ_TYPE(json, OPAL_JSON_OBJECT);

    opal_json_internal_t *in = (opal_json_internal_t *) json;
    opal_json_internal_t *result = NULL;

    json_object_entry entry = {0};

    for (unsigned int i = 0; i < in->value->u.object.length; ++i) {
        entry = in->value->u.object.values[i];
        if (0 == strcmp(entry.name, key)) {
            ret = opal_json_internal_new(entry.value, &result);
            break;
        }
    }

    if (OPAL_SUCCESS == ret) {
        *out = (opal_json_t *) result;
    } else if (result) {
        opal_json_internal_free(result);
    }

    return ret;
}

void opal_json_free(const opal_json_t **json)
{
    opal_json_internal_free((struct opal_json_internal_t *) *json);
}

int opal_json_get_index(const opal_json_t *json, const size_t index, const opal_json_t **out)
{
    int ret = OPAL_ERROR;

    CHECK_OBJ_TYPE(json, OPAL_JSON_ARRAY);

    opal_json_internal_t *in = (opal_json_internal_t *) json;
    opal_json_internal_t *result = NULL;

    if ((size_t) in->value->u.array.length > index) {
        ret = opal_json_internal_new(in->value->u.array.values[index], &result);
    } else {
        opal_show_help("help-json.txt", "Index out of bound", true, index,
                       in->value->u.array.length);
    }

    if (OPAL_SUCCESS == ret) {
        *out = (opal_json_t *) result;
    } else if (result) {
        opal_json_internal_free(result);
    }

    return ret;
}

int opal_json_get_container_size(const opal_json_t *json, size_t *len)
{
    int ret = OPAL_ERROR;
    if (OPAL_JSON_OBJECT == json->type) {
        *len = (size_t) ((opal_json_internal_t *) json)->value->u.object.length;
        ret = OPAL_SUCCESS;
    } else if (OPAL_JSON_ARRAY == json->type) {
        *len = (size_t) ((opal_json_internal_t *) json)->value->u.array.length;
        ret = OPAL_SUCCESS;
    }
    return ret;
}

int opal_json_read_integer(const opal_json_t *json, int64_t *out)
{
    CHECK_OBJ_TYPE(json, OPAL_JSON_INTEGER);
    opal_json_internal_t *in = (opal_json_internal_t *) json;
    *out = in->value->u.integer;
    return OPAL_SUCCESS;
}

int opal_json_read_double(const opal_json_t *json, double *out)
{
    CHECK_OBJ_TYPE(json, OPAL_JSON_DOUBLE);
    opal_json_internal_t *in = (opal_json_internal_t *) json;
    *out = in->value->u.dbl;
    return OPAL_SUCCESS;
}

int opal_json_read_bool(const opal_json_t *json, bool *out)
{
    CHECK_OBJ_TYPE(json, OPAL_JSON_BOOL);
    opal_json_internal_t *in = (opal_json_internal_t *) json;
    *out = in->value->u.boolean;
    return OPAL_SUCCESS;
}

int opal_json_read_string(const opal_json_t *json, const char **out, size_t *len)
{
    CHECK_OBJ_TYPE(json, OPAL_JSON_STRING);
    opal_json_internal_t *in = (opal_json_internal_t *) json;
    *out = in->value->u.string.ptr;
    *len = in->value->u.string.length;
    return OPAL_SUCCESS;
}
