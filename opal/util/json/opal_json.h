/*
 * Copyright (c) 2024      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** @file:
 *
 * Utility data structures and functions to parse JSON from a string or file.
 * Note: The internal 3rd-party code should NOT be directly used. Implement the
 * functionality here if necessary.
 *
 * Below is a trivial example to parse a JSON file with the content {"key": "val"}.

int ret = OPAL_SUCCESS;
const opal_json_t *json = NULL, *item = NULL;
char *val;
size_t len;

ret = opal_json_load_file("my_file.json", &json); // Parse the file content into json
if (OPAL_SUCCESS != ret) {
     goto out;
}

ret = opal_json_get_key(json, "key", &item); // Get the item pointed to by "key"
if (OPAL_SUCCESS != ret) {
     goto out;
}

ret = opal_json_read_string(item, (const char **) &val, &len); // Read the string value and length
if (OPAL_SUCCESS != ret) {
    goto out;
}

out:

// Cleanup
opal_json_free(&json);
opal_json_free(&item);

 */

#ifndef OPAL_JSON_H
#define OPAL_JSON_H

#include "opal/mca/mca.h"

BEGIN_C_DECLS

typedef enum {
    OPAL_JSON_NULL,
    OPAL_JSON_OBJECT,
    OPAL_JSON_ARRAY,
    OPAL_JSON_INTEGER,
    OPAL_JSON_DOUBLE,
    OPAL_JSON_BOOL,
    OPAL_JSON_STRING,
    OPAL_JSON_TYPE_COUNT,
} opal_json_type;

struct opal_json_t {
    opal_json_type type;
};
typedef struct opal_json_t opal_json_t;

/**
 * Load JSON from a string.
 *
 * @param[in]   str     A JSON string
 * @param[in]   len     First bytes to parse
 * @param[out]  json    Output JSON object if the string is valid
 *
 * @returns     OPAL_SUCCESS if the string is parsed successfully
 *              OPAL_ERROR otherwise
 */
OPAL_DECLSPEC int opal_json_load(const char *str, const size_t len, const opal_json_t **json);

/**
 * Load JSON from a file.
 *
 * @param[in]   filename    File name
 * @param[out]  json        Output JSON object if the file valid
 *
 * @returns     OPAL_SUCCESS if the file is read and parsed successfully
 *              OPAL_ERROR otherwise
 */
OPAL_DECLSPEC int opal_json_load_file(const char *filename, const opal_json_t **json);

/**
 * Free JSON resources
 *
 * @param[in]  json     Pointer to JSON object to free
 */
OPAL_DECLSPEC void opal_json_free(const opal_json_t **json);

/**
 * Get the JSON object with a key value from a parent object
 *
 * @param[in]   json    Parent JSON object
 * @param[in]   key     Key value
 * @param[out]  out     Output JSON object with the specified key value
 *
 * @returns     OPAL_SUCCESS if an object is found with the specified key value
 *              OPAL_ERROR otherwise
 */
OPAL_DECLSPEC int opal_json_get_key(const opal_json_t *json, const char *key,
                                    const opal_json_t **out);

/**
 * Get the JSON object at index from a JSON array
 *
 * @param[in]   json    Parent JSON array
 * @param[in]   index   Index value
 * @param[out]  out     Output JSON object at the specified index
 *
 * @returns     OPAL_SUCCESS if an object is found at the specified index
 *              OPAL_ERROR otherwise
 */
OPAL_DECLSPEC int opal_json_get_index(const opal_json_t *json, const size_t index,
                                      const opal_json_t **out);

/**
 * Get the number of objects in a container-type value, i.e. object, array.
 *
 * @param[in]   json    A container JSON object
 * @param[out]  len     Number of elements in the container
 *
 * @returns     OPAL_SUCCESS if successful
 *              OPAL_ERROR otherwise
 */
OPAL_DECLSPEC int opal_json_get_container_size(const opal_json_t *json, size_t *len);

/**
 * Value reader functions
 *
 * The caller is responsible for ensuring the function be called for the correct the object type,
 * i.e. *read_integer should only be called for integer objects.
 *
 * @returns     OPAL_SUCCESS if successful
 *              OPAL_ERROR otherwise
 */

OPAL_DECLSPEC int opal_json_read_bool(const opal_json_t *json, bool *out);
OPAL_DECLSPEC int opal_json_read_integer(const opal_json_t *json, int64_t *out);
OPAL_DECLSPEC int opal_json_read_double(const opal_json_t *json, double *out);
OPAL_DECLSPEC int opal_json_read_string(const opal_json_t *json, const char **out, size_t *len);

END_C_DECLS

#endif
