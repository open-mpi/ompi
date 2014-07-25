/*
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_PMIX_BASE_FNS_H
#define MCA_PMIX_BASE_FNS_H

#include "opal_config.h"
#include "opal/util/error.h"
#include "opal/dss/dss_types.h"


BEGIN_C_DECLS

typedef int (*kvs_put_fn)(const char key[], const char value[]);
typedef int (*kvs_get_fn)(const char key[], char value [], int maxvalue);

int pmix_store_encoded(const char *key, const void *data,
        opal_data_type_t type, char** buffer, int* length);
int pmix_commit_packed( char* buffer_to_put, int data_to_put, int vallen, int* pack_key, kvs_put_fn fn);
int cache_keys_locally(opal_identifier_t* id, const char* key, opal_value_t *out_kv, char* kvs_name, int vallen, kvs_get_fn fn);
int pmix_get_packed(opal_identifier_t* proc, char **packed_data, size_t *len, int vallen, kvs_get_fn fn);

END_C_DECLS

#endif
