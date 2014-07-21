#include "opal/util/error.h"
#include "opal/dss/dss_types.h"
typedef int (*kvs_put_fn)(const char key[], const char value[]);
int pmix_store_encoded(const char *key, const void *data,
        opal_data_type_t type, char** buffer, int* length);
int pmix_commit_packed( char* buffer_to_put, int data_to_put, int vallen, int* pack_key, kvs_put_fn fn);
char* setup_key(opal_identifier_t* name, const char *key, int pmix_keylen_max);
char *pmi_encode(const void *val, size_t vallen);
uint8_t *pmi_decode (const char *data, size_t *retlen);

