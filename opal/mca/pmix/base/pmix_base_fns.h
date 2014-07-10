#include "opal/util/error.h"
#include "opal/dss/dss_types.h"
int pmi_commit_packed(opal_identifier_t* proc, int pmix_vallen_max, char* pmix_kvs_name);
int pmi_store_encoded(opal_identifier_t* id, const char *key, const void *data,
        opal_data_type_t type);
int pmi_get_packed(opal_identifier_t* proc,
        char **packed_data, size_t *len, int pmix_keylen_max);
