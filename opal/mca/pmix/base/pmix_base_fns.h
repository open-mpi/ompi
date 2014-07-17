#include "opal/util/error.h"
#include "opal/dss/dss_types.h"
int pmi_store_encoded(const char *key, const void *data,
        opal_data_type_t type, char** buffer, int* length);
char* setup_key(opal_identifier_t* name, const char *key, int pmix_keylen_max);
char *pmi_encode(const void *val, size_t vallen);
uint8_t *pmi_decode (const char *data, size_t *retlen);

