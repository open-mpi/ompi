/*
 * Copyright (c) 2015-2017 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * Copyright (c) 2016-2017 Mellanox Technologies, Inc.
 *                         All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "gds_dstore.h"

#define ESH_KEY_SIZE(key, size)                             \
__extension__ ({                                            \
    size_t len = strlen(key) + 1 + sizeof(size_t) + size;   \
    len;                                                    \
})

/* in ext slot new offset will be stored in case if
 * new data were added for the same process during
 * next commit
 */
#define EXT_SLOT_SIZE()                                     \
    (ESH_KEY_SIZE(ESH_REGION_EXTENSION, sizeof(size_t)))

#define ESH_KV_SIZE(addr)                                   \
__extension__ ({                                            \
    size_t sz;                                              \
    memcpy(&sz, addr +                                      \
        ESH_KNAME_LEN(ESH_KNAME_PTR(addr)),                 \
        sizeof(size_t));                                    \
    sz += ESH_KNAME_LEN(ESH_KNAME_PTR(addr)) +              \
        sizeof(size_t);                                     \
    sz;                                                     \
})

#define ESH_KNAME_PTR(addr)                                 \
__extension__ ({                                            \
    char *name_ptr = (char *)addr;                          \
    name_ptr;                                               \
})

#define ESH_KNAME_LEN(key)                                  \
__extension__ ({                                            \
    size_t len = strlen((char*)key) + 1;                    \
    len;                                                    \
})

#define ESH_DATA_PTR(addr)                                  \
__extension__ ({                                            \
    uint8_t *data_ptr =                                     \
        addr +                                              \
        sizeof(size_t) +                                    \
        ESH_KNAME_LEN(ESH_KNAME_PTR(addr));                 \
    data_ptr;                                               \
})

#define ESH_DATA_SIZE(addr)                                 \
__extension__ ({                                            \
    size_t data_size;                                       \
    memcpy(&data_size,                                      \
        addr + ESH_KNAME_LEN(ESH_KNAME_PTR(addr)),          \
        sizeof(size_t));                                    \
    data_size;                                              \
})

#define ESH_PUT_KEY(addr, key, buffer, size)                \
__extension__ ({                                            \
    size_t sz = size;                                       \
    memset(addr, 0, ESH_KNAME_LEN(key));                    \
    strncpy((char *)addr, key, ESH_KNAME_LEN(key));         \
    memcpy(addr + ESH_KNAME_LEN(key), &sz,                  \
        sizeof(size_t));                                    \
    memcpy(addr + ESH_KNAME_LEN(key) + sizeof(size_t),      \
            buffer, size);                                  \
})

static size_t _ds12_kv_size(uint8_t *addr);
static size_t _ds12_key_size(const char *key, size_t data_size);
static char* _ds12_key_name_ptr(uint8_t *addr);
static size_t _ds12_key_len(const char *key);
static uint8_t* _ds12_data_ptr(uint8_t *addr);
static size_t _ds12_data_size(uint8_t *addr, uint8_t *data_ptr);
static size_t _ds12_slot_size(void);
static void _ds12_put_key(uint8_t *data, char *key, void *buf, size_t size);

dstore_mod_t dstore_v12_module = {
    "ds12",
    _ds12_kv_size,
    _ds12_key_size,
    _ds12_key_name_ptr,
    _ds12_key_len,
    _ds12_data_ptr,
    _ds12_data_size,
    _ds12_slot_size,
    _ds12_put_key
};

static size_t _ds12_kv_size(uint8_t *addr) {
    return ESH_KV_SIZE(addr);
}

static size_t _ds12_key_size(const char *key, size_t data_size) {
    return ESH_KEY_SIZE(key, data_size);
}

static char* _ds12_key_name_ptr(uint8_t *addr) {
    return ESH_KNAME_PTR(addr);
}

static size_t _ds12_key_len(const char *key) {
    return ESH_KNAME_LEN(key);
}

static uint8_t* _ds12_data_ptr(uint8_t *addr) {
    return ESH_DATA_PTR(addr);
}

static size_t _ds12_data_size(uint8_t *addr, uint8_t *data_ptr) {
    return ESH_DATA_SIZE(addr);
}

static size_t _ds12_slot_size(void) {
    return EXT_SLOT_SIZE();
}

static void _ds12_put_key(uint8_t *data, char *key, void *buf, size_t size) {
    ESH_PUT_KEY(data, key, buf, size);
}
