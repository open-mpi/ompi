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

#define ESH_KV_SIZE(addr)                                   \
__extension__ ({                                            \
    size_t sz;                                              \
    memcpy(&sz, addr, sizeof(size_t));                      \
    sz;                                                     \
})

#define ESH_KNAME_PTR(addr)                                 \
__extension__ ({                                            \
    char *name_ptr = (char *)addr + sizeof(size_t);         \
    name_ptr;                                               \
})

#define ESH_KNAME_LEN(key)                                  \
__extension__ ({                                            \
    size_t kname_len = strlen(key) + 1;                     \
    size_t len = (kname_len < ESH_MIN_KEY_LEN) ?            \
    ESH_MIN_KEY_LEN : kname_len;                            \
    len;                                                    \
})

#define ESH_DATA_PTR(addr)                                  \
__extension__ ({                                            \
    size_t kname_len =                                      \
        ESH_KNAME_LEN(ESH_KNAME_PTR(addr));                 \
    uint8_t *data_ptr = addr + sizeof(size_t) + kname_len;  \
    data_ptr;                                               \
})

#define ESH_DATA_SIZE(addr, data_ptr)                       \
__extension__ ({                                            \
    size_t sz = ESH_KV_SIZE(addr);                          \
    size_t data_size = sz - (data_ptr - addr);              \
    data_size;                                              \
})

#define ESH_KEY_SIZE(key, size)                             \
__extension__ ({                                            \
    size_t len =                                            \
        sizeof(size_t) + ESH_KNAME_LEN(key) + size;         \
    len;                                                    \
})

/* in ext slot new offset will be stored in case if
 * new data were added for the same process during
 * next commit
 */
#define EXT_SLOT_SIZE()                                     \
    (ESH_KEY_SIZE(ESH_REGION_EXTENSION, sizeof(size_t)))


#define ESH_PUT_KEY(addr, key, buffer, size)                \
__extension__ ({                                            \
    size_t sz = ESH_KEY_SIZE(key, size);                    \
    memcpy(addr, &sz, sizeof(size_t));                      \
    memset(addr + sizeof(size_t), 0,                        \
        ESH_KNAME_LEN(key));                                \
    strncpy((char *)addr + sizeof(size_t),                  \
            key, ESH_KNAME_LEN(key));                       \
    memcpy(addr + sizeof(size_t) + ESH_KNAME_LEN(key),      \
            buffer, size);                                  \
})

static size_t _ds20_kv_size(uint8_t *addr);
static size_t _ds20_key_size(const char *key, size_t data_size);
static char* _ds20_key_name_ptr(uint8_t *addr);
static size_t _ds20_key_len(const char *key);
static uint8_t* _ds20_data_ptr(uint8_t *addr);
static size_t _ds20_data_size(uint8_t *addr, uint8_t *data_ptr);
static size_t _ds20_slot_size(void);
static void _ds20_put_key(uint8_t *data, char *key, void *buf, size_t size);

dstore_mod_t dstore_v20_module = {
    "ds20",
    _ds20_kv_size,
    _ds20_key_size,
    _ds20_key_name_ptr,
    _ds20_key_len,
    _ds20_data_ptr,
    _ds20_data_size,
    _ds20_slot_size,
    _ds20_put_key
};

static size_t _ds20_kv_size(uint8_t *addr) {
    return ESH_KV_SIZE(addr);
}

static size_t _ds20_key_size(const char *key, size_t data_size) {
    return ESH_KEY_SIZE(key, data_size);
}

static char* _ds20_key_name_ptr(uint8_t *addr) {
    return ESH_KNAME_PTR(addr);
}

static size_t _ds20_key_len(const char *key) {
    return ESH_KNAME_LEN(key);
}

static uint8_t* _ds20_data_ptr(uint8_t *addr) {
    return ESH_DATA_PTR(addr);
}

static size_t _ds20_data_size(uint8_t *addr, uint8_t *data_ptr) {
    return ESH_DATA_SIZE(addr, data_ptr);
}

static size_t _ds20_slot_size(void) {
    return EXT_SLOT_SIZE();
}

static void _ds20_put_key(uint8_t *data, char *key, void *buf, size_t size) {
    ESH_PUT_KEY(data, key, buf, size);
}
