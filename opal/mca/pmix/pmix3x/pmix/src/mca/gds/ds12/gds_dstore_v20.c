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

static inline size_t _ds20_kv_size(uint8_t *addr) {
    size_t sz;
    memcpy(&sz, addr, sizeof(size_t));
    return sz;
}

static inline size_t _ds20_key_size(const char *key, size_t data_size) {
    size_t len = sizeof(size_t) + _ds20_key_len(key) + data_size;
    return len;
}

static inline char* _ds20_key_name_ptr(uint8_t *addr) {
    char *name_ptr = (char *)addr + sizeof(size_t);
    return name_ptr;
}

static inline size_t _ds20_key_len(const char *key) {
    size_t kname_len = strlen(key) + 1;
    size_t len = (kname_len < ESH_MIN_KEY_LEN) ?
    ESH_MIN_KEY_LEN : kname_len;
    return len;
}

static inline uint8_t* _ds20_data_ptr(uint8_t *addr) {
    size_t kname_len =
        _ds20_key_len(_ds20_key_name_ptr(addr));
    uint8_t *data_ptr = addr + sizeof(size_t) + kname_len;
    return data_ptr;
}

static inline size_t _ds20_data_size(uint8_t *addr, uint8_t *data_ptr) {
    size_t sz = _ds20_kv_size(addr);
    size_t data_size = sz - (data_ptr - addr);
    return data_size;
}

static inline size_t _ds20_slot_size(void) {
    return _ds20_key_size(ESH_REGION_EXTENSION, sizeof(size_t));
}

static inline void _ds20_put_key(uint8_t *data, char *key, void *buf, size_t size) {
    size_t sz = _ds20_key_size(key, size);
    memcpy(data, &sz, sizeof(size_t));
    memset(data + sizeof(size_t), 0, _ds20_key_len(key));
    strncpy((char *)data + sizeof(size_t), key, _ds20_key_len(key));
    memcpy(data + sizeof(size_t) + _ds20_key_len(key), buf, size);
}
