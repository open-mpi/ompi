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

static inline size_t _ds12_kv_size(uint8_t *addr) {
    size_t sz;
    memcpy(&sz, addr + _ds12_key_len(_ds12_key_name_ptr(addr)),
        sizeof(size_t));
    sz += _ds12_key_len(_ds12_key_name_ptr(addr)) + sizeof(size_t);
    return sz;
}

static inline size_t _ds12_key_size(const char *key, size_t data_size) {
    size_t len = strlen(key) + 1 + sizeof(size_t) + data_size;
    return len;
}

static inline char* _ds12_key_name_ptr(uint8_t *addr) {
    char *name_ptr = (char *)addr;
    return name_ptr;
}

static inline size_t _ds12_key_len(const char *key) {
    size_t len = strlen((char*)key) + 1;
    return len;
}

static inline uint8_t* _ds12_data_ptr(uint8_t *addr) {
    uint8_t *data_ptr = addr + sizeof(size_t) +
        _ds12_key_len(_ds12_key_name_ptr(addr));
    return data_ptr;
}

static inline size_t _ds12_data_size(uint8_t *addr, uint8_t *data_ptr) {
    size_t data_size;
    memcpy(&data_size,
        addr + _ds12_key_len(_ds12_key_name_ptr(addr)),
        sizeof(size_t));
    return data_size;
}

static inline size_t _ds12_slot_size(void) {
    return _ds12_key_size(ESH_REGION_EXTENSION, sizeof(size_t));
}

static inline void _ds12_put_key(uint8_t *data, char *key, void *buf, size_t size) {
    size_t sz = size;
    memset(data, 0, _ds12_key_len(key));
    strncpy((char *)data, key, _ds12_key_len(key));
    memcpy(data + _ds12_key_len(key), &sz, sizeof(size_t));
    memcpy(data + _ds12_key_len(key) + sizeof(size_t), buf, size);
}
