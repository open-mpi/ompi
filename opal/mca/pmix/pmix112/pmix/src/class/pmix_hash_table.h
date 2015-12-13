/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2015      Intel, Inc. All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

/** @file
 *
 *  A hash table that may be indexed with either fixed length
 *  (e.g. uint32_t/uint64_t) or arbitrary size binary key
 *  values. However, only one key type may be used in a given table
 *  concurrently.
 */

#ifndef PMIX_HASH_TABLE_H
#define PMIX_HASH_TABLE_H

#include <private/autogen/config.h>
#include <private/prefetch.h>

#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif

#include "src/class/pmix_list.h"

#include <pmix/pmix_common.h>

BEGIN_C_DECLS

PMIX_DECLSPEC PMIX_CLASS_DECLARATION(pmix_hash_table_t);

struct pmix_hash_table_t
{
    pmix_object_t        super;          /**< subclass of pmix_object_t */
    pmix_list_t          ht_nodes;       /**< free list of hash nodes */
    pmix_list_t         *ht_table;       /**< each item is an array of pmix_fhnode_t nodes */
    size_t              ht_table_size;  /**< size of table */
    size_t              ht_size;        /**< number of values on table */
    size_t              ht_mask;
};
typedef struct pmix_hash_table_t pmix_hash_table_t;



/**
 *  Initializes the table size, must be called before using
 *  the table.
 *
 *  @param   table   The input hash table (IN).
 *  @param   size    The size of the table, which will be rounded up
 *                   (if required) to the next highest power of two (IN).
 *  @return  PMIX error code.
 *
 */

PMIX_DECLSPEC int pmix_hash_table_init(pmix_hash_table_t* ht, size_t table_size);

/**
 * Alternative form
 */
PMIX_DECLSPEC int pmix_hash_table_init2(pmix_hash_table_t* ht, size_t estimated_max_size,
                                        int density_numer, int density_denom,
                                        int growth_numer, int growth_denom);

/**
 *  Returns the number of elements currently stored in the table.
 *
 *  @param   table   The input hash table (IN).
 *  @return  The number of elements in the table.
 *
 */

static inline size_t pmix_hash_table_get_size(pmix_hash_table_t *ht)
{
    return ht->ht_size;
}

/**
 *  Remove all elements from the table.
 *
 *  @param   table   The input hash table (IN).
 *  @return  PMIX return code.
 *
 */

PMIX_DECLSPEC pmix_status_t pmix_hash_table_remove_all(pmix_hash_table_t *ht);

/**
 *  Retrieve value via uint32_t key.
 *
 *  @param   table   The input hash table (IN).
 *  @param   key     The input key (IN).
 *  @param   ptr     The value associated with the key
 *  @return  integer return code:
 *           - PMIX_SUCCESS       if key was found
 *           - PMIX_ERR_NOT_FOUND if key was not found
 *           - PMIX_ERROR         other error
 *
 */

PMIX_DECLSPEC pmix_status_t pmix_hash_table_get_value_uint32(pmix_hash_table_t* table, uint32_t key,
						             void** ptr);

/**
 *  Set value based on uint32_t key.
 *
 *  @param   table   The input hash table (IN).
 *  @param   key     The input key (IN).
 *  @param   value   The value to be associated with the key (IN).
 *  @return  PMIX return code.
 *
 */

PMIX_DECLSPEC pmix_status_t pmix_hash_table_set_value_uint32(pmix_hash_table_t* table, uint32_t key, void* value);

/**
 *  Remove value based on uint32_t key.
 *
 *  @param   table   The input hash table (IN).
 *  @param   key     The input key (IN).
 *  @return  PMIX return code.
 *
 */

PMIX_DECLSPEC pmix_status_t pmix_hash_table_remove_value_uint32(pmix_hash_table_t* table, uint32_t key);

/**
 *  Retrieve value via uint64_t key.
 *
 *  @param   table   The input hash table (IN).
 *  @param   key     The input key (IN).
 *  @param   ptr     The value associated with the key
 *  @return  integer return code:
 *           - PMIX_SUCCESS       if key was found
 *           - PMIX_ERR_NOT_FOUND if key was not found
 *           - PMIX_ERROR         other error
 *
 */

PMIX_DECLSPEC pmix_status_t pmix_hash_table_get_value_uint64(pmix_hash_table_t *table, uint64_t key,
						             void **ptr);

/**
 *  Set value based on uint64_t key.
 *
 *  @param   table   The input hash table (IN).
 *  @param   key     The input key (IN).
 *  @param   value   The value to be associated with the key (IN).
 *  @return  PMIX return code.
 *
 */

PMIX_DECLSPEC pmix_status_t pmix_hash_table_set_value_uint64(pmix_hash_table_t *table, uint64_t key, void* value);

/**
 *  Remove value based on uint64_t key.
 *
 *  @param   table   The input hash table (IN).
 *  @param   key     The input key (IN).
 *  @return  PMIX return code.
 *
 */

PMIX_DECLSPEC pmix_status_t pmix_hash_table_remove_value_uint64(pmix_hash_table_t *table, uint64_t key);

/**
 *  Retrieve value via arbitrary length binary key.
 *
 *  @param   table   The input hash table (IN).
 *  @param   key     The input key (IN).
 *  @param   ptr     The value associated with the key
 *  @return  integer return code:
 *           - PMIX_SUCCESS       if key was found
 *           - PMIX_ERR_NOT_FOUND if key was not found
 *           - PMIX_ERROR         other error
 *
 */

PMIX_DECLSPEC pmix_status_t pmix_hash_table_get_value_ptr(pmix_hash_table_t *table, const void* key,
						          size_t keylen, void **ptr);

/**
 *  Set value based on arbitrary length binary key.
 *
 *  @param   table   The input hash table (IN).
 *  @param   key     The input key (IN).
 *  @param   value   The value to be associated with the key (IN).
 *  @return  PMIX return code.
 *
 */

PMIX_DECLSPEC pmix_status_t pmix_hash_table_set_value_ptr(pmix_hash_table_t *table, const void* key, size_t keylen, void* value);

/**
 *  Remove value based on arbitrary length binary key.
 *
 *  @param   table   The input hash table (IN).
 *  @param   key     The input key (IN).
 *  @return  PMIX return code.
 *
 */

PMIX_DECLSPEC pmix_status_t pmix_hash_table_remove_value_ptr(pmix_hash_table_t *table, const void* key, size_t keylen);


/** The following functions are only for allowing iterating through
    the hash table. The calls return along with a key, a pointer to
    the hash node with the current key, so that subsequent calls do
    not have to traverse all over again to the key (although it may
    just be a simple thing - to go to the array element and then
    traverse through the individual list). But lets take out this
    inefficiency too. This is similar to having an STL iterator in
    functionality */

/**
 *  Get the first 32 bit key from the hash table, which can be used later to
 *  get the next key
 *  @param  table   The hash table pointer (IN)
 *  @param  key     The first key (OUT)
 *  @param  value   The value corresponding to this key (OUT)
 *  @param  node    The pointer to the hash table internal node which stores
 *                  the key-value pair (this is required for subsequent calls
 *                  to get_next_key) (OUT)
 *  @return PMIX error code
 *
 */

PMIX_DECLSPEC pmix_status_t pmix_hash_table_get_first_key_uint32(pmix_hash_table_t *table, uint32_t *key,
					                         void **value, void **node);


/**
 *  Get the next 32 bit key from the hash table, knowing the current key
 *  @param  table    The hash table pointer (IN)
 *  @param  key      The key (OUT)
 *  @param  value    The value corresponding to this key (OUT)
 *  @param  in_node  The node pointer from previous call to either get_first
                     or get_next (IN)
 *  @param  out_node The pointer to the hash table internal node which stores
 *                   the key-value pair (this is required for subsequent calls
 *                   to get_next_key) (OUT)
 *  @return PMIX error code
 *
 */

PMIX_DECLSPEC pmix_status_t pmix_hash_table_get_next_key_uint32(pmix_hash_table_t *table, uint32_t *key,
				                                void **value, void *in_node,
				                                void **out_node);


/**
 *  Get the first 64 key from the hash table, which can be used later to
 *  get the next key
 *  @param  table   The hash table pointer (IN)
 *  @param  key     The first key (OUT)
 *  @param  value   The value corresponding to this key (OUT)
 *  @param  node    The pointer to the hash table internal node which stores
 *                  the key-value pair (this is required for subsequent calls
 *                  to get_next_key) (OUT)
 *  @return PMIX error code
 *
 */

PMIX_DECLSPEC pmix_status_t pmix_hash_table_get_first_key_uint64(pmix_hash_table_t *table, uint64_t *key,
				                                 void **value, void **node);


/**
 *  Get the next 64 bit key from the hash table, knowing the current key
 *  @param  table    The hash table pointer (IN)
 *  @param  key      The key (OUT)
 *  @param  value    The value corresponding to this key (OUT)
 *  @param  in_node  The node pointer from previous call to either get_first
                     or get_next (IN)
 *  @param  out_node The pointer to the hash table internal node which stores
 *                   the key-value pair (this is required for subsequent calls
 *                   to get_next_key) (OUT)
 *  @return PMIX error code
 *
 */

PMIX_DECLSPEC pmix_status_t pmix_hash_table_get_next_key_uint64(pmix_hash_table_t *table, uint64_t *key,
				                                void **value, void *in_node,
				                                void **out_node);

/**
 * @brief Returns next power-of-two of the given value.
 *
 * @param value The integer value to return power of 2
 *
 * @returns The next power of two
 *
 * WARNING: *NO* error checking is performed.  This is meant to be a
 * fast inline function.
 * Using __builtin_clz (count-leading-zeros) uses 4 cycles instead of 77
 * compared to the loop-version (on Intel Nehalem -- with icc-12.1.0 -O2).
 */
static inline int pmix_next_poweroftwo(int value)
{
    int power2;

#if PMIX_C_HAVE_BUILTIN_CLZ
    if (PMIX_UNLIKELY (0 == value)) {
        return 1;
    }
    power2 = 1 << (8 * sizeof (int) - __builtin_clz(value));
#else
    for (power2 = 1; value > 0; value >>= 1, power2 <<= 1) /* empty */;
#endif

    return power2;
}


END_C_DECLS

#endif  /* PMIX_HASH_TABLE_H */
