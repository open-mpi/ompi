/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, Inc. All rights reserved.
 * Copyright (c) 2013-2015 Intel, Inc. All rights reserved
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 *
 * Data packing subsystem.
 */

#ifndef PMIX_BFROP_H_
#define PMIX_BFROP_H_

#include <private/autogen/config.h>
#include <pmix/rename.h>
#include <private/types.h>

#include "src/include/pmix_globals.h"
#include "src/buffer_ops/types.h"

BEGIN_C_DECLS

PMIX_DECLSPEC extern const char pmix_version_string[];

/* internally used object for transferring data
 * to/from the server and for storing in the
 * hash tables */
typedef struct {
    pmix_list_item_t super;
    char *key;
    pmix_value_t *value;
} pmix_kval_t;
PMIX_CLASS_DECLARATION(pmix_kval_t);

/* A non-API function for something that happens in a number
 * of places throughout the code base - transferring a value to
 * another pmix_value_t structure
 */
PMIX_DECLSPEC pmix_status_t pmix_value_xfer(pmix_value_t *kv, pmix_value_t *src);
PMIX_DECLSPEC void pmix_value_load(pmix_value_t *v, void *data,
                                   pmix_data_type_t type);
PMIX_DECLSPEC pmix_status_t pmix_value_unload(pmix_value_t *kv, void **data,
                                              size_t *sz, pmix_data_type_t type);
PMIX_DECLSPEC bool pmix_value_cmp(pmix_value_t *p, pmix_value_t *p1);


#define PMIX_LOAD_BUFFER(b, d, s)                       \
    do {                                                \
        (b)->base_ptr = (char*)(d);                     \
        (b)->bytes_used = (s);                          \
        (b)->bytes_allocated = (s);                     \
        (b)->pack_ptr = ((char*)(b)->base_ptr) + (s);   \
        (b)->unpack_ptr = (b)->base_ptr;                \
        (d) = NULL;                                     \
        (s) = 0;                                        \
    } while(0);

#define PMIX_UNLOAD_BUFFER(b, d, s)             \
    do {                                        \
        (d) = (char*)(b)->unpack_ptr;           \
        (s) = (b)->bytes_used;                  \
        (b)->base_ptr = NULL;                   \
        (b)->bytes_used = 0;                    \
        (b)->bytes_allocated = 0;               \
        (b)->pack_ptr = NULL;                   \
        (b)->unpack_ptr = NULL;                 \
    } while (0);


/**
 * Top-level interface function to pack one or more values into a
 * buffer.
 *
 * The pack function packs one or more values of a specified type into
 * the specified buffer.  The buffer must have already been
 * initialized via an PMIX_NEW or PMIX_CONSTRUCT call - otherwise, the
 * pack_value function will return an error. Providing an unsupported
 * type flag will likewise be reported as an error.
 *
 * Note that any data to be packed that is not hard type cast (i.e.,
 * not type cast to a specific size) may lose precision when unpacked
 * by a non-homogeneous recipient.  The BFROP will do its best to deal
 * with heterogeneity issues between the packer and unpacker in such
 * cases. Sending a number larger than can be handled by the recipient
 * will return an error code (generated by the BFROP upon unpacking) -
 * the BFROP cannot detect such errors during packing.
 *
 * @param *buffer A pointer to the buffer into which the value is to
 * be packed.
 *
 * @param *src A void* pointer to the data that is to be packed. Note
 * that strings are to be passed as (char **) - i.e., the caller must
 * pass the address of the pointer to the string as the void*. This
 * allows the BFROP to use a single interface function, but still allow
 * the caller to pass multiple strings in a single call.
 *
 * @param num_values An int32_t indicating the number of values that are
 * to be packed, beginning at the location pointed to by src. A string
 * value is counted as a single value regardless of length. The values
 * must be contiguous in memory. Arrays of pointers (e.g., string
 * arrays) should be contiguous, although (obviously) the data pointed
 * to need not be contiguous across array entries.
 *
 * @param type The type of the data to be packed - must be one of the
 * PMIX defined data types.
 *
 * @retval PMIX_SUCCESS The data was packed as requested.
 *
 * @retval PMIX_ERROR(s) An appropriate PMIX error code indicating the
 * problem encountered. This error code should be handled
 * appropriately.
 *
 * @code
 * pmix_buffer_t *buffer;
 * int32_t src;
 *
 * status_code = pmix_bfrop.pack(buffer, &src, 1, PMIX_INT32);
 * @endcode
 */
typedef pmix_status_t (*pmix_bfrop_pack_fn_t)(pmix_buffer_t *buffer, const void *src,
                                              int32_t num_values,
                                              pmix_data_type_t type);

/**
 * Unpack values from a buffer.
 *
 * The unpack function unpacks the next value (or values) of a
 * specified type from the specified buffer.
 *
 * The buffer must have already been initialized via an PMIX_NEW or
 * PMIX_CONSTRUCT call (and assumedly filled with some data) -
 * otherwise, the unpack_value function will return an
 * error. Providing an unsupported type flag will likewise be reported
 * as an error, as will specifying a data type that DOES NOT match the
 * type of the next item in the buffer. An attempt to read beyond the
 * end of the stored data held in the buffer will also return an
 * error.
 *
 * NOTE: it is possible for the buffer to be corrupted and that
 * the BFROP will *think* there is a proper variable type at the
 * beginning of an unpack region - but that the value is bogus (e.g., just
 * a byte field in a string array that so happens to have a value that
 * matches the specified data type flag). Therefore, the data type error check
 * is NOT completely safe. This is true for ALL unpack functions.
 *
 *
 * Unpacking values is a "destructive" process - i.e., the values are
 * removed from the buffer, thus reducing the buffer size. It is
 * therefore not possible for the caller to re-unpack a value from the
 * same buffer.
 *
 * Warning: The caller is responsible for providing adequate memory
 * storage for the requested data. As noted below, the user
 * must provide a parameter indicating the maximum number of values that
 * can be unpacked into the allocated memory. If more values exist in the
 * buffer than can fit into the memory storage, then the bfrop will unpack
 * what it can fit into that location and return an error code indicating
 * that the buffer was only partially unpacked.
 *
 * Note that any data that was not hard type cast (i.e., not type cast
 * to a specific size) when packed may lose precision when unpacked by
 * a non-homogeneous recipient.  The BFROP will do its best to deal with
 * heterogeneity issues between the packer and unpacker in such
 * cases. Sending a number larger than can be handled by the recipient
 * will return an error code generated by the BFROP upon unpacking - the
 * BFROP cannot detect such errors during packing.
 *
 * @param *buffer A pointer to the buffer from which the value will be
 * extracted.
 *
 * @param *dest A void* pointer to the memory location into which the
 * data is to be stored. Note that these values will be stored
 * contiguously in memory. For strings, this pointer must be to (char
 * **) to provide a means of supporting multiple string
 * operations. The BFROP unpack function will allocate memory for each
 * string in the array - the caller must only provide adequate memory
 * for the array of pointers.
 *
 * @param type The type of the data to be unpacked - must be one of
 * the BFROP defined data types.
 *
 * @retval *max_num_values The number of values actually unpacked. In
 * most cases, this should match the maximum number provided in the
 * parameters - but in no case will it exceed the value of this
 * parameter.  Note that if you unpack fewer values than are actually
 * available, the buffer will be in an unpackable state - the bfrop will
 * return an error code to warn of this condition.
 *
 * @note The unpack function will return the actual number of values
 * unpacked in this location.
 *
 * @retval PMIX_SUCCESS The next item in the buffer was successfully
 * unpacked.
 *
 * @retval PMIX_ERROR(s) The unpack function returns an error code
 * under one of several conditions: (a) the number of values in the
 * item exceeds the max num provided by the caller; (b) the type of
 * the next item in the buffer does not match the type specified by
 * the caller; or (c) the unpack failed due to either an error in the
 * buffer or an attempt to read past the end of the buffer.
 *
 * @code
 * pmix_buffer_t *buffer;
 * int32_t dest;
 * char **string_array;
 * int32_t num_values;
 *
 * num_values = 1;
 * status_code = pmix_bfrop.unpack(buffer, (void*)&dest, &num_values, PMIX_INT32);
 *
 * num_values = 5;
 * string_array = malloc(num_values*sizeof(char *));
 * status_code = pmix_bfrop.unpack(buffer, (void*)(string_array), &num_values, PMIX_STRING);
 *
 * @endcode
 */
typedef pmix_status_t (*pmix_bfrop_unpack_fn_t)(pmix_buffer_t *buffer, void *dest,
                                                int32_t *max_num_values,
                                                pmix_data_type_t type);
/**
 * Copy a payload from one buffer to another
 * This function will append a copy of the payload in one buffer into
 * another buffer. If the destination buffer is NOT empty, then the
 * type of the two buffers MUST match or else an
 * error will be returned. If the destination buffer IS empty, then
 * its type will be set to that of the source buffer.
 * NOTE: This is NOT a destructive procedure - the
 * source buffer's payload will remain intact, as will any pre-existing
 * payload in the destination's buffer.
 */
typedef pmix_status_t (*pmix_bfrop_copy_payload_fn_t)(pmix_buffer_t *dest,
                                                      pmix_buffer_t *src);

/**
 * BFROP initialization function.
 *
 * In dynamic libraries, declared objects and functions don't get
 * loaded until called. We need to ensure that the pmix_bfrop function
 * structure gets loaded, so we provide an "open" call that is
 * executed as part of the program startup.
 */
PMIX_DECLSPEC pmix_status_t pmix_bfrop_open(void);

/**
 * BFROP finalize function
 */
PMIX_DECLSPEC pmix_status_t pmix_bfrop_close(void);


/**
 * Copy a data value from one location to another.
 *
 * Since registered data types can be complex structures, the system
 * needs some way to know how to copy the data from one location to
 * another (e.g., for storage in the registry). This function, which
 * can call other copy functions to build up complex data types, defines
 * the method for making a copy of the specified data type.
 *
 * @param **dest The address of a pointer into which the
 * address of the resulting data is to be stored.
 *
 * @param *src A pointer to the memory location from which the
 * data is to be copied.
 *
 * @param type The type of the data to be copied - must be one of
 * the BFROP defined data types.
 *
 * @retval PMIX_SUCCESS The value was successfully copied.
 *
 * @retval PMIX_ERROR(s) An appropriate error code.
 *
 */
typedef pmix_status_t (*pmix_bfrop_copy_fn_t)(void **dest, void *src, pmix_data_type_t type);

/**
 * Print a data value.
 *
 * Since registered data types can be complex structures, the system
 * needs some way to know how to print them (i.e., convert them to a string
 * representation). Provided for debug purposes.
 *
 * @retval PMIX_SUCCESS The value was successfully printed.
 *
 * @retval PMIX_ERROR(s) An appropriate error code.
 */
typedef pmix_status_t (*pmix_bfrop_print_fn_t)(char **output, char *prefix, void *src, pmix_data_type_t type);

/**
 * Base structure for the BFROP
 *
 * Base module structure for the BFROP - presents the required function
 * pointers to the calling interface.
 */
struct pmix_bfrop_t {
    pmix_bfrop_pack_fn_t              pack;
    pmix_bfrop_unpack_fn_t            unpack;
    pmix_bfrop_copy_fn_t              copy;
    pmix_bfrop_print_fn_t             print;
    pmix_bfrop_copy_payload_fn_t      copy_payload;
};
typedef struct pmix_bfrop_t pmix_bfrop_t;

PMIX_DECLSPEC extern pmix_bfrop_t pmix_bfrop;  /* holds bfrop function pointers */

END_C_DECLS

#endif /* PMIX_BFROP_H */
