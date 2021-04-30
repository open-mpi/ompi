/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include  "mpl.h"

/* Serialize a handle. A serialized handle is a string of
 * characters that can be persisted by the caller. The serialized
 * handle can be used to create another ref to the shared mem seg
 * by deserializing it.
 * str  : A string of chars of len, str_len.
 *          If the function succeeds the serialized handle is copied
 *          into this user buffer
 * hnd  : Handle to shared memory
 */
int MPL_shm_hnd_serialize(char *str, MPL_shm_hnd_t hnd, int str_len)
{
    return MPLI_shm_ghnd_get_by_val(hnd, str, str_len);
}

/* Deserialize a handle.
 * str_hnd  : A null-terminated string of len str_hnd_len that
 *              contains the serialized handle.
 * hnd      : If the call succeeds the user gets back a handle,hnd, to
 *           shared mem - deserialized from strhnd. This handle
 *           will refer to the shm seg referred by the serialized
 *           handle.
 */
int MPL_shm_hnd_deserialize(MPL_shm_hnd_t hnd, const char *str_hnd, size_t str_hnd_len)
{
    int rc = MPL_SUCCESS;
    MPLI_shm_hnd_reset_val(hnd);
    rc = MPLI_shm_ghnd_alloc(hnd, MPL_MEM_SHM);
    if (rc != MPL_SUCCESS)
        return rc;
    rc = MPLI_shm_ghnd_set_by_val(hnd, "%s", str_hnd);
    if (rc != MPL_SUCCESS)
        return rc;
    rc = MPL_shm_seg_open(hnd, 0);
    return rc;
}

/* Get a serialized handle by reference.
 * Rationale: The user might only want to read the serialized view
 * of the handle & hence not want to allocate a buffer for the ser view
 * of the handle.
 * str_ptr  : Pointer to a string of chars to hold the serialized handle
 *           If the function succeeds, the pointer points to a
 *           serialized view of the handle.
 * hnd      : Handle to shm seg which has to be serialized
 */

int MPL_shm_hnd_get_serialized_by_ref(MPL_shm_hnd_t hnd, char **str_ptr)
{
    *str_ptr = (char *) MPLI_shm_ghnd_get_by_ref(hnd);
    return MPL_SUCCESS;
}

/* Deserialize a handle by reference.
 * Rationale : The user already has a serialized view of the handle.
 *            The user does not want to manage the view buffer any more
 *            & also needs to deserialize from the buffer.
 * ser_hnd_ptr  : Pointer to a serialized view of the handle. The user
 *           no longer needs to take care of this buffer.
 * hnd      : If the function succeeds this points to the deserialized
 *           handle.
 */
int MPL_shm_hnd_deserialize_by_ref(MPL_shm_hnd_t hnd, char **ser_hnd_ptr)
{
    MPLI_shm_hnd_reset_val(hnd);
    MPLI_shm_ghnd_set_by_ref(hnd, *ser_hnd_ptr);

    return MPL_shm_seg_open(hnd, 0);
}

/* Initialize a shared memory handle
 * hnd_ptr : A pointer to the shared memory handle
 */

int MPL_shm_hnd_init(MPL_shm_hnd_t * hnd_ptr)
{
    int rc = -1;

    rc = MPLI_shm_hnd_alloc(hnd_ptr, MPL_MEM_SHM);

    if (MPL_SUCCESS != rc)
        return rc;

    MPLI_shm_hnd_reset_val(*hnd_ptr);

    return rc;
}

/* Finalize a shared memory handle.
 * hnd_ptr : A pointer to the shm handle to be finalized.
 *           Any handle that is init has to be finalized.
 */
int MPL_shm_hnd_finalize(MPL_shm_hnd_t * hnd_ptr)
{
    /* A finalize can/should be called on an invalid handle
     * Don't assert if we fail here ...
     */
    MPLI_shm_hnd_close(*hnd_ptr);
    MPLI_shm_hnd_free(*hnd_ptr);

    *hnd_ptr = MPL_SHM_HND_INVALID;

    return 0;
}
