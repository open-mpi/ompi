/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "adio.h"
#include "hint_fns.h"


int ADIOI_Info_check_and_install_int(ADIO_File fd, MPI_Info info, const char *key,
                                     int *local_cache, char *funcname, int *error_code)
{
    int intval, tmp_val, flag, ret = 0;
    char *value;

    value = (char *) ADIOI_Malloc((MPI_MAX_INFO_VAL + 1) * sizeof(char));
    if (value == NULL) {
        *error_code = MPIO_Err_create_code(*error_code,
                                           MPIR_ERR_RECOVERABLE,
                                           funcname, __LINE__, MPI_ERR_OTHER, "**nomem2", 0);
        return -1;
    }

    ADIOI_Info_get(info, key, MPI_MAX_INFO_VAL, value, &flag);
    if (flag) {
        intval = atoi(value);
        tmp_val = intval;

        MPI_Bcast(&tmp_val, 1, MPI_INT, 0, fd->comm);
        /* --BEGIN ERROR HANDLING-- */
        if (tmp_val != intval) {
            MPIO_ERR_CREATE_CODE_INFO_NOT_SAME(funcname, key, error_code);
            ret = -1;
            goto fn_exit;
        }
        /* --END ERROR HANDLING-- */

        ADIOI_Info_set(fd->info, key, value);
        /* some file systems do not cache hints in the fd struct */
        if (local_cache != NULL)
            *local_cache = intval;
    }
  fn_exit:
    ADIOI_Free(value);
    return ret;
}

int ADIOI_Info_check_and_install_enabled(ADIO_File fd, MPI_Info info, const char *key,
                                         int *local_cache, char *funcname, int *error_code)
{
    int tmp_val, flag, ret = 0;
    char *value;

    value = (char *) ADIOI_Malloc((MPI_MAX_INFO_VAL + 1) * sizeof(char));
    if (value == NULL) {
        *error_code = MPIO_Err_create_code(*error_code,
                                           MPIR_ERR_RECOVERABLE,
                                           funcname, __LINE__, MPI_ERR_OTHER, "**nomem2", 0);
        return -1;
    }

    ADIOI_Info_get(info, key, MPI_MAX_INFO_VAL, value, &flag);
    if (flag) {
        if (!strcmp(value, "enable") || !strcmp(value, "ENABLE")) {
            ADIOI_Info_set(fd->info, key, value);
            *local_cache = ADIOI_HINT_ENABLE;
        } else if (!strcmp(value, "disable") || !strcmp(value, "DISABLE")) {
            ADIOI_Info_set(fd->info, key, value);
            *local_cache = ADIOI_HINT_DISABLE;
        } else if (!strcmp(value, "automatic") || !strcmp(value, "AUTOMATIC")) {
            ADIOI_Info_set(fd->info, key, value);
            *local_cache = ADIOI_HINT_AUTO;
            /* treat the user-provided string like "enabled":  either it is a hint
             * ROMIO knows about and can support it, or ROMIO will not return the
             * hint at all in the MPI_File_get_info info object */
        } else if (!strcmp(value, "requested") || !strcmp(value, "REQUESTED")) {
            ADIOI_Info_set(fd->info, key, "enable");
            *local_cache = ADIOI_HINT_ENABLE;
        }

        tmp_val = *local_cache;

        MPI_Bcast(&tmp_val, 1, MPI_INT, 0, fd->comm);
        /* --BEGIN ERROR HANDLING-- */
        if (tmp_val != *local_cache) {
            MPIO_ERR_CREATE_CODE_INFO_NOT_SAME(funcname, key, error_code);
            ret = -1;
            goto fn_exit;
        }
        /* --END ERROR HANDLING-- */
    }
  fn_exit:
    ADIOI_Free(value);
    return ret;
}

int ADIOI_Info_check_and_install_true(ADIO_File fd, MPI_Info info, const char *key,
                                      int *local_cache, char *funcname, int *error_code)
{
    int flag, tmp_val, ret = 0;
    char *value;

    value = (char *) ADIOI_Malloc((MPI_MAX_INFO_VAL + 1) * sizeof(char));
    if (value == NULL) {
        *error_code = MPIO_Err_create_code(*error_code,
                                           MPIR_ERR_RECOVERABLE,
                                           funcname, __LINE__, MPI_ERR_OTHER, "**nomem2", 0);
        return -1;
    }

    ADIOI_Info_get(info, key, MPI_MAX_INFO_VAL, value, &flag);
    if (flag) {
        if (!strcmp(value, "true") || !strcmp(value, "TRUE")) {
            ADIOI_Info_set(fd->info, key, value);
            *local_cache = 1;
        } else if (!strcmp(value, "false") || !strcmp(value, "FALSE")) {
            ADIOI_Info_set(fd->info, key, value);
            *local_cache = 0;
        }
        tmp_val = *local_cache;

        MPI_Bcast(&tmp_val, 1, MPI_INT, 0, fd->comm);
        /* --BEGIN ERROR HANDLING-- */
        if (tmp_val != *local_cache) {
            MPIO_ERR_CREATE_CODE_INFO_NOT_SAME(funcname, key, error_code);
            ret = -1;
            goto fn_exit;
        }
        /* --END ERROR HANDLING-- */
    }
  fn_exit:
    ADIOI_Free(value);
    return ret;
}

int ADIOI_Info_check_and_install_str(ADIO_File fd, MPI_Info info, const char *key,
                                     char **local_cache, char *funcname, int *error_code)
{
    int flag, ret = 0;
    size_t len;
    char *value;

    value = (char *) ADIOI_Malloc((MPI_MAX_INFO_VAL + 1) * sizeof(char));
    if (value == NULL) {
        *error_code = MPIO_Err_create_code(*error_code,
                                           MPIR_ERR_RECOVERABLE,
                                           funcname, __LINE__, MPI_ERR_OTHER, "**nomem2", 0);
        return -1;
    }

    ADIOI_Info_get(info, key, MPI_MAX_INFO_VAL, value, &flag);
    if (flag) {
        ADIOI_Info_set(fd->info, key, value);
        len = (strlen(value) + 1) * sizeof(char);
        *local_cache = ADIOI_Malloc(len);
        if (*local_cache == NULL) {
            *error_code = MPIO_Err_create_code(*error_code,
                                               MPIR_ERR_RECOVERABLE,
                                               funcname, __LINE__, MPI_ERR_OTHER, "**nomem2", 0);
            ret = -1;
            goto fn_exit;
        }
        ADIOI_Strncpy(*local_cache, value, len);
    }
    /* if it has been set already, we ignore it the second time.
     * otherwise we would get an error if someone used the same
     * info value with a cb_config_list value in it in a couple
     * of calls, which would be irritating. */
  fn_exit:
    ADIOI_Free(value);
    return ret;
}
