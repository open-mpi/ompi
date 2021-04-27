/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */


#include "ad_quobytefs.h"

#include "adioi.h"
#include <string.h>

struct ADIOI_Fns_struct ADIO_QUOBYTEFS_operations = {
    ADIOI_QUOBYTEFS_Open,       /* Open */
    ADIOI_GEN_OpenColl, /* OpenColl */
    ADIOI_QUOBYTEFS_ReadContig, /* ReadContig */
    ADIOI_QUOBYTEFS_WriteContig,        /* WriteContig */
    ADIOI_GEN_ReadStridedColl,  /* ReadStridedColl */
    ADIOI_GEN_WriteStridedColl, /* WriteStridedColl */
    ADIOI_GEN_SeekIndividual,   /* SeekIndividual */
    ADIOI_QUOBYTEFS_Fcntl,      /* Fcntl */
    ADIOI_GEN_SetInfo,  /* SetInfo */
    ADIOI_GEN_ReadStrided,      /* ReadStrided */
    ADIOI_GEN_WriteStrided,     /* WriteStrided */
    ADIOI_QUOBYTEFS_Close,      /* Close */
#ifdef ROMIO_HAVE_WORKING_AIO
    ADIOI_QUOBYTEFS_IreadContig,        /* IreadContig */
    ADIOI_QUOBYTEFS_IwriteContig,       /* IwriteContig */
#else
    ADIOI_FAKE_IreadContig,     /* IreadContig */
    ADIOI_FAKE_IwriteContig,    /* IwriteContig */
#endif
    ADIOI_GEN_IODone,   /* ReadDone */
    ADIOI_GEN_IODone,   /* WriteDone */
    ADIOI_GEN_IOComplete,       /* ReadComplete */
    ADIOI_GEN_IOComplete,       /* WriteComplete */
    ADIOI_GEN_IreadStrided,     /* IreadStrided */
    ADIOI_GEN_IwriteStrided,    /* IwriteStrided */
    ADIOI_QUOBYTEFS_Flush,      /* Flush */
    ADIOI_QUOBYTEFS_Resize,     /* Resize */
    ADIOI_QUOBYTEFS_Delete,     /* Delete */
    ADIOI_GEN_Feature,  /* Features */
    "QUOBYTEFS:ROMIO driver for quobyte file system",
    ADIOI_GEN_IreadStridedColl, /* IreadStridedColl */
    ADIOI_GEN_IwriteStridedColl,        /* IwriteStridedColl */
    ADIOI_QUOBYTEFS_SetLock     /* SetLock */
};

static char *extract_registry(const char *filename, int *error_code)
{
    /* input: //registry.address/[volume/]path
     * output: registry.address                */
    static char myname[] = "extract_registry";
    const char *prefix = "//";
    int prefix_size = strlen(prefix);
    if (!strncmp(filename, prefix, prefix_size)) {
        *error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_FATAL, myname,
                                           __LINE__, MPI_ERR_NAME, "Invalid uri", 0);
    }
    char *extract_filename = (char *) filename + prefix_size;
    char *tmp = strchr(extract_filename, '/');
    char *registry = NULL;

    if (tmp != NULL && tmp > extract_filename) {
        size_t length = tmp - extract_filename;
        registry = strndup(extract_filename, length);
        *error_code = MPI_SUCCESS;
        return registry;
    } else {
        *error_code = ADIOI_Err_create_code(myname, filename, EINVAL);
    }
    return NULL;
}

void ADIOI_QUOBYTEFS_CreateAdapter(const char *filename, int *error_code)
{
    static char myname[] = "ADIOI_QUOBYTEFS_CreateAdapter";

    char *registry = extract_registry(filename, error_code);
    if (registry == NULL || *error_code != MPI_SUCCESS) {
        return;
    }
    const char process_name[] = "adio_ffffffff";
    char name_buffer[strlen(process_name)];
    int rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    snprintf(name_buffer, strlen(process_name), "adio_%x", rank);
    quobyte_set_process_name(name_buffer);
    int create_status = quobyte_create_adapter(registry);
    MPL_external_free(registry);
    if (create_status != EISCONN && create_status != 0) {
        *error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_FATAL, myname,
                                           __LINE__, MPI_ERR_IO,
                                           "Could not create quobyte adapter", 0);
        return;
    }
    global_quobyte_io_context = -1;
}

void ADIOI_QUOBYTEFS_DestroyAdapter()
{
    /* TODO(alexey): place holder adapter destruction,
     * for when it works as expected */
}
