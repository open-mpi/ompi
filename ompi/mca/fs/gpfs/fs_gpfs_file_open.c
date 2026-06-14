/*
 *  Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                          University Research and Technology
 *                          Corporation.  All rights reserved.
 *  Copyright (c) 2004-2005 The University of Tennessee and The University
 *                          of Tennessee Research Foundation.  All rights
 *                          reserved.
 *  Copyright (c) 2004-2015 High Performance Computing Center Stuttgart,
 *                          University of Stuttgart.  All rights reserved.
 *  Copyright (c) 2004-2005 The Regents of the University of California.
 *                          All rights reserved.
 *  Copyright (c) 2008-2012 University of Houston. All rights reserved.
 *  Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 *  $COPYRIGHT$
 *  
 *  Additional copyrights may follow
 *  
 *  $HEADER$
 */

#include "ompi_config.h"

#include "ompi/communicator/communicator.h"
#include "ompi/info/info.h"
#include "ompi/file/file.h"
#include "ompi/mca/fs/fs.h"
#include "ompi/mca/fs/base/base.h"
#include "ompi/mca/fcoll/fcoll.h"
#include "ompi/mca/fcoll/base/base.h"
#include "ompi/mca/fbtl/fbtl.h"
#include "ompi/mca/fbtl/base/base.h"
#include "fs_gpfs.h"

#include <sys/types.h>
#include <sys/stat.h>

#include <unistd.h>
#include <gpfs.h>
#include <fcntl.h>
#include <errno.h>
#include <gpfs_fcntl.h>
#include <stdlib.h>
#include <string.h>

struct mca_fs_gpfs_info_cache_t {
    char *use_siox_lib;
    char *gpfs_access_range;
    char *gpfs_free_range;
    char *gpfs_clear_file_cache;
    char *gpfs_cancel_hints;
    char *gpfs_set_replication;
    char *gpfs_byte_range;
    char *gpfs_restripe_data;
#ifdef HAVE_C_SIOX_H
    char *siox_access_range;
    char *siox_free_range;
    char *siox_clear_file_cache;
    char *siox_cancel_hints;
    char *siox_data_ship_start;
    char *siox_data_ship_stop;
    char *siox_set_replication;
    char *siox_byte_range;
    char *siox_restripe_data;
#endif
};

static const char *const mca_fs_gpfs_hints[] = {
    "useSIOXLib",
    "gpfsAccessRange",
    "gpfsFreeRange",
    "gpfsClearFileCache",
    "gpfsCancelHints",
    "gpfsSetReplication",
    "gpfsByteRange",
    "gpfsRestripeData",
    NULL
};

#ifdef HAVE_C_SIOX_H
static const char *const mca_fs_gpfs_siox_hints[] = {
    "sioxAccessRange",
    "sioxFreeRange",
    "sioxClearFileCache",
    "sioxCancelHints",
    "sioxDataShipStart",
    "sioxDataShipStop",
    "sioxSetReplication",
    "sioxByteRange",
    "sioxRestripeData",
    NULL
};
#endif

/*
 * GPFS hint names are intentionally registered only in the GPFS component.
 * Keep the cache keyed by the public hint strings so callback registration,
 * open-time remember logic, and final cleanup all agree on ownership without
 * teaching OMPIO common code about GPFS-specific names.
 */
static char **mca_fs_gpfs_info_cache_slot(ompio_file_t *fh, const char *key)
{
    struct mca_fs_gpfs_info_cache_t *cache;

    if (NULL == fh || NULL == fh->f_fs_ptr || NULL == key) {
        return NULL;
    }

    cache = (struct mca_fs_gpfs_info_cache_t *) fh->f_fs_ptr;
    if (0 == strcmp(key, "useSIOXLib")) {
        return &cache->use_siox_lib;
    }
    if (0 == strcmp(key, "gpfsAccessRange")) {
        return &cache->gpfs_access_range;
    }
    if (0 == strcmp(key, "gpfsFreeRange")) {
        return &cache->gpfs_free_range;
    }
    if (0 == strcmp(key, "gpfsClearFileCache")) {
        return &cache->gpfs_clear_file_cache;
    }
    if (0 == strcmp(key, "gpfsCancelHints")) {
        return &cache->gpfs_cancel_hints;
    }
    if (0 == strcmp(key, "gpfsSetReplication")) {
        return &cache->gpfs_set_replication;
    }
    if (0 == strcmp(key, "gpfsByteRange")) {
        return &cache->gpfs_byte_range;
    }
    if (0 == strcmp(key, "gpfsRestripeData")) {
        return &cache->gpfs_restripe_data;
    }
#ifdef HAVE_C_SIOX_H
    if (0 == strcmp(key, "sioxAccessRange")) {
        return &cache->siox_access_range;
    }
    if (0 == strcmp(key, "sioxFreeRange")) {
        return &cache->siox_free_range;
    }
    if (0 == strcmp(key, "sioxClearFileCache")) {
        return &cache->siox_clear_file_cache;
    }
    if (0 == strcmp(key, "sioxCancelHints")) {
        return &cache->siox_cancel_hints;
    }
    if (0 == strcmp(key, "sioxDataShipStart")) {
        return &cache->siox_data_ship_start;
    }
    if (0 == strcmp(key, "sioxDataShipStop")) {
        return &cache->siox_data_ship_stop;
    }
    if (0 == strcmp(key, "sioxSetReplication")) {
        return &cache->siox_set_replication;
    }
    if (0 == strcmp(key, "sioxByteRange")) {
        return &cache->siox_byte_range;
    }
    if (0 == strcmp(key, "sioxRestripeData")) {
        return &cache->siox_restripe_data;
    }
#endif

    return NULL;
}

static const char *mca_fs_gpfs_get_cached_info(ompio_file_t *fh, const char *key)
{
    char **slot;

    slot = mca_fs_gpfs_info_cache_slot(fh, key);
    if (NULL == slot) {
        return NULL;
    }

    return *slot;
}

static int mca_fs_gpfs_info_cache_alloc(ompio_file_t *fh)
{
    if (NULL == fh) {
        return OMPI_ERR_BAD_PARAM;
    }

    if (NULL != fh->f_fs_ptr) {
        return OMPI_SUCCESS;
    }

    fh->f_fs_ptr = calloc(1, sizeof(struct mca_fs_gpfs_info_cache_t));
    if (NULL == fh->f_fs_ptr) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    return OMPI_SUCCESS;
}

static int mca_fs_gpfs_remember_info(ompio_file_t *fh, const char *key)
{
    char **slot;
    char *copy;
    int flag;
    opal_cstring_t *value_str;

    slot = mca_fs_gpfs_info_cache_slot(fh, key);
    if (NULL == slot) {
        return OMPI_ERR_BAD_PARAM;
    }

    opal_info_get(fh->f_info, key, &value_str, &flag);
    if (!flag) {
        return OMPI_SUCCESS;
    }

    /*
     * The subscriber callback can only return borrowed/static storage.
     * Store a per-file copy of each accepted open-time value so callbacks
     * during later phases can return the actual value that was applied.
     */
    copy = strdup(value_str->string);
    OBJ_RELEASE(value_str);
    if (NULL == copy) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    free(*slot);
    *slot = copy;
    return OMPI_SUCCESS;
}

void mca_fs_gpfs_info_cache_free(ompio_file_t *file)
{
    struct mca_fs_gpfs_info_cache_t *cache;

    if (NULL == file || NULL == file->f_fs_ptr) {
        return;
    }

    cache = (struct mca_fs_gpfs_info_cache_t *) file->f_fs_ptr;
    free(cache->use_siox_lib);
    free(cache->gpfs_access_range);
    free(cache->gpfs_free_range);
    free(cache->gpfs_clear_file_cache);
    free(cache->gpfs_cancel_hints);
    free(cache->gpfs_set_replication);
    free(cache->gpfs_byte_range);
    free(cache->gpfs_restripe_data);
#ifdef HAVE_C_SIOX_H
    free(cache->siox_access_range);
    free(cache->siox_free_range);
    free(cache->siox_clear_file_cache);
    free(cache->siox_cancel_hints);
    free(cache->siox_data_ship_start);
    free(cache->siox_data_ship_stop);
    free(cache->siox_set_replication);
    free(cache->siox_byte_range);
    free(cache->siox_restripe_data);
#endif
    free(cache);
    file->f_fs_ptr = NULL;
}

static const char *mca_fs_gpfs_hint_cb(opal_infosubscriber_t *object,
                                       const char *key,
                                       const char *value)
{
    ompi_file_t *file;
    mca_common_ompio_data_t *data;
    ompio_file_t *fh;

    if (NULL == object || NULL == key || NULL == value) {
        return NULL;
    }

    file = (ompi_file_t *) object;
    data = (mca_common_ompio_data_t *) file->f_io_selected_data;
    if (NULL == data) {
        return NULL;
    }
    fh = &data->ompio_fh;

    /*
     * The existing GPFS code applies these hints only during file open.
     * Later MPI_File_set_info or MPI_File_set_view calls should not make a
     * new value public unless GPFS actually consumed it, so report the
     * cached open-time value outside the OPEN phase.
     */
    if (MCA_COMMON_OMPIO_INFO_PHASE_OPEN != fh->f_info_phase) {
        return mca_fs_gpfs_get_cached_info(fh, key);
    }

    if ('\0' == value[0]) {
        return NULL;
    }

    return value;
}

static int mca_fs_gpfs_subscribe_hints(ompio_file_t *fh, const char *const *hints)
{
    int ret;

    for (int i = 0; NULL != hints[i]; ++i) {
        ret = mca_common_ompio_info_subscribe(fh, hints[i], NULL,
                                              mca_fs_gpfs_hint_cb);
        if (OMPI_SUCCESS != ret) {
            return ret;
        }
    }

    return OMPI_SUCCESS;
}

static int mca_fs_gpfs_remember_hints(ompio_file_t *fh, const char *const *hints)
{
    int ret;

    for (int i = 0; NULL != hints[i]; ++i) {
        ret = mca_fs_gpfs_remember_info(fh, hints[i]);
        if (OMPI_SUCCESS != ret) {
            return ret;
        }
    }

    return OMPI_SUCCESS;
}

static int mca_fs_gpfs_register_info(ompio_file_t *fh)
{
    int ret;

    ret = mca_fs_gpfs_subscribe_hints(fh, mca_fs_gpfs_hints);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

#ifdef HAVE_C_SIOX_H
    ret = mca_fs_gpfs_subscribe_hints(fh, mca_fs_gpfs_siox_hints);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }
#endif

    return OMPI_SUCCESS;
}

static int mca_fs_gpfs_cache_info(ompio_file_t *fh)
{
    int ret;

    /*
     * Registration makes supported keys public; this second pass records
     * only the values that survived registration so get_info remains tied
     * to what the component accepted.
     */
    ret = mca_fs_gpfs_remember_hints(fh, mca_fs_gpfs_hints);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

#ifdef HAVE_C_SIOX_H
    ret = mca_fs_gpfs_remember_hints(fh, mca_fs_gpfs_siox_hints);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }
#endif

    return OMPI_SUCCESS;
}

int
mca_fs_gpfs_file_open (struct ompi_communicator_t *comm,
                        const char* filename,
                        int access_mode,
                        struct opal_info_t *info,
                        ompio_file_t *fh)
{
    int perm, amode;
    int ret = OMPI_SUCCESS;

    perm = mca_fs_base_get_file_perm(fh);
    amode = mca_fs_base_get_file_amode(fh->f_rank, access_mode);

    ret = mca_fs_gpfs_info_cache_alloc(fh);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    ret = mca_fs_gpfs_register_info(fh);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    ret = mca_fs_gpfs_cache_info(fh);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    if(OMPIO_ROOT == fh->f_rank) {
        fh->fd = open (filename, amode, perm);
        if ( 0 > fh->fd ) {
            ret = mca_fs_base_get_mpi_err(errno);
        }
    }

    comm->c_coll->coll_bcast ( &ret, 1, MPI_INT, 0, comm, comm->c_coll->coll_bcast_module);
    if ( OMPI_SUCCESS != ret ) {
        fh->fd = -1;
        return ret;
    }

    if (OMPIO_ROOT != fh->f_rank) {
        fh->fd = open (filename, amode, perm);
        if ( 0 > fh->fd) {
            return mca_fs_base_get_mpi_err(errno);
        }
    }

    fh->f_amode=access_mode;
    mca_fs_gpfs_file_set_info(fh, (struct ompi_info_t *) info);

    return OMPI_SUCCESS;
}
