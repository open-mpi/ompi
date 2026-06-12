/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2017 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2018 University of Houston. All rights reserved.
 * Copyright (c) 2015-2020 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016-2017 IBM Corporation. All rights reserved.
 * Copyright (c) 2024      Advanced Micro Devices, Inc. All rights reserverd.
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "ompi_config.h"
#include "fs_lustre.h"

#include <fcntl.h>
#include <unistd.h>
#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/mca/fs/fs.h"
#include "ompi/mca/fs/base/base.h"
#include "ompi/communicator/communicator.h"
#include "ompi/info/info.h"

#include <sys/ioctl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void *alloc_lum(void);
struct mca_fs_lustre_info_cache_t;
static const char *mca_fs_lustre_layout_hint_cb(opal_infosubscriber_t *object,
                                                const char *key,
                                                const char *value);
static const char *mca_fs_lustre_stripe_size_cb(opal_infosubscriber_t *object,
                                                const char *key,
                                                const char *value);
static const char *mca_fs_lustre_stripe_width_cb(opal_infosubscriber_t *object,
                                                 const char *key,
                                                 const char *value);
static const char *mca_fs_lustre_get_cached_info(ompio_file_t *fh,
                                                 const char *key);
static bool mca_fs_lustre_info_has_key(opal_infosubscriber_t *object,
                                       const char *key);
static int mca_fs_lustre_info_cache_alloc(ompio_file_t *fh);
static int mca_fs_lustre_remember_info(ompio_file_t *fh, const char *key);
static int mca_fs_lustre_remember_info_value(ompio_file_t *fh, const char *key,
                                             const char *value);
static int mca_fs_lustre_register_info(ompio_file_t *fh);
static int mca_fs_lustre_parse_int_hint(opal_info_t *info, const char *key,
                                        int *value);
static int mca_fs_lustre_set_default_hint(ompio_file_t *fh, const char *key,
                                          int value);

static void *alloc_lum(void)
{
  int v1, v3;

  v1 = sizeof(struct lov_user_md_v1) +
    LOV_MAX_STRIPE_COUNT * sizeof(struct lov_user_ost_data_v1);
  v3 = sizeof(struct lov_user_md_v3) +
    LOV_MAX_STRIPE_COUNT * sizeof(struct lov_user_ost_data_v1);

  return malloc(MAX(v1, v3));
}

struct mca_fs_lustre_info_cache_t {
    char *striping_unit;
    char *striping_factor;
    char *stripe_size;
    char *stripe_width;
};

/*
 * Lustre layout hints are consumed while creating/opening the file.  They
 * cannot be safely applied later to an already-open file, so post-open
 * callbacks return the open-time cached value instead of the new user value.
 * Returning NULL tells the subscriber layer that the new value was not
 * accepted and therefore must not become public in MPI_File_get_info.
 */
static const char *mca_fs_lustre_layout_hint_cb(opal_infosubscriber_t *object,
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

    if (MCA_COMMON_OMPIO_INFO_PHASE_OPEN != fh->f_info_phase) {
        return mca_fs_lustre_get_cached_info(fh, key);
    }

    if ('\0' == value[0]) {
        return NULL;
    }

    return value;
}

static bool mca_fs_lustre_info_has_key(opal_infosubscriber_t *object,
                                       const char *key)
{
    int flag;
    opal_cstring_t *value;

    if (NULL == object || NULL == object->s_info || NULL == key) {
        return false;
    }

    opal_info_get(object->s_info, key, &value, &flag);
    if (!flag) {
        return false;
    }

    OBJ_RELEASE(value);
    return true;
}

static const char *mca_fs_lustre_stripe_size_cb(opal_infosubscriber_t *object,
                                                const char *key,
                                                const char *value)
{
    /*
     * If both MPI-IO canonical spelling and the older OMPIO alias are
     * present, keep the canonical key public.  This preserves the existing
     * parser precedence and gives get_info a single accepted spelling.
     */
    if (mca_fs_lustre_info_has_key(object, "striping_unit")) {
        return NULL;
    }

    return mca_fs_lustre_layout_hint_cb(object, key, value);
}

static const char *mca_fs_lustre_stripe_width_cb(opal_infosubscriber_t *object,
                                                 const char *key,
                                                 const char *value)
{
    /*
     * Same rule as stripe_size: the MPI spelling wins over the historical
     * alias when both are supplied.
     */
    if (mca_fs_lustre_info_has_key(object, "striping_factor")) {
        return NULL;
    }

    return mca_fs_lustre_layout_hint_cb(object, key, value);
}

static char **mca_fs_lustre_info_cache_slot(ompio_file_t *fh, const char *key)
{
    struct mca_fs_lustre_info_cache_t *cache;

    if (NULL == fh || NULL == fh->f_fs_ptr || NULL == key) {
        return NULL;
    }

    cache = (struct mca_fs_lustre_info_cache_t *) fh->f_fs_ptr;
    if (0 == strcmp(key, "striping_unit")) {
        return &cache->striping_unit;
    }
    if (0 == strcmp(key, "striping_factor")) {
        return &cache->striping_factor;
    }
    if (0 == strcmp(key, "stripe_size")) {
        return &cache->stripe_size;
    }
    if (0 == strcmp(key, "stripe_width")) {
        return &cache->stripe_width;
    }

    return NULL;
}

static const char *mca_fs_lustre_get_cached_info(ompio_file_t *fh,
                                                 const char *key)
{
    char **slot;

    slot = mca_fs_lustre_info_cache_slot(fh, key);
    if (NULL == slot) {
        return NULL;
    }

    return *slot;
}

static int mca_fs_lustre_info_cache_alloc(ompio_file_t *fh)
{
    if (NULL == fh) {
        return OMPI_ERR_BAD_PARAM;
    }

    if (NULL != fh->f_fs_ptr) {
        return OMPI_SUCCESS;
    }

    fh->f_fs_ptr = calloc(1, sizeof(struct mca_fs_lustre_info_cache_t));
    if (NULL == fh->f_fs_ptr) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    return OMPI_SUCCESS;
}

static int mca_fs_lustre_remember_info_value(ompio_file_t *fh, const char *key,
                                             const char *value)
{
    char **slot;
    char *copy;

    if (NULL == value) {
        return OMPI_ERR_BAD_PARAM;
    }

    slot = mca_fs_lustre_info_cache_slot(fh, key);
    if (NULL == slot) {
        return OMPI_ERR_BAD_PARAM;
    }

    copy = strdup(value);
    if (NULL == copy) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    free(*slot);
    *slot = copy;
    return OMPI_SUCCESS;
}

static int mca_fs_lustre_remember_info(ompio_file_t *fh, const char *key)
{
    int flag;
    int ret;
    opal_cstring_t *value_str;

    opal_info_get(fh->f_info, key, &value_str, &flag);
    if (!flag) {
        return OMPI_SUCCESS;
    }

    ret = mca_fs_lustre_remember_info_value(fh, key, value_str->string);
    OBJ_RELEASE(value_str);
    return ret;
}

void mca_fs_lustre_info_cache_free(ompio_file_t *file)
{
    struct mca_fs_lustre_info_cache_t *cache;

    if (NULL == file || NULL == file->f_fs_ptr) {
        return;
    }

    cache = (struct mca_fs_lustre_info_cache_t *) file->f_fs_ptr;
    free(cache->striping_unit);
    free(cache->striping_factor);
    free(cache->stripe_size);
    free(cache->stripe_width);
    free(cache);
    file->f_fs_ptr = NULL;
}

static int mca_fs_lustre_register_info(ompio_file_t *fh)
{
    int ret;

    ret = mca_common_ompio_info_subscribe(fh, "striping_unit", NULL,
                                          mca_fs_lustre_layout_hint_cb);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    ret = mca_common_ompio_info_subscribe(fh, "stripe_size", NULL,
                                          mca_fs_lustre_stripe_size_cb);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    ret = mca_common_ompio_info_subscribe(fh, "striping_factor", NULL,
                                          mca_fs_lustre_layout_hint_cb);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    return mca_common_ompio_info_subscribe(fh, "stripe_width", NULL,
                                           mca_fs_lustre_stripe_width_cb);
}

static int mca_fs_lustre_parse_int_hint(opal_info_t *info, const char *key,
                                        int *value)
{
    int flag;
    opal_cstring_t *value_str;

    opal_info_get(info, key, &value_str, &flag);
    if (!flag) {
        return 0;
    }

    if (1 == sscanf(value_str->string, "%d", value)) {
        OBJ_RELEASE(value_str);
        return 1;
    }

    OBJ_RELEASE(value_str);
    return 0;
}

static int mca_fs_lustre_set_default_hint(ompio_file_t *fh, const char *key,
                                          int value)
{
    char value_string[32];
    int ret;
    int size;

    if (0 >= value) {
        return OMPI_SUCCESS;
    }

    size = snprintf(value_string, sizeof(value_string), "%d", value);
    if (0 > size || sizeof(value_string) <= (size_t) size) {
        return OMPI_ERR_VALUE_OUT_OF_BOUNDS;
    }

    ret = mca_common_ompio_info_set(fh, key, value_string);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    /*
     * MCA defaults did not come from the user's MPI_Info, so report them
     * with the canonical public name.  Cache the same value so later
     * set_info/set_view attempts cannot overwrite get_info with an
     * unapplied layout request.
     */
    return mca_fs_lustre_remember_info_value(fh, key, value_string);
}

/*
 *	file_open_lustre
 *
 *	Function:	- opens a new file
 *	Accepts:	- same arguments as MPI_File_open()
 *	Returns:	- Success if new file handle
 */

int
mca_fs_lustre_file_open (struct ompi_communicator_t *comm,
                     const char* filename,
                     int access_mode,
                     /* Info hints are applied through the OMPIO subscription
                      * path; this component reads the accepted values from
                      * fh->f_info and its private cache. */
                     struct opal_info_t *info __opal_attribute_unused__,
                     ompio_file_t *fh)
{
    int amode, perm;
    int rc, ret=OMPI_SUCCESS;
    int fs_lustre_stripe_size = -1;
    int fs_lustre_stripe_width = -1;
    char *rfilename = (char *)filename;
    struct lov_user_md *lump=NULL;

    perm = mca_fs_base_get_file_perm(fh);
    amode = mca_fs_base_get_file_amode(fh->f_rank, access_mode);

    ret = mca_fs_lustre_info_cache_alloc(fh);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    ret = mca_fs_lustre_register_info(fh);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    /*
     * Preserve whichever spelling was actually accepted from MPI_Info.
     * The parser still prefers canonical spellings over aliases to match
     * the old behavior, but aliases remain visible when they were the
     * accepted user spelling.
     */
    if (mca_fs_lustre_parse_int_hint(fh->f_info, "striping_unit",
                                     &fs_lustre_stripe_size)) {
        ret = mca_fs_lustre_remember_info(fh, "striping_unit");
        if (OMPI_SUCCESS != ret) {
            return ret;
        }
    } else {
        // internal info object name used earlier. Kept for backwards compatibility.
        if (mca_fs_lustre_parse_int_hint(fh->f_info, "stripe_size",
                                         &fs_lustre_stripe_size)) {
            ret = mca_fs_lustre_remember_info(fh, "stripe_size");
            if (OMPI_SUCCESS != ret) {
                return ret;
            }
        }
    }

    if (mca_fs_lustre_parse_int_hint(fh->f_info, "striping_factor",
                                     &fs_lustre_stripe_width)) {
        ret = mca_fs_lustre_remember_info(fh, "striping_factor");
        if (OMPI_SUCCESS != ret) {
            return ret;
        }
    } else {
        // internal info object name used earlier. Kept for backwards compatibility.
        if (mca_fs_lustre_parse_int_hint(fh->f_info, "stripe_width",
                                         &fs_lustre_stripe_width)) {
            ret = mca_fs_lustre_remember_info(fh, "stripe_width");
            if (OMPI_SUCCESS != ret) {
                return ret;
            }
        }
    }

    if (fs_lustre_stripe_size < 0) {
        fs_lustre_stripe_size = mca_fs_lustre_stripe_size;
        ret = mca_fs_lustre_set_default_hint(fh, "striping_unit",
                                             fs_lustre_stripe_size);
        if (OMPI_SUCCESS != ret) {
            return ret;
        }
    }   

    if (fs_lustre_stripe_width < 0) {
        fs_lustre_stripe_width = mca_fs_lustre_stripe_width;
        ret = mca_fs_lustre_set_default_hint(fh, "striping_factor",
                                             fs_lustre_stripe_width);
        if (OMPI_SUCCESS != ret) {
            return ret;
        }
    }

    /* Check for soft links and replace filename by the actual
       file used in case it is a soft link */
    if (mca_fs_base_is_link(filename)) {
        mca_fs_base_get_real_filename(filename, &rfilename);
        /* make sure the real file is also on a Lustre file system */
        if (LUSTRE != mca_fs_base_get_fstype(rfilename)) {
            opal_output(1, "cannot use a soft-link between a LUSTRE and non-LUSTRE file system\n");
            return OPAL_ERROR;
        }
    }
    
    /* Reset errno */
    errno = 0;
    if (OMPIO_ROOT == fh->f_rank) {
        if ( (fs_lustre_stripe_size>0 || fs_lustre_stripe_width>0) &&
             ( amode&O_CREAT)                                      && 
             ( (amode&O_RDWR)|| amode&O_WRONLY) ) {
            /* this cannot be a soft-link since we are creating the file.
               Not using rfilename here */
            llapi_file_create(filename,
                              fs_lustre_stripe_size,
                              -1, /* MSC need to change that */
                              fs_lustre_stripe_width,
                              0); /* MSC need to change that */
            
            fh->fd = open(filename, amode | O_LOV_DELAY_CREATE, perm);
        }
        else {
            fh->fd = open (filename, amode, perm);
        }

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

    lump = alloc_lum();
    if (NULL == lump) {
        fprintf(stderr,"Cannot allocate memory for extracting stripe size\n");
        return OMPI_ERROR;
    }
    rc = llapi_file_get_stripe(rfilename, lump);
    if (rc != 0) {
        opal_output(1, "get_stripe failed: %d (%s)\n", errno, strerror(errno));
        free(lump);
        return OMPI_ERROR;
    }
    fh->f_stripe_size   = lump->lmm_stripe_size;
    fh->f_stripe_count  = lump->lmm_stripe_count;
    fh->f_fs_block_size = lump->lmm_stripe_size;
    free(lump);

    if (FS_LUSTRE_LOCK_AUTO == mca_fs_lustre_lock_algorithm ||
        FS_LUSTRE_LOCK_NEVER == mca_fs_lustre_lock_algorithm ) {
        fh->f_flags |= OMPIO_LOCK_NEVER;
    }
    else if (FS_LUSTRE_LOCK_ENTIRE_FILE == mca_fs_lustre_lock_algorithm) {
        fh->f_flags |= OMPIO_LOCK_ENTIRE_FILE;
    }
    else if (FS_LUSTRE_LOCK_RANGES == mca_fs_lustre_lock_algorithm) {
        /* Nothing to be done. This is what the posix fbtl component would do
           anyway without additional information . */
    }
    else {
        opal_output ( 1, "Invalid value for mca_fs_lustre_lock_algorithm %d", mca_fs_lustre_lock_algorithm );
    }

    return OMPI_SUCCESS;
}
