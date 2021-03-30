/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "ad_daos.h"
#include <libgen.h>
#include <uuid/uuid.h>
#include <gurt/common.h>

static int parse_filename(const char *path, char **_obj_name, char **_cont_name)
{
    char *f1;
    char *f2;
    char *fname;
    char *cont_name;
    int rc = 0;

    f1 = ADIOI_Strdup(path);
    if (f1 == NULL)
        return ENOMEM;

    f2 = ADIOI_Strdup(path);
    if (f2 == NULL) {
        ADIOI_Free(f1);
        return ENOMEM;
    }

    fname = basename(f1);
    cont_name = dirname(f2);

    if (cont_name[0] == '.') {
        char *ptr;
        char cwd[PATH_MAX];

        ptr = getcwd(cwd, PATH_MAX);
        if (ptr == NULL) {
            rc = errno;
            goto out;
        }

        if (strcmp(cont_name, ".") == 0) {
            cont_name = ADIOI_Strdup(cwd);
            if (cont_name == NULL) {
                rc = ENOMEM;
                goto out;
            }
        }
        *_cont_name = cont_name;
    } else {
        *_cont_name = ADIOI_Strdup(cont_name);
        if (*_cont_name == NULL) {
            rc = ENOMEM;
            goto out;
        }
    }

    *_obj_name = ADIOI_Strdup(fname);
    if (*_obj_name == NULL) {
        rc = ENOMEM;
        goto out;
    }

  out:
    ADIOI_Free(f1);
    ADIOI_Free(f2);
    return rc;
}


static int cache_handles(struct ADIO_DAOS_cont *cont)
{
    int rc;

    cont->c = adio_daos_coh_lookup(cont->attr.da_cuuid);
    if (cont->c == NULL) {
        /** insert handle into container hashtable */
        rc = adio_daos_coh_insert(cont->attr.da_cuuid, cont->coh, &cont->c);
    } else {
        /** g2l handle not needed, already cached */
        rc = daos_cont_close(cont->coh, NULL);
        cont->coh = cont->c->open_hdl;
    }
    if (rc)
        return rc;

    cont->p = adio_daos_poh_lookup(cont->attr.da_puuid);
    if (cont->p == NULL) {
        /** insert handle into pool hashtable */
        rc = adio_daos_poh_insert(cont->attr.da_puuid, cont->poh, &cont->p);
    } else {
        /** g2l handle not needed, already cached */
        rc = daos_pool_disconnect(cont->poh, NULL);
        cont->poh = cont->p->open_hdl;
    }

    return rc;
}

static int share_cont_info(struct ADIO_DAOS_cont *cont, int rank, MPI_Comm comm)
{
    char uuid_buf[74];
    d_iov_t pool_hdl = { NULL, 0, 0 };
    d_iov_t cont_hdl = { NULL, 0, 0 };
    d_iov_t dfs_hdl = { NULL, 0, 0 };
    d_iov_t file_hdl = { NULL, 0, 0 };
    char *buf = NULL;
    uint64_t total_size = 0;
    int rc = 0;

    if (rank == 0) {
        rc = daos_pool_local2global(cont->poh, &pool_hdl);
        if (rc)
            return rc;
        rc = daos_cont_local2global(cont->coh, &cont_hdl);
        if (rc)
            return rc;
        rc = dfs_local2global(cont->dfs, &dfs_hdl);
        if (rc)
            return rc;
        rc = dfs_obj_local2global(cont->dfs, cont->obj, &file_hdl);
        if (rc)
            return rc;

        total_size = sizeof(uuid_buf) + pool_hdl.iov_buf_len + cont_hdl.iov_buf_len +
            dfs_hdl.iov_buf_len + file_hdl.iov_buf_len + sizeof(daos_size_t) * 4;
    }

    /** broadcast size to all peers */
    rc = MPI_Bcast(&total_size, 1, MPI_UINT64_T, 0, comm);
    if (rc != MPI_SUCCESS)
        return -1;

    /** allocate buffers */
    buf = ADIOI_Malloc(total_size);
    if (buf == NULL)
        return -1;

    if (rank == 0) {
        char *ptr = buf;

        uuid_unparse(cont->attr.da_puuid, ptr);
        ptr += 37;
        uuid_unparse(cont->attr.da_cuuid, ptr);
        ptr += 37;

        *((daos_size_t *) ptr) = pool_hdl.iov_buf_len;
        ptr += sizeof(daos_size_t);
        pool_hdl.iov_buf = ptr;
        pool_hdl.iov_len = pool_hdl.iov_buf_len;
        rc = daos_pool_local2global(cont->poh, &pool_hdl);
        if (rc)
            goto out;
        ptr += pool_hdl.iov_buf_len;

        *((daos_size_t *) ptr) = cont_hdl.iov_buf_len;
        ptr += sizeof(daos_size_t);
        cont_hdl.iov_buf = ptr;
        cont_hdl.iov_len = cont_hdl.iov_buf_len;
        rc = daos_cont_local2global(cont->coh, &cont_hdl);
        if (rc)
            goto out;
        ptr += cont_hdl.iov_buf_len;

        *((daos_size_t *) ptr) = dfs_hdl.iov_buf_len;
        ptr += sizeof(daos_size_t);
        dfs_hdl.iov_buf = ptr;
        dfs_hdl.iov_len = dfs_hdl.iov_buf_len;
        rc = dfs_local2global(cont->dfs, &dfs_hdl);
        if (rc)
            goto out;
        ptr += dfs_hdl.iov_buf_len;

        *((daos_size_t *) ptr) = file_hdl.iov_buf_len;
        ptr += sizeof(daos_size_t);
        file_hdl.iov_buf = ptr;
        file_hdl.iov_len = file_hdl.iov_buf_len;
        rc = dfs_obj_local2global(cont->dfs, cont->obj, &file_hdl);
        if (rc)
            goto out;
    }

    rc = MPI_Bcast(buf, total_size, MPI_BYTE, 0, comm);
    if (rc != MPI_SUCCESS)
        goto out;

    if (rank != 0) {
        char *ptr = buf;

        rc = uuid_parse(ptr, cont->attr.da_puuid);
        if (rc)
            goto out;
        ptr += 37;

        rc = uuid_parse(ptr, cont->attr.da_cuuid);
        if (rc)
            goto out;
        ptr += 37;

        pool_hdl.iov_buf_len = *((daos_size_t *) ptr);
        ptr += sizeof(daos_size_t);
        pool_hdl.iov_buf = ptr;
        pool_hdl.iov_len = pool_hdl.iov_buf_len;
        rc = daos_pool_global2local(pool_hdl, &cont->poh);
        if (rc)
            goto out;
        ptr += pool_hdl.iov_buf_len;

        cont_hdl.iov_buf_len = *((daos_size_t *) ptr);
        ptr += sizeof(daos_size_t);
        cont_hdl.iov_buf = ptr;
        cont_hdl.iov_len = cont_hdl.iov_buf_len;
        rc = daos_cont_global2local(cont->poh, cont_hdl, &cont->coh);
        if (rc)
            goto out;
        ptr += cont_hdl.iov_buf_len;

        rc = cache_handles(cont);
        if (rc)
            goto out;

        dfs_hdl.iov_buf_len = *((daos_size_t *) ptr);
        ptr += sizeof(daos_size_t);
        dfs_hdl.iov_buf = ptr;
        dfs_hdl.iov_len = dfs_hdl.iov_buf_len;
        rc = dfs_global2local(cont->poh, cont->coh, O_RDWR, dfs_hdl, &cont->dfs);
        if (rc)
            goto out;
        ptr += dfs_hdl.iov_buf_len;

        if (rank != 0) {
            if (cont->c->dfs == NULL) {
                cont->c->dfs = cont->dfs;
            } else {
                dfs_umount(cont->dfs);
                cont->dfs = cont->c->dfs;
            }
        }

        file_hdl.iov_buf_len = *((daos_size_t *) ptr);
        ptr += sizeof(daos_size_t);
        file_hdl.iov_buf = ptr;
        file_hdl.iov_len = file_hdl.iov_buf_len;
        rc = dfs_obj_global2local(cont->dfs, 0, file_hdl, &cont->obj);
        if (rc)
            goto out;
    }

  out:
    ADIOI_Free(buf);
    return rc;
}

static int get_pool_cont_uuids(const char *path, struct duns_attr_t *attr)
{
    bool bypass_duns = false;
    char *uuid_str;
    int rc;

    d_getenv_bool("DAOS_BYPASS_DUNS", &bypass_duns);

    if (!bypass_duns) {
        attr->da_no_prefix = true;
        rc = duns_resolve_path(path, attr);
        if (rc) {
            PRINT_MSG(stderr, "duns_resolve_path() failed on path %s (%d)\n", path, rc);
            return rc;
        }
        return 0;
    }

    /* use the env variables to retrieve the pool and container */
    uuid_str = getenv("DAOS_POOL");
    if (uuid_str == NULL) {
        PRINT_MSG(stderr, "Can't retrieve DAOS pool uuid\n");
        return EINVAL;
    }
    if (uuid_parse(uuid_str, attr->da_puuid) < 0) {
        PRINT_MSG(stderr, "Failed to parse pool uuid\n");
        return EINVAL;
    }

    uuid_str = getenv("DAOS_CONT");
    if (uuid_str == NULL) {
        PRINT_MSG(stderr, "Can't retrieve DAOS cont uuid\n");
        return EINVAL;
    }
    if (uuid_parse(uuid_str, attr->da_cuuid) < 0) {
        PRINT_MSG(stderr, "Failed to parse container uuid\n");
        return EINVAL;
    }

    attr->da_oclass_id = OC_UNKNOWN;
    attr->da_chunk_size = 0;

    return 0;
}

void ADIOI_DAOS_Open(ADIO_File fd, int *error_code)
{
    struct ADIO_DAOS_cont *cont = fd->fs_ptr;
    static char myname[] = "ADIOI_DAOS_OPEN";
    dfs_obj_t *parent = NULL;
    int perm, old_mask, amode;
    int rc;

    *error_code = MPI_SUCCESS;

    rc = parse_filename(fd->filename, &cont->obj_name, &cont->cont_name);
    if (rc) {
        *error_code = ADIOI_DAOS_err(myname, cont->cont_name, __LINE__, rc);
        return;
    }

    rc = get_pool_cont_uuids(cont->cont_name, &cont->attr);
    if (rc) {
        *error_code = ADIOI_DAOS_err(myname, cont->cont_name, __LINE__, rc);
        return;
    }

    /** Info object setting should override */
    if (fd->hints->fs_hints.daos.obj_class != OC_UNKNOWN)
        cont->attr.da_oclass_id = fd->hints->fs_hints.daos.obj_class;
    if (fd->hints->fs_hints.daos.chunk_size != 0)
        cont->attr.da_chunk_size = fd->hints->fs_hints.daos.chunk_size;

#if 0
    {
        char uuid_str[37];
        uuid_unparse(cont->attr.da_cuuid, uuid_str);

        fprintf(stderr, "Container Open %s %s\n", cont->cont_name, uuid_str);
        fprintf(stderr, "File %s\n", cont->obj_name);
    }
    fprintf(stderr, "chunk_size  = %d\n", cont->attr.da_chunk_size);
    fprintf(stderr, "OCLASS  = %d\n", cont->attr.da_oclass_id);
#endif

    rc = adio_daos_poh_lookup_connect(cont->attr.da_puuid, &cont->p);
    if (rc) {
        PRINT_MSG(stderr, "Failed to connect to DAOS Pool (%d)\n", rc);
        *error_code = ADIOI_DAOS_err(myname, cont->cont_name, __LINE__, rc);
        return;
    }

    cont->poh = cont->p->open_hdl;

    rc = adio_daos_coh_lookup_create(cont->poh, cont->attr.da_cuuid, O_RDWR,
                                     (fd->access_mode & ADIO_CREATE), &cont->c);
    if (rc) {
        *error_code = ADIOI_DAOS_err(myname, cont->cont_name, __LINE__, rc);
        goto err_pool;
    }

    cont->coh = cont->c->open_hdl;

    assert(cont->c->dfs);
    cont->dfs = cont->c->dfs;

    /* Set file access flags */
    amode = 0;
    if (fd->access_mode & ADIO_CREATE)
        amode = amode | O_CREAT;
    if (fd->access_mode & ADIO_RDONLY)
        amode = amode | O_RDONLY;
    if (fd->access_mode & ADIO_WRONLY)
        amode = amode | O_WRONLY;
    if (fd->access_mode & ADIO_RDWR)
        amode = amode | O_RDWR;
    if (fd->access_mode & ADIO_EXCL)
        amode = amode | O_EXCL;

    /* Set DFS permission mode + object type */
    if (fd->perm == ADIO_PERM_NULL) {
        old_mask = umask(022);
        umask(old_mask);
        perm = old_mask ^ 0666;
    } else {
        perm = fd->perm;
    }
    perm = S_IFREG | perm;

    /* Lookup the parent directory. this will be NULL in case of root */
    if (cont->attr.da_rel_path) {
        rc = dfs_lookup(cont->dfs, cont->attr.da_rel_path, amode, &parent, NULL, NULL);
        if (rc) {
            *error_code = ADIOI_DAOS_err(myname, cont->obj_name, __LINE__, rc);
            goto err_cont;
        }
    }

    rc = dfs_open(cont->dfs, parent, cont->obj_name, perm, amode,
                  cont->attr.da_oclass_id, cont->attr.da_chunk_size, NULL, &cont->obj);

    if (parent)
        dfs_release(parent);

    if (rc) {
        *error_code = ADIOI_DAOS_err(myname, cont->obj_name, __LINE__, rc);
        goto err_cont;
    }

  out:
    return;
  err_obj:
    dfs_release(cont->obj);
    if (fd->access_mode & ADIO_CREATE)
        dfs_remove(cont->dfs, NULL, cont->obj_name, true, NULL);
  err_cont:
    adio_daos_coh_release(cont->c);
    cont->c = NULL;
  err_pool:
    adio_daos_poh_release(cont->p);
    cont->p = NULL;
  err_free:
    ADIOI_Free(cont->obj_name);
    ADIOI_Free(cont->cont_name);
    goto out;
}

void ADIOI_DAOS_OpenColl(ADIO_File fd, int rank, int access_mode, int *error_code)
{
    struct ADIO_DAOS_cont *cont;
    int amode, orig_amode_wronly;
    MPI_Comm comm = fd->comm;
    int mpi_size;
    int rc;
    static char myname[] = "ADIOI_DAOS_OPENCOLL";

    ADIOI_DAOS_Init(error_code);
    if (*error_code != MPI_SUCCESS)
        return;

    MPI_Comm_size(comm, &mpi_size);

    orig_amode_wronly = access_mode;
    if (access_mode & ADIO_WRONLY) {
        access_mode = access_mode ^ ADIO_WRONLY;
        access_mode = access_mode | ADIO_RDWR;
    }
    fd->access_mode = access_mode;

    amode = 0;
    if (access_mode & ADIO_RDONLY)
        amode = DAOS_COO_RO;
    else
        amode = DAOS_COO_RW;

    cont = (struct ADIO_DAOS_cont *) ADIOI_Calloc(1, sizeof(struct ADIO_DAOS_cont));
    if (cont == NULL) {
        *error_code = MPI_ERR_NO_MEM;
        return;
    }

    fd->access_mode = access_mode;
    cont->amode = amode;
    fd->fs_ptr = cont;

    if (rank == 0) {
        (*(fd->fns->ADIOI_xxx_Open)) (fd, error_code);
        MPI_Error_class(*error_code, &rc);
    }

    if (mpi_size > 1) {
        MPI_Bcast(&rc, 1, MPI_INT, 0, comm);

        if (rank != 0) {
            if (rc)
                *error_code = MPIO_Err_create_code(MPI_SUCCESS,
                                                   MPIR_ERR_RECOVERABLE, myname,
                                                   __LINE__, rc, "File Open error", 0);
            else
                *error_code = MPI_SUCCESS;
        }
    }
    if (*error_code != MPI_SUCCESS)
        goto err_free;

    if (mpi_size > 1) {
        rc = share_cont_info(cont, rank, comm);
        if (rc) {
            *error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE, myname,
                                               __LINE__, rc, "File Open error", 0);
            goto err_free;
        }
    }

    fd->is_open = 1;
    fd->access_mode = orig_amode_wronly;

#ifdef ADIOI_MPE_LOGGING
    MPE_Log_event(ADIOI_MPE_open_b, 0, NULL);
#endif

    return;

  err_free:
    ADIOI_Free(cont);
    return;
}

void ADIOI_DAOS_Flush(ADIO_File fd, int *error_code)
{
    MPI_Barrier(fd->comm);
    *error_code = MPI_SUCCESS;
}

void ADIOI_DAOS_Delete(const char *filename, int *error_code)
{
    struct adio_daos_hdl *p, *c;
    dfs_t *dfs;
    char *obj_name, *cont_name;
    struct duns_attr_t attr = { };
    static char myname[] = "ADIOI_DAOS_DELETE";
    int rc;

    ADIOI_DAOS_Init(error_code);
    if (*error_code != MPI_SUCCESS)
        return;

    rc = parse_filename(filename, &obj_name, &cont_name);
    if (rc) {
        *error_code = MPI_ERR_NO_MEM;
        return;
    }

    rc = get_pool_cont_uuids(cont_name, &attr);
    if (rc) {
        *error_code = ADIOI_DAOS_err(myname, cont_name, __LINE__, rc);
        return;
    }

    rc = adio_daos_poh_lookup_connect(attr.da_puuid, &p);
    if (rc || p == NULL) {
        PRINT_MSG(stderr, "Failed to connect to pool\n");
        *error_code = ADIOI_DAOS_err(myname, cont_name, __LINE__, rc);
        goto out_free;
    }

    rc = adio_daos_coh_lookup_create(p->open_hdl, attr.da_cuuid, O_RDWR, false, &c);
    if (rc || c == NULL) {
        *error_code = ADIOI_DAOS_err(myname, cont_name, __LINE__, rc);
        goto out_pool;
    }

    if (c->dfs == NULL) {
        /* Mount a flat namespace on the container */
        rc = dfs_mount(p->open_hdl, c->open_hdl, O_RDWR, &dfs);
        if (rc) {
            PRINT_MSG(stderr, "Failed to mount flat namespace (%d)\n", rc);
            *error_code = ADIOI_DAOS_err(myname, obj_name, __LINE__, rc);
            goto out_cont;
        }
        c->dfs = dfs;
    }

    /* Remove the file from the flat namespace */
    rc = dfs_remove(c->dfs, NULL, obj_name, true, NULL);
    if (rc) {
        *error_code = ADIOI_DAOS_err(myname, obj_name, __LINE__, rc);
        goto out_cont;
    }

    *error_code = MPI_SUCCESS;

  out_cont:
    adio_daos_coh_release(c);
  out_pool:
    adio_daos_poh_release(p);
  out_free:
    ADIOI_Free(obj_name);
    ADIOI_Free(cont_name);
    return;
}
