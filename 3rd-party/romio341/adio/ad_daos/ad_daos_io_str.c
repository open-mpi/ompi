/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "ad_daos.h"
#include "adio_extern.h"
#include <assert.h>

#include "../../mpi-io/mpioimpl.h"
#ifdef MPIO_BUILD_PROFILING
#include "../../mpi-io/mpioprof.h"
#endif
#include "mpiu_greq.h"

enum {
    DAOS_WRITE,
    DAOS_READ
};

static void
ADIOI_DAOS_StridedListIO(ADIO_File fd, const void *buf, int count,
                         MPI_Datatype datatype, int file_ptr_type,
                         ADIO_Offset offset, ADIO_Status * status,
                         MPI_Request * request, int rw_type, int *error_code);

static MPIX_Grequest_class ADIOI_DAOS_greq_class = 0;
int ADIOI_DAOS_aio_free_fn(void *extra_state);
int ADIOI_DAOS_aio_poll_fn(void *extra_state, MPI_Status * status);
int ADIOI_DAOS_aio_wait_fn(int count, void **array_of_states, double timeout, MPI_Status * status);

void ADIOI_DAOS_ReadStrided(ADIO_File fd, void *buf, int count,
                            MPI_Datatype datatype, int file_ptr_type,
                            ADIO_Offset offset, ADIO_Status * status, int *error_code)
{
    ADIOI_DAOS_StridedListIO(fd, buf, count, datatype, file_ptr_type,
                             offset, status, NULL, DAOS_READ, error_code);
    return;
}

void ADIOI_DAOS_WriteStrided(ADIO_File fd, const void *buf, int count,
                             MPI_Datatype datatype, int file_ptr_type,
                             ADIO_Offset offset, ADIO_Status * status, int *error_code)
{
    ADIOI_DAOS_StridedListIO(fd, (void *) buf, count, datatype, file_ptr_type,
                             offset, status, NULL, DAOS_WRITE, error_code);
    return;
}

void ADIOI_DAOS_IreadStrided(ADIO_File fd, void *buf, int count,
                             MPI_Datatype datatype, int file_ptr_type,
                             ADIO_Offset offset, ADIO_Request * request, int *error_code)
{
    ADIOI_DAOS_StridedListIO(fd, buf, count, datatype, file_ptr_type,
                             offset, NULL, request, DAOS_READ, error_code);
    return;
}

void ADIOI_DAOS_IwriteStrided(ADIO_File fd, const void *buf, int count,
                              MPI_Datatype datatype, int file_ptr_type,
                              ADIO_Offset offset, MPI_Request * request, int *error_code)
{
    ADIOI_DAOS_StridedListIO(fd, (void *) buf, count, datatype, file_ptr_type,
                             offset, NULL, request, DAOS_WRITE, error_code);
    return;
}


static void
ADIOI_DAOS_StridedListIO(ADIO_File fd, const void *buf, int count,
                         MPI_Datatype datatype, int file_ptr_type,
                         ADIO_Offset offset, ADIO_Status * status,
                         MPI_Request * request, int rw_type, int *error_code)
{
    ADIOI_Flatlist_node *flat_buf, *flat_file;
    int i, j, k, fwr_size = 0, st_index = 0;
    int sum, n_etypes_in_filetype, size_in_filetype;
    MPI_Count bufsize;
    int n_filetypes, etype_in_filetype;
    ADIO_Offset abs_off_in_filetype = 0;
    MPI_Count filetype_size, etype_size, buftype_size;
    MPI_Aint filetype_extent, buftype_extent;
    int buftype_is_contig, filetype_is_contig;
    ADIO_Offset off, disp, start_off;
    int flag, st_fwr_size, st_n_filetypes;
    int mem_list_count;
    int64_t file_length;
    int total_blks_to_write;
    int f_data_wrote;
    int n_write_lists;
    struct ADIO_DAOS_cont *cont = fd->fs_ptr;
    struct ADIO_DAOS_req *aio_req = NULL;
    static char myname[] = "ADIOI_DAOS_StridedListIO";
    int err_flag = 0, ret;
    int mpi_rank;

    MPI_Comm_rank(fd->comm, &mpi_rank);
    *error_code = MPI_SUCCESS;

    ADIOI_Datatype_iscontig(datatype, &buftype_is_contig);
    ADIOI_Datatype_iscontig(fd->filetype, &filetype_is_contig);

    MPI_Type_size_x(fd->filetype, &filetype_size);

    MPI_Type_extent(fd->filetype, &filetype_extent);
    MPI_Type_size_x(datatype, &buftype_size);
    MPI_Type_extent(datatype, &buftype_extent);
    etype_size = fd->etype_size;

    bufsize = buftype_size * count;


    d_sg_list_t *sgl, loc_sgl;
    d_iov_t *iovs;
    dfs_iod_t *iod, loc_iod;
    daos_range_t *rgs;
    daos_size_t *nbytes, loc_nbytes;

    if (request) {
        aio_req = (struct ADIO_DAOS_req *) ADIOI_Calloc(sizeof(struct ADIO_DAOS_req), 1);
        daos_event_init(&aio_req->daos_event, DAOS_HDL_INVAL, NULL);
        iod = &aio_req->iod;
        sgl = &aio_req->sgl;
        nbytes = &aio_req->nbytes;

        if (ADIOI_DAOS_greq_class == 0) {
            MPIX_Grequest_class_create(ADIOI_GEN_aio_query_fn,
                                       ADIOI_DAOS_aio_free_fn, MPIU_Greq_cancel_fn,
                                       ADIOI_DAOS_aio_poll_fn, ADIOI_DAOS_aio_wait_fn,
                                       &ADIOI_DAOS_greq_class);
        }
        MPIX_Grequest_class_allocate(ADIOI_DAOS_greq_class, aio_req, request);
        memcpy(&(aio_req->req), request, sizeof(MPI_Request));
    } else {
        iod = &loc_iod;
        sgl = &loc_sgl;
        nbytes = &loc_nbytes;
    }

    if (filetype_size == 0) {
#ifdef HAVE_STATUS_SET_BYTES
        if (status)
            MPIR_Status_set_bytes(status, datatype, 0);
#endif
        *error_code = MPI_SUCCESS;
        return;
    }

    if (bufsize == 0) {
#ifdef HAVE_STATUS_SET_BYTES
        if (status)
            MPIR_Status_set_bytes(status, datatype, 0);
#endif
        *error_code = MPI_SUCCESS;
        return;
    }

    /* Create Memory SGL */
    file_length = 0;
    if (!buftype_is_contig) {
        flat_buf = ADIOI_Flatten_and_find(datatype);
        mem_list_count = count * flat_buf->count;

        iovs = (d_iov_t *) ADIOI_Malloc(mem_list_count * sizeof(d_iov_t));

        k = 0;
        for (j = 0; j < count; j++) {
            for (i = 0; i < flat_buf->count; i++) {
                ADIO_Offset tmp_off;

                if (flat_buf->blocklens[i] == 0) {
                    continue;
                }
                if (file_length + flat_buf->blocklens[i] > bufsize)
                    break;

                tmp_off = ((size_t) buf + j * buftype_extent + flat_buf->indices[i]);
                file_length += flat_buf->blocklens[i];
                d_iov_set(&iovs[k++], (char *) tmp_off, flat_buf->blocklens[i]);

#ifdef D_PRINT_IO_MEM
                printf("(MEM %d) %d: off %lld len %zu\n", mpi_rank, k,
                       tmp_off, flat_buf->blocklens[i]);
#endif
            }
        }
    } else {
        k = 1;
        iovs = (d_iov_t *) ADIOI_Malloc(sizeof(d_iov_t));
        file_length = bufsize;
        d_iov_set(iovs, (void *) buf, bufsize);
#ifdef D_PRINT_IO_MEM
        printf("(MEM SINGLE) off %lld len %zu\n", buf, bufsize);
#endif
    }
    sgl->sg_nr = k;
    sgl->sg_nr_out = 0;
    sgl->sg_iovs = iovs;
    if (request)
        aio_req->iovs = iovs;

    if (filetype_is_contig) {
        n_write_lists = 1;

        if (file_ptr_type == ADIO_EXPLICIT_OFFSET)
            off = fd->disp + etype_size * offset;
        else
            off = fd->fp_ind;

        rgs = (daos_range_t *) ADIOI_Malloc(sizeof(daos_range_t));
        rgs->rg_idx = off;
        rgs->rg_len = bufsize;
#ifdef D_PRINT_IO
        printf("(%d) Single: idx %lld len %zu\n", mpi_rank, rgs->rg_idx, rgs->rg_len);
#endif
    } else {
        flat_file = ADIOI_Flatten_and_find(fd->filetype);
        disp = fd->disp;

        /* for each case - ADIO_Individual pointer or explicit, find offset
         * (file offset in bytes), n_filetypes (how many filetypes into file to
         * start), fwr_size (remaining amount of data in present file block),
         * and st_index (start point in terms of blocks in starting filetype) */
        if (file_ptr_type == ADIO_INDIVIDUAL) {
            start_off = fd->fp_ind;     /* in bytes */
            n_filetypes = -1;
            flag = 0;
            while (!flag) {
                n_filetypes++;
                for (i = 0; i < flat_file->count; i++) {
                    if (disp + flat_file->indices[i] +
                        ((ADIO_Offset) n_filetypes) * filetype_extent +
                        flat_file->blocklens[i] >= start_off) {
                        st_index = i;
                        fwr_size = disp + flat_file->indices[i] +
                            ((ADIO_Offset) n_filetypes) * filetype_extent
                            + flat_file->blocklens[i] - start_off;
                        flag = 1;
                        break;
                    }
                }
            }   /* while (!flag) */
        } /* if (file_ptr_type == ADIO_INDIVIDUAL) */
        else {
            n_etypes_in_filetype = filetype_size / etype_size;
            n_filetypes = (int) (offset / n_etypes_in_filetype);
            etype_in_filetype = (int) (offset % n_etypes_in_filetype);
            size_in_filetype = etype_in_filetype * etype_size;

            sum = 0;
            for (i = 0; i < flat_file->count; i++) {
                sum += flat_file->blocklens[i];
                if (sum > size_in_filetype) {
                    st_index = i;
                    fwr_size = sum - size_in_filetype;
                    abs_off_in_filetype = flat_file->indices[i] +
                        size_in_filetype - (sum - flat_file->blocklens[i]);
                    break;
                }
            }
            /* abs. offset in bytes in the file */
            start_off = disp + ((ADIO_Offset) n_filetypes) * filetype_extent + abs_off_in_filetype;
        }       /* else [file_ptr_type != ADIO_INDIVIDUAL] */

        st_fwr_size = fwr_size;
        st_n_filetypes = n_filetypes;

        i = 0;
        j = st_index;
        f_data_wrote = MPL_MIN(st_fwr_size, bufsize);
        n_filetypes = st_n_filetypes;

        /* determine how many blocks in file to write */
        total_blks_to_write = 1;
        if (j < (flat_file->count - 1))
            j++;
        else {
            j = 0;
            n_filetypes++;
        }

        while (f_data_wrote < bufsize) {
            f_data_wrote += flat_file->blocklens[j];
            if (flat_file->blocklens[j])
                total_blks_to_write++;
            if (j < (flat_file->count - 1))
                j++;
            else {
                j = 0;
                n_filetypes++;
            }
        }

        j = st_index;
        n_filetypes = st_n_filetypes;
        n_write_lists = total_blks_to_write;

        rgs = (daos_range_t *) ADIOI_Malloc(sizeof(daos_range_t) * n_write_lists);

#if 0
        for (i = 0; i < flat_file->count; i++)
            fprintf(stderr, "(%d) FF: %d: off %lld, len %zu\n", mpi_rank, i,
                    flat_file->indices[i], flat_file->blocklens[i]);
        fprintf(stderr, "NUM IO lists = %d\n", n_write_lists);
#endif

        for (i = 0; i < n_write_lists; i++) {
            if (!i) {
                rgs[i].rg_idx = start_off;
                rgs[i].rg_len = MPL_MIN(f_data_wrote, st_fwr_size);
#ifdef D_PRINT_IO
                printf("(%d) %d: idx %lld len %zu\n", mpi_rank, i, rgs[i].rg_idx, rgs[i].rg_len);
#endif
            } else {
                if (flat_file->blocklens[j]) {
                    rgs[i].rg_idx = disp +
                        ((ADIO_Offset) n_filetypes) * filetype_extent + flat_file->indices[j];
                    rgs[i].rg_len = flat_file->blocklens[j];
#ifdef D_PRINT_IO
                    printf("(%d) %d: idx %lld len %zu\n",
                           mpi_rank, i, rgs[i].rg_idx, rgs[i].rg_len);
#endif
                } else
                    i--;
            }

            if (j < (flat_file->count - 1))
                j++;
            else {
                j = 0;
                n_filetypes++;
            }
        }
    }

    /** set array location */
    iod->iod_nr = n_write_lists;
    iod->iod_rgs = rgs;
    if (request)
        aio_req->rgs = rgs;

    if (rw_type == DAOS_WRITE) {
        ret = dfs_writex(cont->dfs, cont->obj, iod, sgl, (request ? &aio_req->daos_event : NULL));
        if (ret != 0) {
            PRINT_MSG(stderr, "dfs_writex() failed with %d\n", ret);
            *error_code = ADIOI_DAOS_err(myname, cont->obj_name, __LINE__, ret);
            return;
        }
        *nbytes = bufsize;
    } else if (rw_type == DAOS_READ) {
        ret = dfs_readx(cont->dfs, cont->obj, iod, sgl, nbytes,
                        (request ? &aio_req->daos_event : NULL));
        if (ret != 0) {
            PRINT_MSG(stderr, "dfs_readx() failed with %d\n", ret);
            *error_code = ADIOI_DAOS_err(myname, cont->obj_name, __LINE__, ret);
            return;
        }
    }

    if (file_ptr_type == ADIO_INDIVIDUAL) {
        if (filetype_is_contig)
            fd->fp_ind += bufsize;
        else
            fd->fp_ind = rgs[n_write_lists - 1].rg_idx + rgs[n_write_lists - 1].rg_len;
    }

    if (!err_flag)
        *error_code = MPI_SUCCESS;

    fd->fp_sys_posn = -1;       /* clear this. */

#ifdef HAVE_STATUS_SET_BYTES
    if (request == NULL && status) {
        MPIR_Status_set_bytes(status, datatype, *nbytes);
    }
#endif

    if (!request) {
        ADIOI_Free(iovs);
        ADIOI_Free(rgs);
    }

    return;
}
