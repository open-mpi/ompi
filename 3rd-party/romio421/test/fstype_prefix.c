/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>     /* strcmp() */
#include <libgen.h>     /* basename() */
#include <unistd.h>     /* access() */
#include <romioconf.h>

#include <mpi.h>

/* set a file system type prefix known to ROMIO which is enabled at configure */
static const char *enabled_prefix =
#ifdef ROMIO_UFS
    "ufs:";
#elif defined(ROMIO_NFS)
    "nfs:";
#elif defined(ROMIO_XFS)
    "xfs:";
#elif defined(ROMIO_PVFS2)
    "pvfs2:";
#elif defined(ROMIO_GPFS)
    "gpfs:";
#elif defined(ROMIO_PANFS)
    "panfs:";
#elif defined(ROMIO_LUSTRE)
    "lustre:";
#elif defined(ROMIO_DAOS)
    "daos:";
#elif defined(ROMIO_TESTFS)
    "testfs:";
#elif defined(ROMIO_IME)
    "ime:";
#elif defined(ROMIO_QUOBYTEFS)
    "quobyte:";
#else
    ;
#error "ROMIO: no file system is enabled"
#endif

/* set a file system type prefix known to ROMIO but not enabled at configure */
static const char *disabled_prefix =
#ifndef ROMIO_UFS
    "ufs:";
#elif !defined(ROMIO_NFS)
    "nfs:";
#elif !defined(ROMIO_XFS)
    "xfs:";
#elif !defined(ROMIO_PVFS2)
    "pvfs2:";
#elif !defined(ROMIO_GPFS)
    "gpfs:";
#elif !defined(ROMIO_PANFS)
    "panfs:";
#elif !defined(ROMIO_LUSTRE)
    "lustre:";
#elif !defined(ROMIO_DAOS)
    "daos:";
#elif !defined(ROMIO_TESTFS)
    "testfs:";
#elif !defined(ROMIO_IME)
    "ime:";
#elif !defined(ROMIO_QUOBYTEFS)
    "quobyte:";
#endif

void err_expected(int err, int exp_err)
{
    int rank, errorclass;

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Error_class(err, &errorclass);
    if (exp_err != errorclass) {
        fprintf(stderr, "rank %d: MPI error class (%d) is not expected (%d)\n", rank, errorclass,
                exp_err);

        MPI_Abort(MPI_COMM_WORLD, -1);
        exit(1);
    }
}

void err_handler(int err, char *err_msg)
{
    int rank, errorStringLen;
    char errorString[MPI_MAX_ERROR_STRING];

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Error_string(err, errorString, &errorStringLen);
    if (err_msg == NULL)
        err_msg = "";
    fprintf(stderr, "rank %d: MPI error (%s) : %s\n", rank, err_msg, errorString);
    MPI_Abort(MPI_COMM_WORLD, -1);
    exit(1);
}

int main(int argc, char **argv)
{
    int err = 0, verbose = 0, rank;
    char filename[256];
    MPI_File fh;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    /* test a file system type prefix unknown to ROMIO */
    sprintf(filename, "nosuch_fstype:%s.out", basename(argv[0]));
    if (verbose && rank == 0)
        fprintf(stdout, "Testing file name prefix (unknown to ROMIO): %s", filename);
    err = MPI_File_open(MPI_COMM_WORLD, filename, MPI_MODE_CREATE | MPI_MODE_RDWR,
                        MPI_INFO_NULL, &fh);
    if (err != MPI_SUCCESS)
        err_handler(err, "MPI_File_open()");
    err = MPI_File_close(&fh);
    if (err != MPI_SUCCESS)
        err_handler(err, "MPI_File_close()");

    if (access(filename, F_OK) != 0) {
        /* file does not exist */
        err = -1;
        goto err_out;
    }
    unlink(filename);
    if (verbose && rank == 0)
        fprintf(stdout, " ---- pass\n");

    /* test a file system type prefix known to ROMIO and enabled at configure */
    sprintf(filename, "%s%s.out", enabled_prefix, basename(argv[0]));
    if (verbose && rank == 0)
        fprintf(stdout, "Testing file name prefix (known and enabled): %s", enabled_prefix);
    err = MPI_File_open(MPI_COMM_WORLD, filename, MPI_MODE_CREATE | MPI_MODE_RDWR,
                        MPI_INFO_NULL, &fh);
    if (err != MPI_SUCCESS)
        err_handler(err, "MPI_File_open()");
    err = MPI_File_close(&fh);
    if (err != MPI_SUCCESS)
        err_handler(err, "MPI_File_close()");

    /* strip the known prefix */
    sprintf(filename, "%s.out", basename(argv[0]));
    if (access(filename, F_OK) != 0) {
        /* file does not exist */
        err = -1;
        goto err_out;
    }
    unlink(filename);
    if (verbose && rank == 0)
        fprintf(stdout, " ---- pass\n");

    /* set a known file system type prefix to ROMIO in environment variable ROMIO_FSTYPE_FORCE */
    setenv("ROMIO_FSTYPE_FORCE", enabled_prefix, 1);
    sprintf(filename, "%s.out", basename(argv[0]));
    if (verbose && rank == 0)
        fprintf(stdout, "Testing ROMIO_FSTYPE_FORCE prefix (known and enabled): %s",
                enabled_prefix);
    err = MPI_File_open(MPI_COMM_WORLD, filename, MPI_MODE_CREATE | MPI_MODE_RDWR, MPI_INFO_NULL,
                        &fh);
    if (err != MPI_SUCCESS)
        err_handler(err, "MPI_File_open()");
    err = MPI_File_close(&fh);
    if (err != MPI_SUCCESS)
        err_handler(err, "MPI_File_close()");
    if (access(filename, F_OK) != 0) {
        /* file does not exist */
        err = -1;
        goto err_out;
    }
    unlink(filename);
    if (verbose && rank == 0)
        fprintf(stdout, " ---- pass\n");

    /* test a file system type prefix known to ROMIO but disabled at configure */
    sprintf(filename, "%s%s.out", disabled_prefix, basename(argv[0]));
    if (verbose && rank == 0)
        fprintf(stdout, "Testing file name prefix (known but disabled): %s", disabled_prefix);
    err = MPI_File_open(MPI_COMM_WORLD, filename, MPI_MODE_CREATE | MPI_MODE_RDWR,
                        MPI_INFO_NULL, &fh);
    err_expected(err, MPI_ERR_IO);
    if (verbose && rank == 0)
        fprintf(stdout, " ---- pass\n");

    /* set a known file system type prefix to ROMIO in environment variable ROMIO_FSTYPE_FORCE */
    setenv("ROMIO_FSTYPE_FORCE", disabled_prefix, 1);
    sprintf(filename, "%s.out", basename(argv[0]));
    if (verbose && rank == 0)
        fprintf(stdout, "Testing ROMIO_FSTYPE_FORCE prefix (known but disabled): %s",
                disabled_prefix);
    err = MPI_File_open(MPI_COMM_WORLD, filename, MPI_MODE_CREATE | MPI_MODE_RDWR, MPI_INFO_NULL,
                        &fh);
    err_expected(err, MPI_ERR_IO);
    if (verbose && rank == 0)
        fprintf(stdout, " ---- pass\n");

    err = 0;

  err_out:
    if (rank == 0 && err == 0)
        fprintf(stdout, " No Errors\n");

    MPI_Finalize();
    return err;
}
