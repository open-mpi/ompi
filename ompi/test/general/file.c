/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Unit test for the ompi/file layer (file.c): the MPI_File object
 * lifecycle, exercised via MPI-IO on a local temp file opened on
 * MPI_COMM_SELF (single process).  Covers open/close, amode, group,
 * name, info, view, atomicity, errhandler, plus a small write/read and
 * file delete.
 *
 * Note: the library is compiled with -DNDEBUG, so assert() is a no-op
 * here -- all verification must go through test_verify().
 */

#include "ompi_config.h"

#include <stdio.h>
#include <string.h>

#include "support.h"

#include "mpi.h"

#define TEST_FILE_NAME "ompi_file_test.dat"

static int file_eh_calls = 0;

static void file_errhandler(MPI_File *fh, int *code, ...)
{
    (void) fh; (void) code;
    ++file_eh_calls;
}

int main(int argc, char *argv[])
{
    test_init("ompi file");

    int rc = MPI_Init(&argc, &argv);
    test_verify("MPI_Init succeeds", MPI_SUCCESS == rc);

    /* Make MPI-IO errors return rather than abort the test. */
    MPI_File_set_errhandler(MPI_FILE_NULL, MPI_ERRORS_RETURN);

    MPI_File fh = MPI_FILE_NULL;
    rc = MPI_File_open(MPI_COMM_SELF, TEST_FILE_NAME,
                       MPI_MODE_CREATE | MPI_MODE_RDWR, MPI_INFO_NULL, &fh);
    test_verify("File_open succeeds", MPI_SUCCESS == rc && MPI_FILE_NULL != fh);
    if (MPI_SUCCESS != rc || MPI_FILE_NULL == fh) {
        /*
         * The open failure has already been recorded above (this test
         * fails -- it does not skip).  Without a valid file handle the
         * remaining operations would dereference MPI_FILE_NULL and (for
         * the non-file calls below, e.g. on the resulting group) invoke
         * MPI_COMM_WORLD's default, fatal error handler -- aborting the
         * process instead of reporting a clean failure.  Stop here so the
         * recorded failure is what surfaces, after emitting a hint about a
         * common environmental cause.
         */
        char errstr[MPI_MAX_ERROR_STRING] = {0};
        int errlen = 0;
        MPI_Error_string(rc, errstr, &errlen);
        fprintf(stderr,
                "\n[ompi file test] MPI_File_open failed: %s\n"
                "[ompi file test] HINT: in a restricted/sandboxed environment "
                "(e.g. some LLM coding\n"
                "[ompi file test] harnesses), notably on macOS, ompio's default "
                "shared-file-pointer\n"
                "[ompi file test] component \"sm\" cannot create the POSIX named "
                "semaphore it needs\n"
                "[ompi file test] (sem_open -> EPERM).  To check whether that is "
                "the cause, re-run\n"
                "[ompi file test] with the sm component disabled:\n"
                "[ompi file test]     OMPI_MCA_sharedfp=^sm <test-program>\n"
                "[ompi file test] If that makes the test pass, the failure is an "
                "environment\n"
                "[ompi file test] limitation, not an Open MPI bug.\n\n",
                errstr);
        int r = test_finalize();
        MPI_Finalize();
        return r;
    }

    int amode = 0;
    rc = MPI_File_get_amode(fh, &amode);
    test_verify("File_get_amode succeeds", MPI_SUCCESS == rc);
    test_verify("amode includes MPI_MODE_RDWR", 0 != (amode & MPI_MODE_RDWR));

    MPI_Group grp = MPI_GROUP_NULL;
    rc = MPI_File_get_group(fh, &grp);
    test_verify("File_get_group succeeds", MPI_SUCCESS == rc);
    int gsize = -1;
    MPI_Group_size(grp, &gsize);
    test_verify("file group size is 1", 1 == gsize);
    MPI_Group_free(&grp);

    /* custom file error handler via call_errhandler */
    MPI_Errhandler eh = MPI_ERRHANDLER_NULL;
    rc = MPI_File_create_errhandler(file_errhandler, &eh);
    test_verify("File_create_errhandler succeeds", MPI_SUCCESS == rc);
    rc = MPI_File_set_errhandler(fh, eh);
    test_verify("File_set_errhandler succeeds", MPI_SUCCESS == rc);
    MPI_Errhandler got = MPI_ERRHANDLER_NULL;
    MPI_File_get_errhandler(fh, &got);
    test_verify("File_get_errhandler returns the one we set", got == eh);
    if (MPI_ERRHANDLER_NULL != got) {
        MPI_Errhandler_free(&got);
    }
    file_eh_calls = 0;
    MPI_File_call_errhandler(fh, MPI_ERR_OTHER);
    test_verify("custom file errhandler was invoked", 1 == file_eh_calls);
    /* restore returning handler before doing real I/O */
    MPI_File_set_errhandler(fh, MPI_ERRORS_RETURN);
    MPI_Errhandler_free(&eh);

    /* set_view / get_view */
    rc = MPI_File_set_view(fh, 0, MPI_INT, MPI_INT, "native", MPI_INFO_NULL);
    test_verify("File_set_view succeeds", MPI_SUCCESS == rc);
    MPI_Offset disp = -1;
    MPI_Datatype etype = MPI_DATATYPE_NULL, ftype = MPI_DATATYPE_NULL;
    char datarep[MPI_MAX_DATAREP_STRING];
    rc = MPI_File_get_view(fh, &disp, &etype, &ftype, datarep);
    test_verify("File_get_view succeeds", MPI_SUCCESS == rc);
    test_verify("view datarep is native", 0 == strcmp(datarep, "native"));

    /* write then read back */
    int wbuf[8] = {0, 1, 2, 3, 4, 5, 6, 7};
    MPI_Status status;
    rc = MPI_File_write_at(fh, 0, wbuf, 8, MPI_INT, &status);
    test_verify("File_write_at succeeds", MPI_SUCCESS == rc);

    int rbuf[8] = {0};
    rc = MPI_File_read_at(fh, 0, rbuf, 8, MPI_INT, &status);
    test_verify("File_read_at succeeds", MPI_SUCCESS == rc);
    test_verify("file data round-trips", 0 == memcmp(wbuf, rbuf, sizeof(wbuf)));

    MPI_Offset size = -1;
    rc = MPI_File_get_size(fh, &size);
    test_verify("File_get_size succeeds", MPI_SUCCESS == rc);
    test_verify("file size covers what we wrote", size >= (MPI_Offset) sizeof(wbuf));

    /* atomicity */
    rc = MPI_File_set_atomicity(fh, 1);
    test_verify("File_set_atomicity succeeds", MPI_SUCCESS == rc);
    int atom = -1;
    MPI_File_get_atomicity(fh, &atom);
    test_verify("atomicity reads back as set", 1 == atom);

    /* info */
    MPI_Info info = MPI_INFO_NULL;
    rc = MPI_File_get_info(fh, &info);
    test_verify("File_get_info succeeds", MPI_SUCCESS == rc && MPI_INFO_NULL != info);
    if (MPI_INFO_NULL != info) {
        MPI_Info_free(&info);
    }

    /* seek / position */
    rc = MPI_File_seek(fh, 0, MPI_SEEK_SET);
    test_verify("File_seek succeeds", MPI_SUCCESS == rc);
    MPI_Offset pos = -1;
    MPI_File_get_position(fh, &pos);
    test_verify("File_get_position after seek-to-0 is 0", 0 == pos);

    rc = MPI_File_close(&fh);
    test_verify("File_close succeeds", MPI_SUCCESS == rc);
    test_verify("File_close NULLs the handle", MPI_FILE_NULL == fh);

    rc = MPI_File_delete(TEST_FILE_NAME, MPI_INFO_NULL);
    test_verify("File_delete succeeds", MPI_SUCCESS == rc);

    int r = test_finalize();
    MPI_Finalize();
    return r;
}
