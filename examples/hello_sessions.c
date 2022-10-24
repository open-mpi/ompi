#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mpi.h"

void print_error(const char *msg, int rc)
{
    char err_str[MPI_MAX_ERROR_STRING];
    int resultlen = sizeof(err_str) - 1;

    MPI_Error_string(rc, err_str,  &resultlen);
    fprintf (stderr, "%s return err code  = %d (%s)\n", msg, rc, err_str);
}

int main (int argc, char *argv[])
{
    MPI_Session session, session2;
    int rc;

    rc = MPI_Session_init (MPI_INFO_NULL, MPI_ERRORS_RETURN, &session);
    if (MPI_SUCCESS != rc) {
        print_error("Session initialization failed", rc);
        return -1;
    }

    rc = MPI_Session_finalize (&session);
    if (MPI_SUCCESS != rc) {
        print_error("Session finalize failed", rc);
        return -1;
    }

    MPI_Init(&argc, &argv);
    MPI_Finalize();

    rc = MPI_Session_init (MPI_INFO_NULL, MPI_ERRORS_RETURN, &session);
    if (MPI_SUCCESS != rc) {
        print_error("Second session initialization failed", rc);
        return -1;
    }

    rc = MPI_Session_init (MPI_INFO_NULL, MPI_ERRORS_RETURN, &session2);
    if (MPI_SUCCESS != rc) {
        print_error("Third session initialization failed", rc);
        return -1;
    }

    rc = MPI_Session_finalize (&session2);
    if (MPI_SUCCESS != rc) {
        print_error("Second session finalize failed", rc);
        return -1;
    }

    rc = MPI_Session_finalize (&session);
    if (MPI_SUCCESS != rc) {
        print_error("Third session finalize failed", rc);
        return -1;
    }

    return 0;
}
