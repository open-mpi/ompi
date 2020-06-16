/*
 * Copyright (c) 2020      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include "mpi.h"

#define print1(format...) if(0 == rank) printf(format)

int main_child(int argc, char *argv[]);

int main(int argc, char *argv[])
{
    int rank=MPI_PROC_NULL, rc;
    /* info_env and error handlers */
    char init_errh_info[MPI_MAX_INFO_VAL+1]; int flag;
    MPI_Errhandler errh;
    /* error ops */
    int eclass=MPI_SUCCESS;
    char estr[MPI_MAX_ERROR_STRING]="NOT UPDATED"; int slen;
    /* spawn params */
    char* spawn_argv[3];
    MPI_Info spawn_info;
    int spawn_err[2] = {MPI_SUCCESS};
    MPI_Comm icomm = MPI_COMM_NULL;

    /* We will verify pre-init behavior in a spawnee to avoid aborting early in
     * implementations with only partial support.
     */
    if(argc > 1 && 0 == strcmp(argv[1], "preinit-error")) {
        return main_child(argc, argv);
    }

    /* Lets assume everything goes fine until we inject our own errors, no
     * error checking */
    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    print1(
"# This test checks compliance with MPI-4 initial error handler.\n"
"# This test assumes that the command line parameter '-initial-errhandler mpi_errors_return'\n"
"# is passed to 'mpiexec', in which case, a compliant implementation will:\n"
"#   * Set the MPI_INFO_ENV key to the requested error handler.\n"
"#   * The requested handler is set on the predefined communicators MPI_COMM_SELF, MPI_COMM_WORLD,\n"
"#     and the communicator returned from MPI_COMM_GET_PARENT.\n"
"# In a high quality implementation:\n"
"#   * Errors reported from calls during, before, and after MPI_INIT and MPI_FINALIZE also invoke the\n"
"#     initial error handler.\n"
"#   * MPI_ERROR_STRING and MPI_ERROR_CLASS provide useful information before/after MPI_INIT and\n"
"#     MPI_FINALIZE respectively.\n\n");

    print1("MPI_INFO_ENV for key 'mpi_initial_errhandler'\n");
    MPI_Info_get(MPI_INFO_ENV, "mpi_initial_errhandler", MPI_MAX_INFO_VAL, init_errh_info, &flag);
    if(flag) {
        print1("  MPI-4 COMPLIANT:\tMPI_INFO_ENV value set for key 'mpi_initial_errhandler' = %s\n\n", init_errh_info);
    }
    else {
        print1("  NOT MPI-4 COMPLIANT:\tMPI_INFO_ENV has no value set for key 'mpi_initial_errhandler'\n\n");
    }

    print1("MPI_COMM_GET_ERRHANDLER:\n");
    MPI_Comm_get_errhandler(MPI_COMM_SELF, &errh);
    if(MPI_ERRORS_RETURN == errh) {
        print1("  MPI-4 COMPLIANT:\tMPI_COMM_SELF error handler set to MPI_ERRORS_RETURN.\n\n");
    }
    else
    if(MPI_ERRORS_ABORT == errh) {
        print1("  UNEXPECTED:\tMPI_COMM_SELF error handler set to MPI_ERRORS_ABORT.\n\n");
    }
    else
    if(MPI_ERRORS_ARE_FATAL == errh) {
        print1("  NOT MPI-4 COMPLIANT:\tMPI_COMM_SELF error handler set to MPI_ERRORS_ARE_FATAL.\n\n");
    }
    else {
        print1("  UNEXPECTED:\tMPI_COMM_SELF error handler is not one of the predefined ones.\n\n");
    }

    sleep(1);

    MPI_Info_create(&spawn_info);
    MPI_Info_set(spawn_info, "mpi_initial_errhandler", "mpi_errors_return");
    spawn_argv[0] = argv[0];
    spawn_argv[1] = "preinit-error";
    spawn_argv[2] = NULL;
    MPI_Comm_spawn(argv[0], &spawn_argv[1], 1, spawn_info, 0, MPI_COMM_WORLD, &icomm, spawn_err);

    /* wait for the spawnee completion before testing post-finalize error
     * handling */
    MPI_Barrier(icomm);
    MPI_Comm_disconnect(&icomm);
    sleep(2);

    /* set error handler to fatal before FINALIZE */
    rc = MPI_Comm_set_errhandler(MPI_COMM_SELF, MPI_ERRORS_ARE_FATAL);
    if(MPI_SUCCESS != rc) {
        MPI_Error_string(rc, estr, &slen);
        fprintf(stderr, "  UNEXPECTED: An error occured during MPI_COMM_SETERRHANDLER(SELF) rc=%d: %s\n", rc, estr);
        return rc;
    }
    /* FINALIZE should force reversion to the initial errhandler, so we need to
     * check again (though we did not insert errors so all should go smooth). */
    rc = MPI_Finalize();
    if(MPI_SUCCESS != rc) {
        MPI_Error_string(rc, estr, &slen);
        fprintf(stderr, "  UNEXPECTED: An error occured during MPI_FINALIZE rc=%d: %s\n", rc, estr);
        return rc;
    }

    printf("Post-finalize MPI_ERROR_STRING call:\n");
    rc = MPI_Error_string(MPI_ERR_WIN, estr, &slen);
    if(MPI_SUCCESS != rc) {
        fprintf(stderr, "  NOT MPI-4 COMPLIANT:\tpost-finalize MPI_ERROR_STRING returned %d (expected MPI_SUCCESS)\n", rc);
    }
    else if(0 == strcmp(estr, "NOT UPDATED")) {
        fprintf(stderr, "  NOT MPI-4 COMPLIANT:\tpost-finalize MPI_ERROR_STRING did not set a valid string.\n");
    }
    else {
        /* We can't further check if the error string makes sense; In any
         * case, any string is compliant, even low-quality non-informative
         * generic strings. So we just print it. */
        printf("  MPI-4 COMPLIANT:\tpost-finalize MPI_ERROR_STRING for MPI_ERR_WIN: %s\n", estr);
    }
    return 0;
}

int main_child(int argc, char *argv[]) {
    int rank=0, rc;
    MPI_Comm icomm=MPI_COMM_NULL;
    int eclass=MPI_SUCCESS;
    char estr[MPI_MAX_ERROR_STRING]="NOT UPDATED"; int slen;

    /* ERROR_CLASS and ERROR_STRING are callable before MPI_INIT */

    printf("Pre-init MPI_ERROR_CLASS call:\n");
    rc = MPI_Error_class(MPI_ERR_WIN, &eclass);
    if(MPI_SUCCESS != rc) {
        fprintf(stderr, "  NOT MPI-4 COMPLIANT:\tpre-init MPI_ERROR_CLASS returned %d (expected MPI_SUCCESS)\n", rc);
    }
    else if(MPI_ERR_WIN != eclass) {
        fprintf(stderr, "  NOT MPI-4 COMPLIANT:\tpre-init MPI_ERROR_CLASS set eclass=%d (expected %d)\n", eclass, MPI_ERR_WIN);
    }
    else {
        printf("  MPI-4 COMPLIANT:\tPre-init MPI_ERROR_CLASS\n");
    }

    print1("Pre-init MPI_ERROR_STRING call:\n");
    rc = MPI_Error_string(MPI_ERR_WIN, estr, &slen);
    if(MPI_SUCCESS != rc) {
        fprintf(stderr, "  NOT MPI-4 COMPLIANT:\tpre-init MPI_ERROR_STRING returned %d (expected MPI_SUCCESS)\n", rc);
    }
    else if(0 == strcmp(estr, "NOT UPDATED")) {
        fprintf(stderr, "  NOT MPI-4 COMPLIANT:\tpre-init MPI_ERROR_STRING did not set a valid string.\n");
    }
    else {
        /* We can't further check if the error string makes sense; In any
         * case, any string is compliant, even low-quality non-informative
         * generic strings. So we just print it. */
        printf("  MPI-4 COMPLIANT:\tPre-init MPI_ERROR_STRING for MPI_ERR_WIN: %s\n", estr);
    }

    printf("Pre-init error in a call: compliant if it does not abort\n");
    rc = MPI_Error_class(MPI_ERR_LASTCODE+1, &eclass);
    eclass = rc;
    rc = MPI_Error_string(eclass, estr, &slen);
    if(MPI_SUCCESS != rc) {
        printf("  MPI-4 COMPLIANT:\tPre-init MPI_ERROR_CLASS with erroneous arguments returned (LOW QUALITY: error code=%d caused error %d in MPI_ERROR_STRING).\n", eclass, rc);
    }
    else {
        printf("  MPI-4 COMPLIANT:\tPre-init MPI_ERROR_STRING for non-existing code returned %d: %s\n", eclass, estr);
    }

    printf("Initializing MPI and setting error handlers on predefined communicators.\n");
    rc = MPI_Init(&argc, &argv);
    if(MPI_SUCCESS != rc) {
        MPI_Error_string(rc, estr, &slen);
        fprintf(stderr, "  UNEXPECTED: An error occured during MPI_INIT rc=%d: %s\n", rc, estr);
        return rc;
    }

    /* sync-up with parent */
    MPI_Comm_get_parent(&icomm);
    rc = MPI_Comm_set_errhandler(icomm, MPI_ERRORS_ARE_FATAL);
    if(MPI_SUCCESS != rc) {
        MPI_Error_string(rc, estr, &slen);
        fprintf(stderr, "  UNEXPECTED: An error occured during MPI_COMM_SETERRHANDLER(PARENT) rc=%d: %s\n", rc, estr);
        return rc;
    }
    MPI_Barrier(icomm);
    MPI_Comm_disconnect(&icomm);

    MPI_Finalize();
    return 0;
}
