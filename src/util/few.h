/*
 * $HEADER$
 */

#ifndef OMPI_FEW_H
#define OMPI_FEW_H
#ifdef __cplusplus
extern "C" {
#endif
	
/**
 *  Forks, execs, and waits for a subordinate program
 *
 * @param argv Null-terminated argument vector; argv[0] is the program
 * (same as arguments to execvp())
 *
 * @param status Upon success, will be filled with the return status
 * from waitpid(2).  The WIF* macros can be used to examine the value
 * (see waitpid(2)).
 *
 * @retval OMPI_SUCCESS If the child launched and exited.
 * @retval OMPI_ERR_IN_ERRNO If a failure occurred, errno should be
 * examined for the specific error.
 *
 * This function forks, execs, and waits for an executable to
 * complete.  The input argv must be a NULL-terminated array (perhaps
 * built with the ompi_arr_*() interface).  Upon success, OMPI_SUCCESS
 * is returned.  This function will wait either until the child
 * process has exited or waitpid() returns an error other than EINTR.
 *
 * Note that a return of OMPI_SUCCESS does \em not imply that the child
 * process exited successfully -- it simply indicates that the child
 * process exited.  The WIF* macros (see waitpid(2)) should be used to
 * examine the status to see hold the child exited.
 */
int ompi_few(char *argv[], int *status);

#ifdef __cplusplus
}
#endif
#endif /* OMPI_FEW_H */
