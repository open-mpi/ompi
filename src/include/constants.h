/*
 * $HEADER$
 */

#ifndef OMPI_CONSTANTS_H
#define OMPI_CONSTANTS_H

/* error codes */
enum {
    OMPI_SUCCESS = 0,
    OMPI_ERROR = -1,
    OMPI_ERR_OUT_OF_RESOURCE = -2, /* fatal error */
    OMPI_ERR_TEMP_OUT_OF_RESOURCE = -3, /* try again later */
    OMPI_ERR_RESOURCE_BUSY = -4,
    OMPI_ERR_BAD_PARAM = -5,     /* equivalent to MPI_ERR_ARG error code */
    OMPI_ERR_RECV_LESS_THAN_POSTED = -6,
    OMPI_ERR_RECV_MORE_THAN_POSTED = -7,
    OMPI_ERR_NO_MATCH_YET = -8,
    OMPI_ERR_FATAL = -9,
    OMPI_ERR_NOT_IMPLEMENTED = -10,
    OMPI_ERR_NOT_SUPPORTED = -11,
    OMPI_ERR_INTERUPTED = -12,
    OMPI_ERR_WOULD_BLOCK = -13,
    OMPI_ERR_IN_ERRNO = -14,
    OMPI_ERR_UNREACH = -15,
    OMPI_ERR_NOT_FOUND = -16
};

#endif /* OMPI_CONSTANTS_H */

