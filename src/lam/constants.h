/*
 * $HEADER$
 */

#ifndef LAM_CONSTANTS_H
#define LAM_CONSTANTS_H

/* error codes */
enum {
    LAM_SUCCESS = 0,
    LAM_ERROR = -1,
    LAM_ERR_OUT_OF_RESOURCE = -2, /* fatal error */
    LAM_ERR_TEMP_OUT_OF_RESOURCE = -3, /* try again later */
    LAM_ERR_RESOURCE_BUSY = -4,
    LAM_ERR_BAD_PARAM = -5,     /* equivalent to MPI_ERR_ARG error code */
    LAM_ERR_RECV_LESS_THAN_POSTED = -6,
    LAM_ERR_RECV_MORE_THAN_POSTED = -7,
    LAM_ERR_NO_MATCH_YET = -8,
    LAM_ERR_FATAL = -9,
    LAM_ERR_NOT_IMPLEMENTED = -10,
    LAM_ERR_NOT_SUPPORTED = -11,
    LAM_ERR_INTERUPTED = -12,
    LAM_ERR_WOULD_BLOCK = -13
};

#endif /* LAM_CONSTANTS_H */

