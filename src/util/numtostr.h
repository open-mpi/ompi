/*
 * $HEADER$
 */

#ifndef OMPI_NUMTOSTR_UTIL
#define OMPI_NUMTOSTR_UTIL

/**
 * Convert a long integer to a char* string.  The returned buffer is
 * allocated by calling malloc() and must be freed by the caller.
 *  
 *  @param num (IN)      Input number
 *  @return              String containing number (NULL on failure)
 */
char* ltostr(long num);


/**
 * Convert a double to a char* string.  The returned buffer is allocated
 * by calling malloc() and must be freed by the caller.
 *
 * @param num (IN)       Input number
 * @return               String containing number (NULL on failure)
 */
char* dtostr(double num);

#endif /* OMPI_NUMTOSTR_UTIL */
