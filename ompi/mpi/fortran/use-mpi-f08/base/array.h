/*
 * Bigcount array conversion macros for Fortran templates.
 */

#define OMPI_FORTRAN_BIGCOUNT_ARRAY_SET(array, tmp_array, n) \
    do { \
        if (sizeof(*(array)) == sizeof(*(tmp_array))) { \
            (tmp_array) = (array); \
        } else { \
            (tmp_array) = malloc(sizeof(*tmp_array) * n); \
            for (int bigcount_array_i = 0; bigcount_array_i < n; ++bigcount_array_i) { \
                (tmp_array)[bigcount_array_i] = (array)[bigcount_array_i]; \
            } \
        } \
    } while (0)

#define OMPI_FORTRAN_BIGCOUNT_ARRAY_CLEANUP(array, tmp_array) \
    do { \
        if ((array) != (tmp_array)) { \
            free(tmp_array); \
        } \
    } while (0)
