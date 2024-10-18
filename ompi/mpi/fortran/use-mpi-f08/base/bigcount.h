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
        if ((array) != (tmp_array) && NULL != (tmp_array)) { \
            free(tmp_array); \
        } \
    } while (0)

#define OMPI_FORTRAN_BIGCOUNT_ARRAY_CLEANUP_NONBLOCKING(array, tmp_array, c_request, c_ierr, idx) \
    do { \
        if (MPI_SUCCESS == (c_ierr)) { \
            ompi_coll_base_nbc_request_t* nb_request = (ompi_coll_base_nbc_request_t*)c_request; \
            if ((array) != (tmp_array) && (tmp_array) != NULL) { \
                nb_request->data.release_arrays[(idx)++] = tmp_array; \
            } \
            nb_request->data.release_arrays[idx] = NULL; \
        } else { \
            OMPI_FORTRAN_BIGCOUNT_ARRAY_CLEANUP((array), (tmp_array)); \
        } \
    } while (0)
