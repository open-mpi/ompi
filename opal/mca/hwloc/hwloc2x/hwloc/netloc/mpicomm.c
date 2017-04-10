/*
 * Copyright © 2016-2017 Inria.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 * See COPYING in top-level directory.
 *
 * $HEADER$
 */


#define _GNU_SOURCE         /* See feature_test_macros(7) */
#include <stdio.h>
#include <stdlib.h>

#include <netloc.h>
#include <private/netloc.h>

int netloc_build_comm_mat(char *filename, int *pn, double ***pmat)
{
    FILE *input = fopen(filename, "r");

    if (!input ) {
        perror("fopen");
        return NETLOC_ERROR;
    }
    char *line = NULL;
    size_t linesize = 0;

    char *ptr= NULL;
    int i,j;

    j = -1;
    i = 0;

    /* Get the number of elements in a line to find the size of the matrix */
    netloc_line_get(&line, &linesize, input);
    int n = 0;
    char *remain_line = line;
    while ((ptr = netloc_line_get_next_token(&remain_line, ' '))) {
        if (!strlen(ptr))
            break;
        n++;
    }
    rewind(input);

    if (!n) {
        goto error;
    }

    double *mat_values = (double *)malloc(sizeof(double[n*n]));
    double **mat = (double **)malloc(sizeof(double *[n]));
    for (int i = 0; i < n; i++) {
        mat[i] = &mat_values[i*n];
    }

    while (netloc_line_get(&line, &linesize, input) != -1) {
        char *remain_line = line;
        j = 0;
        while ((ptr = netloc_line_get_next_token(&remain_line, ' '))){
            if (!strlen(ptr))
                break;
            mat[i][j] = atof(ptr);
            if (mat[i][j] < 0) {
                fprintf(stderr, "Warning: negative value in comm matrix "
                        "(mat[%d][%d] = %f)\n", i, j, mat[i][j]);
            }
            j++;
        }
        if (j != n) {
            fprintf(stderr, "Error at %d %d (%d!=%d). "
                    "Too many columns for %s\n", i, j, j, n, filename);
            goto error;
        }
        i++;
    }

    if (i != n) {
        fprintf(stderr,"Error at %d %d. Too many rows for %s\n",
                i, j, filename);
        goto error;
    }

    fclose (input);

    *pn = n;
    *pmat = mat;

    free(line);
    return NETLOC_SUCCESS;

error:
    free(line);
    free(mat_values);
    free(mat);
    *pmat = NULL;
    *pn = 0;
    fclose (input);
    return NETLOC_ERROR;
}
