/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2019      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ddt_lib.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/datatype/opal_datatype_internal.h"
#include "opal/runtime/opal.h"

#include <time.h>
#include <stdlib.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <stdio.h>


static int
mca_common_ompio_decode_datatype ( ompi_datatype_t *datatype,
                                   int count,
                                   struct iovec **iov,
                                   uint32_t *iovec_count,
                                   int increment)
{
    opal_convertor_t *convertor;
    size_t remaining_length = 0;
    uint32_t i;
    uint32_t temp_count;
    struct iovec *temp_iov=NULL;
    size_t temp_data;

    convertor = opal_convertor_create( opal_local_arch, 0 );

    if (OMPI_SUCCESS != opal_convertor_prepare_for_send (convertor,
                                                         &(datatype->super),
                                                         count,
                                                         NULL)) {
        opal_output (1, "Cannot attach the datatype to a convertor\n");
        return OMPI_ERROR;
    }

    if ( 0 == datatype->super.size ) {
        *iovec_count = 0;
        *iov = NULL;
        return OMPI_SUCCESS;
    }

    remaining_length = count * datatype->super.size;

    temp_count = increment;
    temp_iov = (struct iovec*)malloc(temp_count * sizeof(struct iovec));
    if (NULL == temp_iov) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    while (0 == opal_convertor_raw(convertor, temp_iov,
                                   &temp_count, &temp_data)) {
        *iovec_count = *iovec_count + temp_count;
        *iov = (struct iovec *) realloc (*iov, *iovec_count * sizeof(struct iovec));
        if (NULL == *iov) {
            opal_output(1, "OUT OF MEMORY\n");
            free(temp_iov);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        for (i = 0 ; i < temp_count ; i++) {
            (*iov)[i+(*iovec_count-temp_count)].iov_base = temp_iov[i].iov_base;
            (*iov)[i+(*iovec_count-temp_count)].iov_len = temp_iov[i].iov_len;
        }

        remaining_length -= temp_data;
        temp_count = increment;
    }
    *iovec_count = *iovec_count + temp_count;
    if ( temp_count > 0 ) {
        *iov = (struct iovec *) realloc (*iov, *iovec_count * sizeof(struct iovec));
        if (NULL == *iov) {
            opal_output(1, "OUT OF MEMORY\n");
            free(temp_iov);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
    }
    for (i=0 ; i<temp_count ; i++) {
        (*iov)[i+(*iovec_count-temp_count)].iov_base = temp_iov[i].iov_base;
        (*iov)[i+(*iovec_count-temp_count)].iov_len = temp_iov[i].iov_len;
    }

    remaining_length -= temp_data;

    if (remaining_length != 0) {
        printf( "Not all raw description was been extracted (%lu bytes missing)\n",
                (unsigned long) remaining_length );
    }

    free (temp_iov);

    return OMPI_SUCCESS;
}

int main (int argc, char *argv[]) {
    dt_elem_desc_t descs[185] = {
        { .loop = { { 16, 0}, 2, 4, -1, 16} },
        { .elem = { { 310, 9 }, 1, 12, 1, 4} },
        { .end_loop = { {16, 1}, 2, -1,  12, 4} },
        { .elem = { { 310, 9 }, 1, 1352, 1, 64} },
        { .loop = { { 16, 0}, 2, 3, -1, 16} },
        { .elem = { { 310, 9 }, 1, 8, 1, 1408} },
        { .end_loop = { {16, 1}, 2, -1,  8, 1424} },
        { .loop = { { 16, 0}, 2, 4, -1, 16} },
        { .elem = { { 310, 9 }, 1, 12, 1, 10932} },
        { .end_loop = { {16, 1}, 2, -1,  12, 10932} },
        { .elem = { { 310, 9 }, 1, 640, 1, 10992} },
        { .elem = { { 310, 9 }, 1, 712, 1, 14248} },
        { .loop = { { 16, 0}, 2, 3, -1, 16} },
        { .elem = { { 310, 9 }, 1, 8, 1, 14952} },
        { .end_loop = { {16, 1}, 2, -1,  8, 14968} },
        { .loop = { { 16, 0}, 2, 4, -1, 16} },
        { .elem = { { 310, 9 }, 1, 12, 1, 21860} },
        { .end_loop = { {16, 1}, 2, -1,  12, 21860} },
        { .elem = { { 310, 9 }, 1, 1352, 1, 21920} },
        { .loop = { { 16, 0}, 2, 3, -1, 16} },
        { .elem = { { 310, 9 }, 1, 8, 1, 23264} },
        { .end_loop = { {16, 1}, 2, -1,  8, 23280} },
        { .loop = { { 16, 0}, 2, 4, -1, 16} },
        { .elem = { { 310, 9 }, 1, 12, 1, 30172} },
        { .end_loop = { {16, 1}, 2, -1,  12, 30172} },
        { .elem = { { 310, 9 }, 1, 192, 1, 30232} },
        { .elem = { { 310, 9 }, 1, 1160, 1, 33040} },
        { .loop = { { 16, 0}, 2, 3, -1, 16} },
        { .elem = { { 310, 9 }, 1, 8, 1, 34192} },
        { .end_loop = { {16, 1}, 2, -1,  8, 34208} },
        { .loop = { { 16, 0}, 2, 4, -1, 16} },
        { .elem = { { 310, 9 }, 1, 12, 1, 41100} },
        { .end_loop = { {16, 1}, 2, -1,  12, 41100} },
        { .elem = { { 310, 9 }, 1, 1352, 1, 41160} },
        { .loop = { { 16, 0}, 2, 3, -1, 16} },
        { .elem = { { 310, 9 }, 1, 8, 1, 42504} },
        { .end_loop = { {16, 1}, 2, -1,  8, 42520} },
        { .loop = { { 16, 0}, 2, 4, -1, 16} },
        { .elem = { { 310, 9 }, 1, 12, 1, 52028} },
        { .end_loop = { {16, 1}, 2, -1,  12, 52028} },
        { .elem = { { 310, 9 }, 1, 1352, 1, 52088} },
        { .loop = { { 16, 0}, 2, 3, -1, 16} },
        { .elem = { { 310, 9 }, 1, 8, 1, 53432} },
        { .end_loop = { {16, 1}, 2, -1,  8, 53448} },
        { .loop = { { 16, 0}, 2, 4, -1, 16} },
        { .elem = { { 310, 9 }, 1, 12, 1, 60340} },
        { .end_loop = { {16, 1}, 2, -1,  12, 60340} },
        { .elem = { { 310, 9 }, 1, 1344, 1, 60400} },
        { .loop = { { 16, 0}, 2, 4, -1, 16} },
        { .elem = { { 310, 9 }, 1, 8, 1, 64360} },
        { .end_loop = { {16, 1}, 2, -1,  8, 64360} },
        { .loop = { { 16, 0}, 2, 4, -1, 16} },
        { .elem = { { 310, 9 }, 1, 12, 1, 71268} },
        { .end_loop = { {16, 1}, 2, -1,  12, 71268} },
        { .elem = { { 310, 9 }, 1, 1352, 1, 71328} },
        { .loop = { { 16, 0}, 2, 3, -1, 16} },
        { .elem = { { 310, 9 }, 1, 8, 1, 72672} },
        { .end_loop = { {16, 1}, 2, -1,  8, 72688} },
        { .loop = { { 16, 0}, 2, 4, -1, 16} },
        { .elem = { { 310, 9 }, 1, 12, 1, 79580} },
        { .end_loop = { {16, 1}, 2, -1,  12, 79580} },
        { .elem = { { 310, 9 }, 1, 896, 1, 79640} },
        { .elem = { { 310, 9 }, 1, 456, 1, 83152} },
        { .loop = { { 16, 0}, 2, 3, -1, 16} },
        { .elem = { { 310, 9 }, 1, 8, 1, 83600} },
        { .end_loop = { {16, 1}, 2, -1,  8, 83616} },
        { .loop = { { 16, 0}, 2, 4, -1, 16} },
        { .elem = { { 310, 9 }, 1, 12, 1, 90508} },
        { .end_loop = { {16, 1}, 2, -1,  12, 90508} },
        { .elem = { { 310, 9 }, 1, 1352, 1, 90568} },
        { .loop = { { 16, 0}, 2, 3, -1, 16} },
        { .elem = { { 310, 9 }, 1, 8, 1, 91912} },
        { .end_loop = { {16, 1}, 2, -1,  8, 91928} },
        { .loop = { { 16, 0}, 2, 4, -1, 16} },
        { .elem = { { 310, 9 }, 1, 12, 1, 98820} },
        { .end_loop = { {16, 1}, 2, -1,  12, 98820} },
        { .elem = { { 310, 9 }, 1, 448, 1, 98880} },
        { .elem = { { 310, 9 }, 1, 904, 1, 101944} },
        { .loop = { { 16, 0}, 2, 3, -1, 16} },
        { .elem = { { 310, 9 }, 1, 8, 1, 102840} },
        { .end_loop = { {16, 1}, 2, -1,  8, 102856} },
        { .loop = { { 16, 0}, 2, 4, -1, 16} },
        { .elem = { { 310, 9 }, 1, 12, 1, 109748} },
        { .end_loop = { {16, 1}, 2, -1,  12, 109748} },
        { .elem = { { 310, 9 }, 1, 1352, 1, 109808} },
        { .loop = { { 16, 0}, 2, 3, -1, 16} },
        { .elem = { { 310, 9 }, 1, 8, 1, 111152} },
        { .end_loop = { {16, 1}, 2, -1,  8, 111168} },
        { .loop = { { 16, 0}, 2, 4, -1, 16} },
        { .elem = { { 310, 9 }, 1, 12, 1, 118060} },
        { .end_loop = { {16, 1}, 2, -1,  12, 118060} },
        { .elem = { { 310, 9 }, 1, 1352, 1, 120736} },
        { .loop = { { 16, 0}, 2, 3, -1, 16} },
        { .elem = { { 310, 9 }, 1, 8, 1, 122080} },
        { .end_loop = { {16, 1}, 2, -1,  8, 122096} },
        { .loop = { { 16, 0}, 2, 4, -1, 16} },
        { .elem = { { 310, 9 }, 1, 12, 1, 128988} },
        { .end_loop = { {16, 1}, 2, -1,  12, 128988} },
        { .elem = { { 310, 9 }, 1, 1352, 1, 129048} },
        { .loop = { { 16, 0}, 2, 3, -1, 16} },
        { .elem = { { 310, 9 }, 1, 8, 1, 130392} },
        { .end_loop = { {16, 1}, 2, -1,  8, 130408} },
        { .loop = { { 16, 0}, 2, 4, -1, 16} },
        { .elem = { { 310, 9 }, 1, 12, 1, 139916} },
        { .end_loop = { {16, 1}, 2, -1,  12, 139916} },
        { .elem = { { 310, 9 }, 1, 1352, 1, 139976} },
        { .loop = { { 16, 0}, 2, 3, -1, 16} },
        { .elem = { { 310, 9 }, 1, 8, 1, 141320} },
        { .end_loop = { {16, 1}, 2, -1,  8, 141336} },
        { .loop = { { 16, 0}, 2, 4, -1, 16} },
        { .elem = { { 310, 9 }, 1, 12, 1, 148228} },
        { .end_loop = { {16, 1}, 2, -1,  12, 148228} },
        { .elem = { { 310, 9 }, 1, 1152, 1, 148288} },
        { .elem = { { 310, 9 }, 1, 200, 1, 152056} },
        { .loop = { { 16, 0}, 2, 3, -1, 16} },
        { .elem = { { 310, 9 }, 1, 8, 1, 152248} },
        { .end_loop = { {16, 1}, 2, -1,  8, 152264} },
        { .loop = { { 16, 0}, 2, 4, -1, 16} },
        { .elem = { { 310, 9 }, 1, 12, 1, 159156} },
        { .end_loop = { {16, 1}, 2, -1,  12, 159156} },
        { .elem = { { 310, 9 }, 1, 1352, 1, 159216} },
        { .loop = { { 16, 0}, 2, 3, -1, 16} },
        { .elem = { { 310, 9 }, 1, 8, 1, 160560} },
        { .end_loop = { {16, 1}, 2, -1,  8, 160576} },
        { .loop = { { 16, 0}, 2, 4, -1, 16} },
        { .elem = { { 310, 9 }, 1, 12, 1, 167468} },
        { .end_loop = { {16, 1}, 2, -1,  12, 167468} },
        { .elem = { { 310, 9 }, 1, 704, 1, 167528} },
        { .elem = { { 310, 9 }, 1, 648, 1, 170848} },
        { .loop = { { 16, 0}, 2, 3, -1, 16} },
        { .elem = { { 310, 9 }, 1, 8, 1, 171488} },
        { .end_loop = { {16, 1}, 2, -1,  8, 171504} },
        { .loop = { { 16, 0}, 2, 4, -1, 16} },
        { .elem = { { 310, 9 }, 1, 12, 1, 178396} },
        { .end_loop = { {16, 1}, 2, -1,  12, 178396} },
        { .elem = { { 310, 9 }, 1, 1352, 1, 178456} },
        { .loop = { { 16, 0}, 2, 3, -1, 16} },
        { .elem = { { 310, 9 }, 1, 8, 1, 179800} },
        { .end_loop = { {16, 1}, 2, -1,  8, 179816} },
        { .loop = { { 16, 0}, 2, 4, -1, 16} },
        { .elem = { { 310, 9 }, 1, 12, 1, 186708} },
        { .end_loop = { {16, 1}, 2, -1,  12, 186708} },
        { .elem = { { 310, 9 }, 1, 256, 1, 186768} },
        { .elem = { { 310, 9 }, 1, 1096, 1, 189640} },
        { .loop = { { 16, 0}, 2, 3, -1, 16} },
        { .elem = { { 310, 9 }, 1, 8, 1, 190728} },
        { .end_loop = { {16, 1}, 2, -1,  8, 190744} },
        { .loop = { { 16, 0}, 2, 4, -1, 16} },
        { .elem = { { 310, 9 }, 1, 12, 1, 197636} },
        { .end_loop = { {16, 1}, 2, -1,  12, 197636} },
        { .elem = { { 310, 9 }, 1, 1352, 1, 197696} },
        { .loop = { { 16, 0}, 2, 3, -1, 16} },
        { .elem = { { 310, 9 }, 1, 8, 1, 199040} },
        { .end_loop = { {16, 1}, 2, -1,  8, 199056} },
        { .loop = { { 16, 0}, 2, 4, -1, 16} },
        { .elem = { { 310, 9 }, 1, 12, 1, 208564} },
        { .end_loop = { {16, 1}, 2, -1,  12, 208564} },
        { .elem = { { 310, 9 }, 1, 1352, 1, 208624} },
        { .loop = { { 16, 0}, 2, 3, -1, 16} },
        { .elem = { { 310, 9 }, 1, 8, 1, 209968} },
        { .end_loop = { {16, 1}, 2, -1,  8, 209984} },
        { .elem = { { 310, 9 }, 1, 12, 1, 216876} },
        { .elem = { { 310, 9 }, 1, 16, 1, 216936} },
        { .elem = { { 310, 9 }, 1, 16, 1, 217000} },
        { .elem = { { 310, 9 }, 1, 16, 1, 217064} },
        { .elem = { { 310, 9 }, 1, 16, 1, 217128} },
        { .elem = { { 310, 9 }, 1, 16, 1, 217192} },
        { .elem = { { 310, 9 }, 1, 16, 1, 217256} },
        { .elem = { { 310, 9 }, 1, 16, 1, 217320} },
        { .elem = { { 310, 9 }, 1, 16, 1, 217384} },
        { .elem = { { 310, 9 }, 1, 16, 1, 217448} },
        { .elem = { { 310, 9 }, 1, 16, 1, 217512} },
        { .elem = { { 310, 9 }, 1, 16, 1, 217576} },
        { .elem = { { 310, 9 }, 1, 16, 1, 217640} },
        { .elem = { { 310, 9 }, 1, 16, 1, 217704} },
        { .elem = { { 310, 9 }, 1, 16, 1, 217768} },
        { .elem = { { 310, 9 }, 1, 16, 1, 217832} },
        { .elem = { { 310, 9 }, 1, 16, 1, 217896} },
        { .elem = { { 310, 9 }, 1, 16, 1, 217960} },
        { .elem = { { 310, 9 }, 1, 16, 1, 218024} },
        { .elem = { { 310, 9 }, 1, 16, 1, 218088} },
        { .elem = { { 310, 9 }, 1, 16, 1, 218152} },
        { .elem = { { 310, 9 }, 1, 16, 1, 218216} },
        { .elem = { { 310, 9 }, 1, 8, 1, 218280} },
        { .end_loop = { {0, 1}, 184, -1,  31684, 4} },
    };
    opal_init_util(NULL, NULL);
    ompi_datatype_init();

    ompi_datatype_t * datatype = OBJ_NEW(ompi_datatype_t);

    datatype->super.flags = 3332;
    datatype->super.id = 0;
    datatype->super.bdt_used = 512;
    datatype->super.size = 31684;
    datatype->super.true_lb = 4;
    datatype->super.true_ub = 218288;
    datatype->super.lb = 0;
    datatype->super.ub = 218344;
    datatype->super.nbElems = 31684;
    datatype->super.align = 1;
    datatype->super.loops = 1146;
    datatype->super.desc.length = 3351;
    datatype->super.desc.used = 184;
    datatype->super.desc.desc = descs;
    datatype->super.opt_desc.length = 3351;
    datatype->super.opt_desc.used = 184;
    datatype->super.opt_desc.desc = descs;

    /* Get the entire raw description of the datatype in a single call */
    uint32_t iovec_count_300 = 0;
    struct iovec * iov_300 = NULL;
    mca_common_ompio_decode_datatype ( datatype, 1, &iov_300, &iovec_count_300, 300);
    /* Get the raw description of the datatype 10 elements at the time. This stresses some
     * of the execution paths in the convertor raw.
     */
    uint32_t iovec_count_10 = 0;
    struct iovec * iov_10 = NULL;
    mca_common_ompio_decode_datatype ( datatype, 1, &iov_10, &iovec_count_10, 10);
    /* Get the raw description of the datatype one element at the time. This stresses all
     * execution paths in the convertor raw.
     */
    uint32_t iovec_count_1 = 0;
    struct iovec * iov_1 = NULL;
    mca_common_ompio_decode_datatype ( datatype, 1, &iov_1, &iovec_count_1, 1);

    assert(iovec_count_300 == iovec_count_10);
    assert(iovec_count_300 == iovec_count_1);
    // assert(iov[100].iov_base == iov2[100].iov_base);
    // assert(iov[100].iov_len == iov2[100].iov_len);
    for (uint32_t i = 0; i < iovec_count_300; i++) {
        assert(iov_300[i].iov_base == iov_10[i].iov_base);
        assert(iov_300[i].iov_len == iov_10[i].iov_len);
        assert(iov_300[i].iov_base == iov_1[i].iov_base);
        assert(iov_300[i].iov_len == iov_1[i].iov_len);
    }

    return 0;
}

