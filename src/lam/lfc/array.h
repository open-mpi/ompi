/*
 * $HEADER$
 *
 * Copyright 2002-2003. The Regents of the University of California. This material
 * was produced under U.S. Government contract W-7405-ENG-36 for Los Alamos
 * National Laboratory, which is operated by the University of California for
 * the U.S. Department of Energy. The Government is granted for itself and
 * others acting on its behalf a paid-up, nonexclusive, irrevocable worldwide
 * license in this material to reproduce, prepare derivative works, and
 * perform publicly and display publicly. Beginning five (5) years after
 * October 10,2002 subject to additional five-year worldwide renewals, the
 * Government is granted for itself and others acting on its behalf a paid-up,
 * nonexclusive, irrevocable worldwide license in this material to reproduce,
 * prepare derivative works, distribute copies to the public, perform publicly
 * and display publicly, and to permit others to do so. NEITHER THE UNITED
 * STATES NOR THE UNITED STATES DEPARTMENT OF ENERGY, NOR THE UNIVERSITY OF
 * CALIFORNIA, NOR ANY OF THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR
 * IMPLIED, OR ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY,
 * COMPLETENESS, OR USEFULNESS OF ANY INFORMATION, APPARATUS, PRODUCT, OR
 * PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD NOT INFRINGE PRIVATELY
 * OWNED RIGHTS.
                                                                                                     
 * Additionally, this program is free software; you can distribute it and/or
 * modify it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2 of the License,
 * or any later version.  Accordingly, this program is distributed in the hope
 * that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#ifndef LAM_ARRAY_H
#define LAM_ARRAY_H

#include "include/lam_types.h"
#include "lam/lfc/object.h"

typedef struct lam_array
{
    lam_object_t    super;
    lam_object_t  **arr_items;
    size_t          arr_length;
} lam_array_t;

extern lam_class_info_t    array_cls;

void lam_arr_init(lam_array_t *arr);
void lam_arr_destroy(lam_array_t *arr);

/* initializes array with fixed length.
 * lam_arr_init() must have been called first.
 */
void lam_arr_init_with(lam_array_t *arr, size_t length);

static inline lam_object_t *lam_arr_get_item(lam_array_t *arr, int index)
{
    if ( (index >=0) && (index < arr->arr_length) )
    {
        return arr->arr_items[index];
    }
}


void lam_arr_remove_item(lam_array_t *arr, int index);
void lam_arr_set_item(lam_array_t *arr, lam_object_t *item, int index);


#endif  /* LAM_ARRAY_H */


