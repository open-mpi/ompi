/*
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
#include <string.h>
#include "lam/base/array.h"

lam_class_info_t array_cls = {"lam_array_t", &object_cls, 
    (class_init_t) lam_arr_init, (class_destroy_t)lam_arr_destroy};

void lam_arr_init(lam_array_t *arr)
{
    SUPER_INIT(arr, array_cls.cls_parent);
    arr->arr_items = NULL;
    arr->arr_length = 0;
}

void lam_arr_destroy(lam_array_t *arr)
{
    free(arr->arr_items);
    SUPER_DESTROY(arr, array_cls.cls_parent);
}

void lam_arr_init_with(lam_array_t *arr, size_t length)
{
    /* initializes array with fixed length.
    lam_arr_init() must have been called first. */
    if ( !arr->arr_items )
    {
        arr->arr_items = malloc(sizeof(lam_object_t *)*length);
        arr->arr_length = length;
        bzero(arr->arr_items, sizeof(lam_object_t *)*length);
    }
}


void lam_arr_remove_item(lam_array_t *arr, int index)
{
    if ( (index >=0) && (index < arr->arr_length) )
    {
        arr->arr_items[index] = NULL;
    }
}

void lam_arr_set_item(lam_array_t *arr, lam_object_t *item, int index)
{
    if ( (index >=0) && (index < arr->arr_length) )
    {
        arr->arr_items[index] = item;
    }    
}

