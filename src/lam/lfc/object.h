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

#ifndef LAM_OBJECT_H
#define LAM_OBJECT_H

#include <stdlib.h>
#include "include/lam_types.h"
#include "lam/os/atomic.h"

/*
 *
 *      Base data structures
 *
 */

struct lam_object;

typedef void (*class_init_t)(struct lam_object *);
typedef void (*class_destroy_t)(struct lam_object *);

typedef struct lam_class_info
{
    const char          *cls_name;
    struct lam_class_info  *cls_parent;
    class_init_t         cls_init;
    class_destroy_t      cls_destroy;
} lam_class_info_t;

#define OBJECT(obj)       ((lam_object_t *)(obj))
#define CREATE_OBJECT(obj, lam_obj_type, cls_ptr)    \
    do {    \
        obj = (lam_obj_type *)malloc(sizeof(lam_obj_type));     \
        if ( obj )                                  \
        {                                           \
            OBJECT(obj)->obj_class = cls_ptr;  \
            OBJECT(obj)->obj_class->cls_init(OBJECT(obj));     \
        }                                           \
    } while (0)

/*
 *
 *      Available Classes
 *
 */
 
extern lam_class_info_t lam_object_cls;
 
/*
 *
 *      Helpful macros
 *
 */

#define SUPER(obj)  (&((obj)->super))

/* Use STATIC_INIT to initialize objects that are not
    dynamically allocated. */
#define STATIC_INIT(obj, cls_ptr) \
    do {    \
        OBJECT(&(obj))->obj_class = cls_ptr;           \
        OBJECT(&(obj))->obj_class->cls_init(OBJECT(&(obj)));     \
    } while (0)

/* super_cls should be the pointer to the obj's parent
    class info struct. */
#define SUPER_INIT(obj, super_cls)         \
        do { \
                (super_cls)->cls_init(OBJECT(obj));         \
        } while (0)

#define SUPER_DESTROY(obj, super_cls)      (super_cls)->cls_destroy(OBJECT(obj))
#define OBJ_RETAIN(obj)         if ( obj ) lam_obj_retain(OBJECT(obj))
#define OBJ_RELEASE(obj)        if ( obj ) lam_obj_release(OBJECT(obj))

typedef struct lam_object
{
    lam_class_info_t  *obj_class;
    int                obj_refcnt;
} lam_object_t;

void lam_obj_init(lam_object_t *obj);
void lam_obj_destroy(lam_object_t *obj);

/*
 returns 1 if object's class is derived from cls, otherwise 0.
 */
int  lam_obj_is_kind_of(lam_object_t *obj, lam_class_info_t *cls);

static inline int lam_obj_get_ref_count(lam_object_t *obj) 
{
    return obj->obj_refcnt;
}

static inline void lam_obj_retain(lam_object_t *obj)
{
    fetchNadd(&obj->obj_refcnt, 1);
}
                                                                                                        
static inline void lam_obj_release(lam_object_t *obj)
{
    if ( fetchNadd(&obj->obj_refcnt, -1) == 1 )
    {
        obj->obj_class->cls_destroy(obj);
    }
}

#endif /* LAM_OBJECT_H */

