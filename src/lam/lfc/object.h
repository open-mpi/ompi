/*
 * $HEADER$
 */

#ifndef LAM_OBJECT_H
#define LAM_OBJECT_H

#include <stdlib.h>
#include "lam/types.h"
#include "lam/atomic.h"
#include "lam/mem/malloc.h"

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

/* Use STATIC_DESTROY to destroy an object that is not
dynamically allocated. */
#define STATIC_DESTROY(obj) \
    do {    \
        OBJECT(&(obj))->obj_class->cls_destroy(OBJECT(&(obj)));     \
    } while (0)
        
/* super_cls should be the pointer to the obj's parent
    class info struct. */
#define SUPER_INIT(obj, super_cls)         \
        do { \
                (super_cls)->cls_init(OBJECT(obj));         \
        } while (0)

#define SUPER_DESTROY(obj, super_cls)      (super_cls)->cls_destroy(OBJECT(obj))
#define OBJECT(obj) ((lam_object_t *)(obj))
#define OBJ_RETAIN(obj)         if ( obj ) lam_obj_retain(OBJECT(obj))
#define OBJ_RELEASE(obj)        if ( obj ) lam_obj_release(OBJECT(obj))

#define OBJ_CREATE(obj_type, class_info) \
((obj_type*)lam_create_object(sizeof(obj_type), class_info))


typedef struct lam_object
{
    lam_class_info_t  *obj_class;
    int                obj_refcnt;
} lam_object_t;

void lam_obj_init(lam_object_t *obj);
void lam_obj_destroy(lam_object_t *obj);


static inline lam_object_t* lam_create_object(size_t size, lam_class_info_t* class_info)
{
    lam_object_t *obj = (lam_object_t*)LAM_MALLOC(size);
    if ( NULL != obj ) {
        obj->obj_class = class_info;
        obj->obj_class->cls_init(obj);
    }
    return obj;
}

/*
 * This function is used by inline functions later in this file, and
 * it must be defined by other header files later (eg., one of the
 * atomic.h's).
 */
static inline int fetchNadd(volatile int *addr, int inc);

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
        LAM_FREE(obj);
    }
}

#endif /* LAM_OBJECT_H */

