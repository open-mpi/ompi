/*
 * $HEADER$
 */

/**
 * @file:
 *
 * Simple C-language object-oriented system with single inheritance
 * and ownership-based memory management using a retain/release model.
 *
 * A class consists of a struct and singly-instantiated "class info"
 * descriptor.  The first element of the struct must be the parent
 * class's struct.  The class descriptor must be given a well-known
 * name based upon the class struct name: If the struct is sally_t,
 * the class info descriptor should be xxx_t_class_info.
 *
 * (a) To define a class
 *
 * In a interface (.h) file, define the class.  The first element
 * should always be the parent class, and be called "super",
 * for example
 * 
 *   typedef struct sally_t sally_t;
 *   struct sally_t
 *   {
 *     parent_t super;
 *     void *first_member;
 *     ...
 *   };
 *
 *   extern lam_class_info_t sally_t_class_info;
 *
 * All classes must have a parent.
 * 
 * In an implementation (.c) file, instantiate a class info descriptor
 * for this class, and should be the name of the class with
 * "_class_info" appended:
 *
 *   lam_class_info_t sally_t_class_info = {
 *     "sally_t",
 *     CLASS_INFO(parent_t),  // pointer to parent_t_class_info
 *     sally_construct,
 *     sally_destruct
 *   };
 *
 * This variable should be publically advertised using the "extern"
 * statement in the interface file as shown above.
 *
 * sally_construct, and sally_destruct are function pointers to the
 * constructor and destructor for the class and are best defined as
 * static functions in the implementation file.
 *
 * The first thing sally_construct should do is run its parent's
 * constructor using the OBJ_CONSTRUCT_SUPER macro:
 *
 *   OBJ_CONSTRUCT_SUPER(obj, type_of_parent);
 *
 * Similarly, the las thing sally_destruct should do is run its
 * parents' destructor.
 *
 *   OBJ_DESTRUCT_SUPER(obj, type_of_parent);
 *
 * Other class methods may be added to the struct.
 *
 * (b) Class instantiation: dynamic
 *
 * To create a instance of a class (an object) use OBJ_NEW:
 *
 *   sally_t *sally = OBJ_NEW(sally_t);
 *
 * which allocates memory of sizeof(sally_t) and runs the class's
 * "init" method.
 *
 * Use OBJ_RETAIN, OBJ_RELEASE to do reference-count-based
 * memory management:
 *
 *   OBJ_RETAIN(sally);
 *   OBJ_RELEASE(sally);
 *   OBJ_RELEASE(sally);
 *
 * When the reference count reaches zero, the class's "fini" method
 * is run and the memory is freed.
 *
 * N.B. There is no explicit free/delete method for dynamic objects in
 * this model.
 *
 * (c) Class instantiation: static
 *
 * For an object with static (or stack) allocation, it is only
 * necessary to initialize the memory, which is done using
 * OBJ_CONSTRUCT:
 *
 *   sally_t sally;
 *
 *   OBJ_CONSTRUCT(&sally, sally_t);
 *
 * The retain/release model is not necessary here, but before the
 * object goes out of scope, OBJ_DESTRUCT should be run to release
 * initialized resources:
 *
 *   OBJ_DESTRUCT(&sally);
 */

#ifndef LAM_OBJECT_H
#define LAM_OBJECT_H

#include <stdlib.h>

#include "lam/types.h"
#include "lam/atomic.h"
#include "lam/mem/malloc.h"

/*
 * Class definition
 */

typedef struct lam_object_t lam_object_t;
typedef struct lam_class_info_t lam_class_info_t;
typedef void (*lam_construct_t) (lam_object_t *);
typedef void (*lam_destruct_t) (lam_object_t *);

struct lam_class_info_t {
    const char *cls_name;
    lam_class_info_t *cls_parent;
    lam_construct_t cls_construct;
    lam_destruct_t cls_destruct;
};

extern lam_class_info_t lam_object_t_class_info;


/**
 * Base object for the class system.  This is special and does not
 * follow the pattern for other classes.
 */
struct lam_object_t {
    lam_class_info_t *obj_class_info; /**< class information */
    int obj_reference_count;          /**< reference count for the class */
};

extern void lam_object_construct(lam_object_t *obj);
extern void lam_object_destruct(lam_object_t *obj);


/* Inline functions and prototypes *******************************/

/**
 * Create new object: dynamically allocate storage and run the class
 * constructor.
 *
 * Do not use this function directly: use OBJ_NEW() instead.
 *
 * @param size          Size of the object
 * @param size          Pointer to the class info struct for this class
 * @return              Pointer to the object 
 */
static inline lam_object_t *lam_new(size_t size,
                                    lam_class_info_t *
                                    class_info)
{
    lam_object_t *obj = (lam_object_t *) malloc(size);
    if (NULL != obj) {
	obj->obj_class_info = class_info;
	obj->obj_class_info->cls_construct(obj);
    }
    return obj;
}

/*
 * This function is used by inline functions later in this file, and
 * it must be defined by other header files later (eg., one of the
 * atomic.h's).
 */
static inline int fetchNadd(volatile int *addr, int inc);

/**
 * Test if object inherits from class
 *
 * @param obj           Pointer to the object
 * @param class         Class to query
 * @return              1 if the object is of, or derived from, typ
 *
 */
#define OBJ_IS_KIND_OF(obj, class) lam_obj_is_kind_of(obj, class ## _class_info)


/**
 * Test if object inherits from class.
 *
 * Do not use this function directly: use OBJ_IS_KIND_OF instead.
 *
 * @param obj           Pointer to the object
 * @param class         Class to query
 * @return              1 if the object is of, or derived from, this class
 */
static inline int lam_obj_is_kind_of(lam_object_t *obj,
				     lam_class_info_t *class_info)
{
    return 0;
}


/**
 * Return the reference count of this object.
 *
 * @param obj           Pointer to the object
 * @return              The reference count value
 */
static inline int lam_obj_get_ref_count(lam_object_t *obj)
{
    return obj->obj_reference_count;
}


/**
 * Retain an object (by incrementing its reference count)
 *
 * Do not use this function directly: use OBJ_RETAIN instead.
 *
 * @param obj           Pointer to the object
 */
static inline void lam_obj_retain(lam_object_t *obj)
{
    fetchNadd(&obj->obj_reference_count, 1);
}


/**
 * Release an object (by decrementing its reference count).  If the
 * reference count reaches zero, destruct (finalize) the object and
 * free its storage.
 *
 * Do not use this function directly: use OBJ_RELEASE instead.
 *
 * @param obj           Pointer to the object
 */
static inline void lam_obj_release(lam_object_t *obj)
{
    if (fetchNadd(&obj->obj_reference_count, -1) == 1) {
	obj->obj_class_info->cls_destruct(obj);
	free(obj);
    }
}


/*
 * Macros
 */

/**
 * Return a pointer to the object cast to the base object type
 *
 * @param obj   Pointer to the object
 * @return      Cast pointer to the object
 */
#define OBJECT(obj)     ((lam_object_t *)(obj))


/**
 * Return a pointer to the class info descriptor associated with a
 * class type.
 *
 * @param type  Name of class
 * @return      Pointer to class info descriptor
 */
#define CLASS_INFO(type)     (&(type ## _class_info))


/**
 * Create an object: dynamically allocate storage and run the class
 * constructor.
 *
 * @param type          Type (class) of the object
 * @return              Pointer to the object 
 */
#define OBJ_NEW(type)                                                  \
    ((type *) lam_new(sizeof(type), CLASS_INFO(type)))


/**
 * Retain an object (by incrementing its reference count)
 *
 * @param obj           Pointer to the object
 */
#define OBJ_RETAIN(obj)                                                \
    do {                                                               \
        if (obj) lam_obj_retain(OBJECT(obj));                          \
    } while (0)


/**
 * Release an object (by decrementing its reference count).  If the
 * reference count reaches zero, destruct (finalize) the object and
 * free its storage.
 *
 * @param obj           Pointer to the object
 */
#define OBJ_RELEASE(obj)                                               \
    do {                                                               \
        if (obj) lam_obj_release(OBJECT(obj));                         \
    } while (0)


/**
 * Construct (initialize) objects that are not dynamically allocated.
 *
 * @param obj   Pointer to the object
 * @param type  The object type
 */
#define OBJ_CONSTRUCT(obj, type)                                       \
    do {                                                               \
        OBJECT(obj)->obj_class_info = CLASS_INFO(type);                \
        OBJECT(obj)->obj_class_info->cls_construct(OBJECT(obj));       \
    } while (0)


/**
 * Destruct (finalize) an object that is not dynamically allocated.
 *
 * @param obj   Pointer to the object
 */
#define OBJ_DESTRUCT(obj)                                              \
    do {                                                               \
        OBJECT(obj)->obj_class_info->cls_destruct(OBJECT(obj));        \
    } while (0)


/**
 * Construct (initialize) the parent object
 *
 * @param obj           Pointer to the object
 * @param super_type    Type of the parent object
 */
#define OBJ_CONSTRUCT_SUPER(obj, super_type)                           \
    do {                                                               \
        CLASS_INFO(super_type)->cls_construct(OBJECT(obj));            \
    } while (0)


/**
 * Destruct (finalize) the parent object.
 *
 * @param obj           Pointer to the object
 * @param super_type    Type of the parent object
 */
#define OBJ_DESTRUCT_SUPER(obj, super_type)                            \
    do {                                                               \
        CLASS_INFO(super_type)->cls_destruct(OBJECT(obj));             \
    } while (0)

#endif				/* LAM_OBJECT_H */
