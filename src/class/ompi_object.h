/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file:
 *
 * A simple C-language object-oriented system with single inheritance
 * and ownership-based memory management using a retain/release model.
 *
 * A class consists of a struct and singly-instantiated class
 * descriptor.  The first element of the struct must be the parent
 * class's struct.  The class descriptor must be given a well-known
 * name based upon the class struct name (if the struct is sally_t,
 * the class descriptor should be sally_t_class) and must be
 * statically initialized as discussed below.
 *
 * (a) To define a class
 *
 * In a interface (.h) file, define the class.  The first element
 * should always be the parent class, for example
 * @code
 *   typedef struct sally_t sally_t;
 *   struct sally_t
 *   {
 *     parent_t parent;
 *     void *first_member;
 *     ...
 *   };
 *
 *   OBJ_CLASS_DECLARATION(sally_t);
 * @endcode
 * All classes must have a parent which is also class.
 * 
 * In an implementation (.c) file, instantiate a class descriptor for
 * the class like this:
 * @code
 *   OBJ_CLASS_INSTANCE(sally_t, parent_t, sally_construct, sally_destruct);
 * @endcode
 * This macro actually expands to 
 * @code
 *   ompi_class_t sally_t_class = {
 *     "sally_t",
 *     OBJ_CLASS(parent_t),  // pointer to parent_t_class
 *     sally_construct,
 *     sally_destruct,
 *     0, 0, NULL, NULL
 *   };
 * @endcode
 * This variable should be declared in the interface (.h) file using
 * the OBJ_CLASS_DECLARATION macro as shown above.
 *
 * sally_construct, and sally_destruct are function pointers to the
 * constructor and destructor for the class and are best defined as
 * static functions in the implementation file.  NULL pointers maybe
 * supplied instead.
 *
 * Other class methods may be added to the struct.
 *
 * (b) Class instantiation: dynamic
 *
 * To create a instance of a class (an object) use OBJ_NEW:
 * @code
 *   sally_t *sally = OBJ_NEW(sally_t);
 * @endcode
 * which allocates memory of sizeof(sally_t) and runs the class's
 * constructors.
 *
 * Use OBJ_RETAIN, OBJ_RELEASE to do reference-count-based
 * memory management:
 * @code
 *   OBJ_RETAIN(sally);
 *   OBJ_RELEASE(sally);
 *   OBJ_RELEASE(sally);
 * @endcode
 * When the reference count reaches zero, the class's destructor, and
 * those of its parents, are run and the memory is freed.
 *
 * N.B. There is no explicit free/delete method for dynamic objects in
 * this model.
 *
 * (c) Class instantiation: static
 *
 * For an object with static (or stack) allocation, it is only
 * necessary to initialize the memory, which is done using
 * OBJ_CONSTRUCT:
 * @code
 *   sally_t sally;
 *
 *   OBJ_CONSTRUCT(&sally, sally_t);
 * @endcode
 * The retain/release model is not necessary here, but before the
 * object goes out of scope, OBJ_DESTRUCT should be run to release
 * initialized resources:
 * @code
 *   OBJ_DESTRUCT(&sally);
 * @endcode
 */

#ifndef OMPI_OBJECT_H
#define OMPI_OBJECT_H

#include <assert.h>
#include <stdlib.h>

#include "include/sys/atomic.h"

/*
 * BEGIN_C_DECLS should be used at the beginning of your declarations,
 * so that C++ compilers don't mangle their names.  Use END_C_DECLS at
 * the end of C declarations.
 */
#undef BEGIN_C_DECLS
#undef END_C_DECLS
#if defined(c_plusplus) || defined(__cplusplus)
# define BEGIN_C_DECLS extern "C" {
# define END_C_DECLS }
#else
#define BEGIN_C_DECLS          /* empty */
#define END_C_DECLS            /* empty */
#endif

/* typedefs ***********************************************************/

typedef struct ompi_object_t ompi_object_t;
typedef struct ompi_class_t ompi_class_t;
typedef void (*ompi_construct_t) (ompi_object_t *);
typedef void (*ompi_destruct_t) (ompi_object_t *);


/* types **************************************************************/

/**
 * Class descriptor.
 *
 * There should be a single instance of this descriptor for each class
 * definition.
 */
struct ompi_class_t {
    const char *cls_name;           /**< symbolic name for class */
    ompi_class_t *cls_parent;       /**< parent class descriptor */
    ompi_construct_t cls_construct; /**< class constructor */
    ompi_destruct_t cls_destruct;   /**< class destructor */
    int cls_initialized;            /**< is class initialized */
    int cls_depth;                  /**< depth of class hierarchy tree */
    ompi_construct_t *cls_construct_array;
                                    /**< array of parent class constructors */
    ompi_destruct_t *cls_destruct_array;
                                    /**< array of parent class destructors */
};

/**
 * Base object.
 *
 * This is special and does not follow the pattern for other classes.
 */
struct ompi_object_t {
    ompi_class_t *obj_class;            /**< class descriptor */
    volatile int obj_reference_count;   /**< reference count */
#if OMPI_ENABLE_DEBUG
   const char* cls_init_file_name;        /**< In debug mode store the file where the object get contructed */
   int   cls_init_lineno;           /**< In debug mode store the line number where the object get contructed */
#endif  /* OMPI_ENABLE_DEBUG */
};

/* macros ************************************************************/

/**
 * Return a pointer to the class descriptor associated with a
 * class type.
 *
 * @param NAME          Name of class
 * @return              Pointer to class descriptor
 */
#define OBJ_CLASS(NAME)     (&(NAME ## _class))


/**
 * Static initializer for a class descriptor
 *
 * @param NAME          Name of class
 * @param PARENT        Name of parent class
 * @param CONSTRUCTOR   Pointer to constructor
 * @param DESTRUCTOR    Pointer to destructor
 *
 * Put this in NAME.c
 */
#define OBJ_CLASS_INSTANCE(NAME, PARENT, CONSTRUCTOR, DESTRUCTOR)       \
    ompi_class_t NAME ## _class = {                                     \
        # NAME,                                                         \
        OBJ_CLASS(PARENT),                                              \
        (ompi_construct_t) CONSTRUCTOR,                                 \
        (ompi_destruct_t) DESTRUCTOR,                                   \
        0, 0, NULL, NULL                                                \
    }


/**
 * Declaration for class descriptor
 *
 * @param NAME          Name of class
 *
 * Put this in NAME.h
 */
#define OBJ_CLASS_DECLARATION(NAME)             \
    extern ompi_class_t NAME ## _class


/**
 * Create an object: dynamically allocate storage and run the class
 * constructor.
 *
 * @param type          Type (class) of the object
 * @return              Pointer to the object 
 */
static inline ompi_object_t *ompi_obj_new(size_t size, ompi_class_t * cls);
#if OMPI_ENABLE_DEBUG
static inline ompi_object_t *ompi_obj_new_debug(size_t obj_size, ompi_class_t* type, const char* file, int line)
{
    ompi_object_t* object = ompi_obj_new(obj_size, type);
    object->cls_init_file_name = file;
    object->cls_init_lineno = line;
    return object;
}
#define OBJ_NEW(type)                                   \
    ((type *)ompi_obj_new_debug(sizeof(type), OBJ_CLASS(type), __FILE__, __LINE__))
#else
#define OBJ_NEW(type)                                   \
    ((type *) ompi_obj_new(sizeof(type), OBJ_CLASS(type)))
#endif  /* OMPI_ENABLE_DEBUG */

/**
 * Retain an object (by incrementing its reference count)
 *
 * @param object        Pointer to the object
 */
#define OBJ_RETAIN(object)                                              \
    do {                                                                \
        assert(NULL != object);                                         \
        assert(NULL != ((ompi_object_t *) (object))->obj_class);        \
        if (NULL != object) {                                           \
            ompi_obj_update((ompi_object_t *) (object), 1);             \
        }                                                               \
        assert(((ompi_object_t *) (object))->obj_reference_count >= 0); \
    } while (0)


/**
 * Release an object (by decrementing its reference count).  If the
 * reference count reaches zero, destruct (finalize) the object and
 * free its storage.
 *
 * Note: If the object is freed, then the value of the pointer is set
 * to NULL.
 *
 * @param object        Pointer to the object
 */
#define OBJ_RELEASE(object)                                             \
    do {                                                                \
        assert(NULL != object);                                         \
        assert(NULL != ((ompi_object_t *) (object))->obj_class);        \
        if (0 == ompi_obj_update((ompi_object_t *) (object), -1)) {     \
            ompi_obj_run_destructors((ompi_object_t *) (object));       \
            free(object);                                               \
            object = NULL;                                              \
        }                                                               \
    } while (0)


/**
 * Construct (initialize) objects that are not dynamically allocated.
 *
 * @param object        Pointer to the object
 * @param type          The object type
 */

#if OMPI_ENABLE_DEBUG
#define OBJ_CONSTRUCT(object, type)                             \
do {                                                            \
    OBJ_CONSTRUCT_INTERNAL((object), OBJ_CLASS(type));          \
    ((ompi_object_t *)(object))->cls_init_file_name = __FILE__; \
    ((ompi_object_t *)(object))->cls_init_lineno = __LINE__;    \
} while (0)
#else
#define OBJ_CONSTRUCT(object, type)                             \
    OBJ_CONSTRUCT_INTERNAL(object, OBJ_CLASS(type))
#endif  /* OMPI_ENABLE_DEBUG */

#define OBJ_CONSTRUCT_INTERNAL(object, type)                            \
    do {                                                                \
        if (0 == (type)->cls_initialized) {                             \
            ompi_class_initialize((type));                              \
        }                                                               \
        if (NULL != object) {                                           \
            ((ompi_object_t *) (object))->obj_class = (type);           \
            ((ompi_object_t *) (object))->obj_reference_count = 1;      \
            ompi_obj_run_constructors((ompi_object_t *) (object));      \
        }                                                               \
    } while (0)


/**
 * Destruct (finalize) an object that is not dynamically allocated.
 *
 * @param object        Pointer to the object
 */
#define OBJ_DESTRUCT(object)                                      \
    do {                                                          \
        if (NULL != object) {                                     \
            ompi_obj_run_destructors((ompi_object_t *) (object)); \
        }                                                         \
    } while (0)

BEGIN_C_DECLS
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_object_t);


/* declarations *******************************************************/

/**
 * Lazy initialization of class descriptor.
 *
 * Specifically cache arrays of function pointers for the constructor
 * and destructor hierarchies for this class.
 *
 * @param class    Pointer to class descriptor
 */
OMPI_DECLSPEC void ompi_class_initialize(ompi_class_t *);

/**
 * Shut down the class system and release all memory
 *
 * This function should be invoked as the ABSOLUTE LAST function to
 * use the class subsystem.  It frees all associated memory with ALL
 * classes, rendering all of them inoperable.  It is here so that
 * tools like valgrind and purify don't report still-reachable memory
 * upon process termination.
 */
int ompi_class_finalize(void);

END_C_DECLS
/**
 * Run the hierarchy of class constructors for this object, in a
 * parent-first order.
 *
 * Do not use this function directly: use OBJ_CONSTRUCT() instead.
 *
 * WARNING: This implementation relies on a hardwired maximum depth of
 * the inheritance tree!!!
 *
 * Hardwired for fairly shallow inheritance trees
 * @param size          Pointer to the object.
 */
static inline void ompi_obj_run_constructors(ompi_object_t * object)
{
    ompi_class_t *cls;
    int i;

    assert(NULL != object);
    assert(NULL != object->obj_class);

    cls = object->obj_class;
    for (i = cls->cls_depth - 1; i >= 0; i--) {
        if (cls->cls_construct_array[i]) {
            (cls->cls_construct_array[i]) (object);
        }
    }
}


/**
 * Run the hierarchy of class destructors for this object, in a
 * parent-last order.
 *
 * Do not use this function directly: use OBJ_DESTRUCT() instead.
 *
 * @param size          Pointer to the object.
 */
static inline void ompi_obj_run_destructors(ompi_object_t * object)
{
    ompi_class_t *cls;
    int i;

    assert(NULL != object);
    assert(NULL != object->obj_class);

    cls = object->obj_class;
    for (i = 0; i < cls->cls_depth; i++) {
        if (cls->cls_destruct_array[i]) {
            (cls->cls_destruct_array[i]) (object);
        }
    }
}


/**
 * Create new object: dynamically allocate storage and run the class
 * constructor.
 *
 * Do not use this function directly: use OBJ_NEW() instead.
 *
 * @param size          Size of the object
 * @param cls           Pointer to the class descriptor of this object
 * @return              Pointer to the object 
 */
static inline ompi_object_t *ompi_obj_new(size_t size, ompi_class_t * cls)
{
    ompi_object_t *object;

    assert(size >= sizeof(ompi_object_t));

    object = (ompi_object_t *) malloc(size);
    if (0 == cls->cls_initialized) {
        ompi_class_initialize(cls);
    }
    if (NULL != object) {
        object->obj_class = cls;
        object->obj_reference_count = 1;
        ompi_obj_run_constructors(object);
    }
    return object;
}


/**
 * Atomically update the object's reference count by some increment.
 *
 * This function should not be used directly: it is called via the
 * macros OBJ_RETAIN and OBJ_RELEASE
 *
 * @param object        Pointer to the object
 * @param inc           Increment by which to update reference count
 * @return              New value of the reference count
 */
static inline int ompi_obj_update(ompi_object_t *object, int inc)
{
#if OMPI_HAVE_THREAD_SUPPORT
    ompi_atomic_add(&(object->obj_reference_count), inc );
#else
    object->obj_reference_count += inc;
#endif
    return object->obj_reference_count;
}


/**********************************************************************/

#endif                          /* OMPI_OBJECT_H */
