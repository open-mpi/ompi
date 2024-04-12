/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2021-2023 Triad National Security, LLC. All rights reserved.
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
 *   struct sally_t
 *   {
 *     parent_t parent;
 *     void *first_member;
 *     ...
 *   };
 *   typedef struct sally_t sally_t;
 *
 *   PMIX_CLASS_DECLARATION(sally_t);
 * @endcode
 * All classes must have a parent which is also class.
 *
 * In an implementation (.c) file, instantiate a class descriptor for
 * the class like this:
 * @code
 *   PMIX_CLASS_INSTANCE(sally_t, parent_t, sally_construct, sally_destruct);
 * @endcode
 * This macro actually expands to
 * @code
 *   pmix_class_t sally_t_class = {
 *     "sally_t",
 *     PMIX_CLASS(parent_t),  // pointer to parent_t_class
 *     sally_construct,
 *     sally_destruct,
 *     0, 0, NULL, NULL,
 *     sizeof ("sally_t")
 *   };
 * @endcode
 * This variable should be declared in the interface (.h) file using
 * the PMIX_CLASS_DECLARATION macro as shown above.
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
 * To create a instance of a class (an object) use PMIX_NEW:
 * @code
 *   sally_t *sally = PMIX_NEW(sally_t);
 * @endcode
 * which allocates memory of sizeof(sally_t) and runs the class's
 * constructors.
 *
 * Use PMIX_RETAIN, PMIX_RELEASE to do reference-count-based
 * memory management:
 * @code
 *   PMIX_RETAIN(sally);
 *   PMIX_RELEASE(sally);
 *   PMIX_RELEASE(sally);
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
 * PMIX_CONSTRUCT:
 * @code
 *   sally_t sally;
 *
 *   PMIX_CONSTRUCT(&sally, sally_t);
 * @endcode
 * The retain/release model is not necessary here, but before the
 * object goes out of scope, PMIX_DESTRUCT should be run to release
 * initialized resources:
 * @code
 *   PMIX_DESTRUCT(&sally);
 * @endcode
 */

#ifndef PMIX_OBJECT_H
#define PMIX_OBJECT_H

#include "src/include/pmix_config.h"
#include "pmix_common.h"

#include <assert.h>
#ifdef HAVE_STDLIB_H
#    include <stdlib.h>
#endif /* HAVE_STDLIB_H */
#include <pthread.h>
#include <stdio.h>
#include <errno.h>

BEGIN_C_DECLS

#if PMIX_ENABLE_DEBUG
/* Any kind of unique ID should do the job */
#    define PMIX_OBJ_MAGIC_ID ((0xdeafbeedULL << 32) + 0xdeafbeedULL)
#endif

/*
 * Macros for variadic object instantiation: w/wout custom memory allocator.
 *
 * NOTE(skg) There is probably a nicer way to implement this functionality. In
 * particular, with further macro magic we can probably unify a lot of the
 * common code here, but that is for another day.
 */

/**
 * Takes in at least three arguments, eats all of them except the third.
 * This allows us to do things like:
 * PMIX_NEW_HAS_ARITY_HELPER(sally_t, TMA, NO_TMA, ERROR) --> NO_TMA
 * PMIX_NEW_HAS_ARITY_HELPER(sally_t, a_tma, TMA, NO_TMA, ERROR) --> TMA
 */
#define PMIX_NEW_HAS_ARITY_HELPER(_1, _2, N, ...) N

#define PMIX_NEW_HAS_ARITY_IMPL(...) \
    PMIX_NEW_HAS_ARITY_HELPER(__VA_ARGS__)
/*
 * PMIX_CONSTRUCT() takes a maximum of three arguments:
 * Two arguments means caller does not want a custom memory allocator.
 * Three arguments means caller wants a custom (TMA) memory allocator.
 *
 * Please see PMIX_NEW_HAS_ARITY_HELPER's description above for more details
 * about how this macro functions.
 */
#define PMIX_CONSTRUCT_HAS_ARITY_HELPER(_1, _2, _3, N, ...) N

#define PMIX_CONSTRUCT_HAS_ARITY_IMPL(...) \
    PMIX_CONSTRUCT_HAS_ARITY_HELPER(__VA_ARGS__)

/*
 * Macro suffix naming convention must be _TMA or _NO_TMA.
 *
 * These are the displacement mode lists used in the PMIX_NEW_HAS_ARITY_HELPER
 * example above.
 */
#define PMIX_NEW_HAS_ARGS_SOURCE() TMA, NO_TMA, ERROR

#define PMIX_CONSTRUCT_HAS_ARGS_SOURCE() TMA, NO_TMA, ERROR, ERROR

#define PMIX_OBJ_HAS_ARGS(...) \
    PMIX_NEW_HAS_ARITY_IMPL(__VA_ARGS__, PMIX_NEW_HAS_ARGS_SOURCE())

#define PMIX_CONSTRUCT_HAS_ARGS(...) \
    PMIX_CONSTRUCT_HAS_ARITY_IMPL(__VA_ARGS__, PMIX_CONSTRUCT_HAS_ARGS_SOURCE())
/*
 * These are used to generate the proper macro name and forward the relevant
 * arguments depending on the number of arguments used.
 */
#define PMIX_NEW_DISAMBIGUATE2(has_args, ...) \
    PMIX_NEW_ ## has_args (__VA_ARGS__)

#define PMIX_NEW_DISAMBIGUATE(has_args, ...) \
    PMIX_NEW_DISAMBIGUATE2(has_args, __VA_ARGS__)

#define PMIX_CONSTRUCT_DISAMBIGUATE2(has_args, ...) \
    PMIX_CONSTRUCT_ ## has_args (__VA_ARGS__)

#define PMIX_CONSTRUCT_DISAMBIGUATE(has_args, ...) \
    PMIX_CONSTRUCT_DISAMBIGUATE2(has_args, __VA_ARGS__)

/* typedefs ***********************************************************/

typedef struct pmix_object_t pmix_object_t;
typedef struct pmix_class_t pmix_class_t;
typedef void (*pmix_construct_t)(pmix_object_t *);
typedef void (*pmix_destruct_t)(pmix_object_t *);

/* types **************************************************************/

/** Memory allocator for objects */
typedef struct pmix_tma {
    /** Pointer to the TMA's malloc() function. */
    void *(*tma_malloc)(struct pmix_tma *, size_t);
    /** Pointer to the TMA's calloc() function. */
    void *(*tma_calloc)(struct pmix_tma *, size_t, size_t);
    /** Pointer to the TMA's realloc() function. */
    void *(*tma_realloc)(struct pmix_tma *, void *, size_t);
    /*
     * NOTE: The seemingly unnecessary name mangling here is in response to
     * certain compilers not liking the use of a function pointer named strdup.
     */
    /** Pointer to the TMA's strdup() function. */
    char *(*tma_strdup)(struct pmix_tma *, const char *s);
    /**
     * A memmove()-like function that copies the provided contents to an
     * appropriate location in the memory area maintained by the allocator.
     * Like memmove(), it returns a pointer to the content's destination.
     */
    void *(*tma_memmove)(struct pmix_tma *tma, const void *src, size_t n);
    /** Pointer to the TMA's free() function. */
    void (*tma_free)(struct pmix_tma *, void *);
    /** Points to a user-defined TMA context. */
    void *data_context;
    /**
     * Points to generic data used by a TMA. An example includes a pointer to a
     * value that maintains the next available address.
     */
    void **data_ptr;
} pmix_tma_t;

static inline void *pmix_tma_malloc(pmix_tma_t *tma, size_t size)
{
    if (NULL != tma) {
        return tma->tma_malloc(tma, size);
    } else {
        return malloc(size);
    }
}

static inline void *pmix_tma_calloc(pmix_tma_t *tma, size_t nmemb, size_t size)
{
    if (NULL != tma) {
        return tma->tma_calloc(tma, nmemb, size);
    } else {
        return calloc(nmemb, size);
    }
}

static inline void *pmix_tma_realloc(pmix_tma_t *tma, void *ptr, size_t size)
{
    if (NULL != tma) {
        return tma->tma_realloc(tma, ptr, size);
    } else {
        return realloc(ptr, size);
    }
}

static inline char *pmix_tma_strdup(pmix_tma_t *tma, const char *src)
{
    if (NULL != tma) {
        return tma->tma_strdup(tma, src);
    } else {
        return strdup(src);
    }
}

static inline void pmix_tma_free(pmix_tma_t *tma, void *ptr)
{
    if (NULL != tma) {
        tma->tma_free(tma, ptr);
    } else {
        free(ptr);
    }
}

/**
 * Class descriptor.
 *
 * There should be a single instance of this descriptor for each class
 * definition.
 */
struct pmix_class_t {
    const char *cls_name;           /**< symbolic name for class */
    pmix_class_t *cls_parent;       /**< parent class descriptor */
    pmix_construct_t cls_construct; /**< class constructor */
    pmix_destruct_t cls_destruct;   /**< class destructor */
    int cls_initialized;            /**< is class initialized */
    int cls_depth;                  /**< depth of class hierarchy tree */
    pmix_construct_t *cls_construct_array;
    /**< array of parent class constructors */
    pmix_destruct_t *cls_destruct_array;
    /**< array of parent class destructors */
    size_t cls_sizeof; /**< size of an object instance */
};

PMIX_EXPORT extern int pmix_class_init_epoch;

/**
 * For static initializations of OBJects.
 *
 * @param NAME   Name of the class to initialize
 */
#if PMIX_ENABLE_DEBUG
#    define PMIX_OBJ_STATIC_INIT(BASE_CLASS)        \
        {                                           \
            .obj_magic_id = PMIX_OBJ_MAGIC_ID,      \
            .obj_class = PMIX_CLASS(BASE_CLASS),    \
            .obj_lock = PTHREAD_MUTEX_INITIALIZER,  \
            .obj_reference_count = 1,               \
            .obj_tma = {                            \
                .tma_malloc = NULL,                 \
                .tma_calloc = NULL,                 \
                .tma_realloc = NULL,                \
                .tma_strdup = NULL,                 \
                .tma_memmove = NULL,                \
                .tma_free = NULL,                   \
                .data_context = NULL,               \
                .data_ptr = NULL                    \
            },                                      \
            .cls_init_file_name = __FILE__,         \
            .cls_init_lineno = __LINE__             \
        }
#else
#    define PMIX_OBJ_STATIC_INIT(BASE_CLASS)        \
        {                                           \
            .obj_class = PMIX_CLASS(BASE_CLASS),    \
            .obj_lock = PTHREAD_MUTEX_INITIALIZER,  \
            .obj_reference_count = 1,               \
            .obj_tma = {                            \
                .tma_malloc = NULL,                 \
                .tma_calloc = NULL,                 \
                .tma_realloc = NULL,                \
                .tma_strdup = NULL,                 \
                .tma_memmove = NULL,                \
                .tma_free = NULL,                   \
                .data_context = NULL,               \
                .data_ptr = NULL                    \
            }                                       \
        }
#endif

/**
 * Base object.
 *
 * This is special and does not follow the pattern for other classes.
 */
struct pmix_object_t {
#if PMIX_ENABLE_DEBUG
    /** Magic ID -- want this to be the very first item in the
        struct's memory */
    uint64_t obj_magic_id;
#endif
    pthread_mutex_t obj_lock;
    pmix_class_t *obj_class;                 /**< class descriptor */
    int32_t obj_reference_count;             /**< reference count */
    pmix_tma_t obj_tma;                      /**< allocator for this object */
#if PMIX_ENABLE_DEBUG
    const char *cls_init_file_name; /**< In debug mode store the file where the object get constructed */
    int cls_init_lineno; /**< In debug mode store the line number where the object get constructed */
#endif                   /* PMIX_ENABLE_DEBUG */
};

/* macros ************************************************************/

/**
 * Return a pointer to the class descriptor associated with a
 * class type.
 *
 * @param NAME          Name of class
 * @return              Pointer to class descriptor
 */
#define PMIX_CLASS(NAME) (&(NAME##_class))

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
#define PMIX_CLASS_INSTANCE(NAME, PARENT, CONSTRUCTOR, DESTRUCTOR) \
    pmix_class_t NAME##_class = {#NAME,                            \
                                 PMIX_CLASS(PARENT),               \
                                 (pmix_construct_t) CONSTRUCTOR,   \
                                 (pmix_destruct_t) DESTRUCTOR,     \
                                 0,                                \
                                 0,                                \
                                 NULL,                             \
                                 NULL,                             \
                                 sizeof(NAME)}

/**
 * Declaration for class descriptor
 *
 * @param NAME          Name of class
 *
 * Put this in NAME.h
 */
#define PMIX_CLASS_DECLARATION(NAME) extern pmix_class_t NAME##_class

/**
 * Create an object: dynamically allocate storage and run the class
 * constructor.
 *
 * @param type          Type (class) of the object
 * @return              Pointer to the object
 */
static inline pmix_object_t *pmix_obj_new_tma(pmix_class_t *cls, pmix_tma_t *tma);
#if PMIX_ENABLE_DEBUG
static inline pmix_object_t *pmix_obj_new_debug_tma(pmix_class_t *type, pmix_tma_t *tma,
                                                    const char *file, int line)
{
    pmix_object_t *object = pmix_obj_new_tma(type, tma);
    if (NULL != object) {
        object->obj_magic_id = PMIX_OBJ_MAGIC_ID;
        object->cls_init_file_name = file;
        object->cls_init_lineno = line;
    }
    return object;
}

#define PMIX_NEW_NO_TMA(type) \
    ((type *)pmix_obj_new_debug_tma(PMIX_CLASS(type), NULL, __FILE__, __LINE__))

#define PMIX_NEW_TMA(type, tma) \
    ((type *)pmix_obj_new_debug_tma(PMIX_CLASS(type), (tma), __FILE__, __LINE__))

#else
#define PMIX_NEW_NO_TMA(type)   ((type *)pmix_obj_new_tma(PMIX_CLASS(type), NULL))

#define PMIX_NEW_TMA(type, tma) ((type *)pmix_obj_new_tma(PMIX_CLASS(type), (tma)))

#endif /* PMIX_ENABLE_DEBUG */

/**
 * PMIX_NEW() takes a maximum of two arguments:
 * One argument means caller does not want a custom memory allocator, namely the
 * common case.
 * Two arguments means caller wants a custom (TMA) memory allocator.
 */
#define PMIX_NEW(...) \
    PMIX_NEW_DISAMBIGUATE(PMIX_OBJ_HAS_ARGS(__VA_ARGS__), __VA_ARGS__)

/**
 * Retain an object (by incrementing its reference count)
 *
 * @param object        Pointer to the object
 */
#if PMIX_ENABLE_DEBUG
#    define PMIX_RETAIN(object)                                                      \
        do {                                                                         \
            assert(NULL != ((pmix_object_t *) (object))->obj_class);                 \
            assert(PMIX_OBJ_MAGIC_ID == ((pmix_object_t *) (object))->obj_magic_id); \
            pmix_obj_update((pmix_object_t *) (object), 1);                          \
            assert(((pmix_object_t *) (object))->obj_reference_count >= 0);          \
        } while (0)
#else
#    define PMIX_RETAIN(object) pmix_obj_update((pmix_object_t *) (object), 1)
#endif

/**
 * Helper macro for the debug mode to store the locations where the status of
 * an object change.
 */
#if PMIX_ENABLE_DEBUG
#    define PMIX_REMEMBER_FILE_AND_LINENO(OBJECT, FILE, LINENO)      \
        do {                                                         \
            ((pmix_object_t *) (OBJECT))->cls_init_file_name = FILE; \
            ((pmix_object_t *) (OBJECT))->cls_init_lineno = LINENO;  \
        } while (0)
#    define PMIX_SET_MAGIC_ID(OBJECT, VALUE)                      \
        do {                                                      \
            ((pmix_object_t *) (OBJECT))->obj_magic_id = (VALUE); \
        } while (0)
#else
#    define PMIX_REMEMBER_FILE_AND_LINENO(OBJECT, FILE, LINENO)
#    define PMIX_SET_MAGIC_ID(OBJECT, VALUE)
#endif /* PMIX_ENABLE_DEBUG */

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
#if PMIX_ENABLE_DEBUG
#    define PMIX_RELEASE(object)                                           \
        do {                                                               \
            pmix_object_t *_obj = (pmix_object_t *) object;                \
            assert(NULL != _obj->obj_class);                               \
            assert(PMIX_OBJ_MAGIC_ID == _obj->obj_magic_id);               \
            if (0 == pmix_obj_update(_obj, -1)) {                          \
                PMIX_SET_MAGIC_ID((object), 0);                            \
                pmix_obj_run_destructors(_obj);                            \
                PMIX_REMEMBER_FILE_AND_LINENO(object, __FILE__, __LINE__); \
                if (NULL != _obj->obj_tma.tma_free) {                      \
                    pmix_tma_free(&_obj->obj_tma, object);                 \
                }                                                          \
                else {                                                     \
                    free(object);                                          \
                }                                                          \
                object = NULL;                                             \
            }                                                              \
        } while (0)
#else
#    define PMIX_RELEASE(object)                            \
        do {                                                \
            pmix_object_t *_obj = (pmix_object_t *) object; \
            if (0 == pmix_obj_update(_obj, -1)) {           \
                pmix_obj_run_destructors(_obj);             \
                if (NULL != _obj->obj_tma.tma_free) {       \
                    pmix_tma_free(&_obj->obj_tma, object);  \
                }                                           \
                else {                                      \
                    free(object);                           \
                }                                           \
                object = NULL;                              \
            }                                               \
        } while (0)
#endif

/**
 * Construct (initialize) objects that are not dynamically allocated.
 *
 * @param object        Pointer to the object
 * @param type          The object type
 */
static inline void pmix_obj_construct_tma(pmix_object_t *obj, pmix_tma_t *tma)
{
    if (NULL == tma) {
        obj->obj_tma.tma_malloc = NULL;
        obj->obj_tma.tma_calloc = NULL;
        obj->obj_tma.tma_realloc = NULL;
        obj->obj_tma.tma_strdup = NULL;
        obj->obj_tma.tma_memmove = NULL;
        obj->obj_tma.tma_free = NULL;
        obj->obj_tma.data_context = NULL;
        obj->obj_tma.data_ptr = NULL;
    } else {
        obj->obj_tma = *tma;
    }
}

#define PMIX_CONSTRUCT_INTERNAL_TMA(object, type, t)               \
    do {                                                           \
        PMIX_SET_MAGIC_ID((object), PMIX_OBJ_MAGIC_ID);            \
        if (pmix_class_init_epoch != (type)->cls_initialized) {    \
            pmix_class_initialize((type));                         \
        }                                                          \
        ((pmix_object_t *) (object))->obj_class = (type);          \
        ((pmix_object_t *) (object))->obj_reference_count = 1;     \
        pmix_obj_construct_tma(((pmix_object_t *) (object)), (t)); \
        pmix_obj_run_constructors((pmix_object_t *) (object));     \
        PMIX_REMEMBER_FILE_AND_LINENO(object, __FILE__, __LINE__); \
    } while (0)


#define PMIX_CONSTRUCT_TMA(object, type, t)                           \
    do {                                                              \
        PMIX_CONSTRUCT_INTERNAL_TMA((object), PMIX_CLASS(type), (t)); \
    } while (0)


#define PMIX_CONSTRUCT_NO_TMA(object, type)     \
    do {                                        \
        PMIX_CONSTRUCT_TMA(object, type, NULL); \
    } while (0)

#define PMIX_CONSTRUCT(...) \
    PMIX_CONSTRUCT_DISAMBIGUATE(PMIX_CONSTRUCT_HAS_ARGS(__VA_ARGS__), __VA_ARGS__)

/**
 * Destruct (finalize) an object that is not dynamically allocated.
 *
 * @param object        Pointer to the object
 */
#if PMIX_ENABLE_DEBUG
#    define PMIX_DESTRUCT(object)                                                    \
        do {                                                                         \
            assert(PMIX_OBJ_MAGIC_ID == ((pmix_object_t *) (object))->obj_magic_id); \
            PMIX_SET_MAGIC_ID((object), 0);                                          \
            pmix_obj_run_destructors((pmix_object_t *) (object));                    \
            PMIX_REMEMBER_FILE_AND_LINENO(object, __FILE__, __LINE__);               \
        } while (0)
#else
#    define PMIX_DESTRUCT(object)                                      \
        do {                                                           \
            pmix_obj_run_destructors((pmix_object_t *) (object));      \
            PMIX_REMEMBER_FILE_AND_LINENO(object, __FILE__, __LINE__); \
        } while (0)
#endif

PMIX_CLASS_DECLARATION(pmix_object_t);

/* declarations *******************************************************/

/**
 * Lazy initialization of class descriptor.
 *
 * Specifically cache arrays of function pointers for the constructor
 * and destructor hierarchies for this class.
 *
 * @param class    Pointer to class descriptor
 */
PMIX_EXPORT void pmix_class_initialize(pmix_class_t *cls);

/**
 * Shut down the class system and release all memory
 *
 * This function should be invoked as the ABSOLUTE LAST function to
 * use the class subsystem.  It frees all associated memory with ALL
 * classes, rendering all of them inoperable.  It is here so that
 * tools like valgrind and purify don't report still-reachable memory
 * upon process termination.
 */
PMIX_EXPORT int pmix_class_finalize(void);

/**
 * Run the hierarchy of class constructors for this object, in a
 * parent-first order.
 *
 * Do not use this function directly: use PMIX_CONSTRUCT() instead.
 *
 * WARNING: This implementation relies on a hardwired maximum depth of
 * the inheritance tree!!!
 *
 * Hardwired for fairly shallow inheritance trees
 * @param size          Pointer to the object.
 */
static inline void pmix_obj_run_constructors(pmix_object_t *object)
{
    pmix_construct_t *cls_construct;

    assert(NULL != object->obj_class);

    cls_construct = object->obj_class->cls_construct_array;
    while (NULL != *cls_construct) {
        (*cls_construct)(object);
        cls_construct++;
    }
}

/**
 * Run the hierarchy of class destructors for this object, in a
 * parent-last order.
 *
 * Do not use this function directly: use PMIX_DESTRUCT() instead.
 *
 * @param size          Pointer to the object.
 */
static inline void pmix_obj_run_destructors(pmix_object_t *object)
{
    pmix_destruct_t *cls_destruct;

    assert(NULL != object->obj_class);

    cls_destruct = object->obj_class->cls_destruct_array;
    while (NULL != *cls_destruct) {
        (*cls_destruct)(object);
        cls_destruct++;
    }
}

/**
 * Create new object: dynamically allocate storage and run the class
 * constructor.
 *
 * Do not use this function directly: use PMIX_NEW() instead.
 *
 * @param size          Size of the object
 * @param cls           Pointer to the class descriptor of this object
 * @return              Pointer to the object
 */
static inline pmix_object_t *pmix_obj_new_tma(pmix_class_t *cls, pmix_tma_t *tma)
{
    pmix_object_t *object;
    assert(cls->cls_sizeof >= sizeof(pmix_object_t));

    object = (pmix_object_t *) pmix_tma_malloc(tma, cls->cls_sizeof);

    if (pmix_class_init_epoch != cls->cls_initialized) {
        pmix_class_initialize(cls);
    }
    if (NULL != object) {
#if PMIX_ENABLE_DEBUG
        pthread_mutexattr_t attr;
        pthread_mutexattr_init(&attr);

        /* set type to ERRORCHECK so that we catch recursive locks */
#    if PMIX_HAVE_PTHREAD_MUTEX_ERRORCHECK_NP
        pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_ERRORCHECK_NP);
#    elif PMIX_HAVE_PTHREAD_MUTEX_ERRORCHECK
        pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_ERRORCHECK);
#    endif /* PMIX_HAVE_PTHREAD_MUTEX_ERRORCHECK_NP */

        pthread_mutex_init(&object->obj_lock, &attr);
        pthread_mutexattr_destroy(&attr);

#else

        /* Without debugging, choose the fastest available mutexes */
        pthread_mutex_init(&object->obj_lock, NULL);

#endif /* PMIX_ENABLE_DEBUG */
        object->obj_class = cls;
        object->obj_reference_count = 1;
        if (NULL == tma) {
            object->obj_tma.tma_malloc = NULL;
            object->obj_tma.tma_calloc = NULL;
            object->obj_tma.tma_realloc = NULL;
            object->obj_tma.tma_strdup = NULL;
            object->obj_tma.tma_free = NULL;
            object->obj_tma.data_context = NULL;
            object->obj_tma.data_ptr = NULL;
        } else {
            object->obj_tma = *tma;
        }
        pmix_obj_run_constructors(object);
    }
    return object;
}

/**
 * Atomically update the object's reference count by some increment.
 *
 * This function should not be used directly: it is called via the
 * macros PMIX_RETAIN and PMIX_RELEASE
 *
 * @param object        Pointer to the object
 * @param inc           Increment by which to update reference count
 * @return              New value of the reference count
 */
static inline int pmix_obj_update(pmix_object_t *object, int inc) __pmix_attribute_always_inline__;
static inline int pmix_obj_update(pmix_object_t *object, int inc)
{
    int ret = pthread_mutex_lock(&object->obj_lock);
    if (ret == EDEADLK) {
        errno = ret;
        perror("pthread_mutex_lock()");
        abort();
    }
    ret = (object->obj_reference_count += inc);
    pthread_mutex_unlock(&object->obj_lock);
    return ret;
}

/**
 * Get a pointer to a given object's memory allocator. Returns a pointer to the
 * TMA, if available. Returns NULL Otherwise.
 */
static inline pmix_tma_t *
pmix_obj_get_tma(
    pmix_object_t *obj
) {
    // Look for a given function pointer. If it isn't NULL, then assume that
    // this object has a custom memory allocator.
    if (obj->obj_tma.tma_malloc) {
        return &obj->obj_tma;
    }
    return NULL;
}

END_C_DECLS

#endif
