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
 * @file@
 *
 * Hooks for catching release of memory from the current process
 *
 * Hooks for catching the release of memory from the current process.
 * For linking reasons, this is not a component framework (some of
 * these require tight coupling into libopal and the wrapper compilers
 * and that entire stack).
 */

#ifndef OPAL_MEMORY_MEMORY_H
#define OPAl_MEMORY_MEMORY_H

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


int opal_mem_free_init(void);

int opal_mem_free_finalize(void);


/**
 * Callback when memory is about to be released by the process
 *
 * Callback when the process is about to release memory (but before it
 * actually does so).  It is not specified whether this means that
 * free() is being called on that piece of memory and the memory may
 * remain associated with the process or if the memory is being given
 * back to the system through sbrk() or munmap().
 *
 * @param buf     Pointer to the start of the allocation 
 *                to be released
 * @param lentgh  Length of the allocation to be released
 *                (starting at \c buf)
 * @param cbdata  Pointer-length of information passed to 
 *                the handler registration function.
 */
typedef void (opal_mem_free_unpin_fn_t)(void *buf, size_t length, void *cbdata);


/**
 * Query functionality of memory callbacks
 *
 * Query whether the system is capable of providing callbacks when
 * memory is about to be released by a process.
 *
 * @retval true opal_mem_free_register_handler() will not return 
 *              \c OMPI_ERR_NOT_SUPPORTED.
 * @retval false opal_mem_free_register_handler() will always return
 *              \c OMPI_ERR_NOT_SUPPORTED.
 *
 * \note There is no reason you have to call this function before
 * calling opal_mem_free_register_handler().  It exists for component
 * selection logic that may want to see what the status of the memory
 * hook support is without actually registering anything.
 */
bool opal_mem_free_is_supported(void);


/**
 * Register callback for when memory is to be released
 *
 * Register a \c opal_memory_unpin_fn_t function pointer to be called
 * whenever the current process is about to release memory.
 *
 * @param func    Function pointer to call when memory is to be released
 * @param cbdata  A pointer-length field to be passed to func when it is
 *                invoked.
 *
 * @retval OMPI_SUCCESS The registration completed successfully.
 * @retval OMPI_EXISTS  The function is already registered and will not
 *                      be registered again.
 * @retval OMPI_ERR_NOT_SUPPORTED There are no hooks available for 
 *                      receiving callbacks when memory is to be released
 */
int opal_mem_free_register_handler(opal_mem_free_unpin_fn_t *func, void *cbdata);


/**
 * Unregister previously registered callback
 *
 * Unregister previously registered callback.
 *
 * @param func   Function pointer to registered callback to remove
 *
 * @retval OMPI_SUCCESS The function was successfully deregistered
 * @retval OMPI_ERR_NOT_FOUND The function was not previously registered
 */
int opal_mem_free_unregister_handler(opal_mem_free_unpin_fn_t *func);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* OPAL_MEMORY_MEMORY_H */
