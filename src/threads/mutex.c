/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "threads/mutex.h"

/*
 * Default to a safe value
 */
bool ompi_uses_threads = (bool) OMPI_HAVE_THREADS;

