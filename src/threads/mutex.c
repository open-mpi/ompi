/*
 * $HEADER$
 */

#include "lam_config.h"

#include "threads/mutex.h"

/*
 * Default to a safe value
 */
bool lam_uses_threads = (bool) LAM_HAVE_THREADS;

