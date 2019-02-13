
#ifndef  OPAL_MCA_THREADS_ARGOBOTS_THREADS_ARGOBOTS_H
#define  OPAL_MCA_THREADS_ARGOBOTS_THREADS_ARGOBOTS_H 1

#include <abt.h>

static inline void ensure_init_argobots(void) {
	if (ABT_initialized() != 0)
		ABT_init(0, 0);
}

#endif /* OPAL_MCA_THREADS_ARGOBOTS_THREADS_ARGOBOTS_H */
