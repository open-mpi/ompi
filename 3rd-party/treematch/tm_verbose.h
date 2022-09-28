#include <stdio.h>
#include "ompi_config.h"

#define NONE     0
/* output in stderr*/
#define CRITICAL 1
#define ERROR    2
/* output in stdout*/
#define WARNING  3
#define TIMING   4
#define INFO     5
#define DEBUG    6


/* return 0 on errror and  1 on success */
OMPI_HIDDEN int          tm_open_verbose_file(char *filename);
OMPI_HIDDEN int          tm_close_verbose_file(void);
OMPI_HIDDEN void         tm_set_verbose_level(unsigned int level);
OMPI_HIDDEN unsigned int tm_get_verbose_level(void);
OMPI_HIDDEN FILE *       tm_get_verbose_output(void);

#define tm_verbose_printf(level, ...) level <= tm_get_verbose_level()?fprintf(tm_get_verbose_output(),__VA_ARGS__):0

