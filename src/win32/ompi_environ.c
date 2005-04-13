/*
 Copyright (c) 2004-2005 The Trustees of Indiana University.
                         All rights reserved.
 Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
                         All rights reserved.
 Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
                         University of Stuttgart.  All rights reserved.
 Copyright (c) 2004-2005 The Regents of the University of California.
                         All rights reserved.
 $COPYRIGHT$
 
 Additional copyrights may follow
 
 $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "include/constants.h"
#include "util/printf.h"
#include "util/argv.h"

#include "win32/ompi_environ.h"

int ompi_setenv(const char *name, const char *value, bool overwrite,
                char ***env)
{
   int i;
   char *newvalue, *compare;
   size_t len;

   /* Make the new value */

   if (NULL == value) {
      asprintf(&newvalue, "%s=", name);
   } else {
      asprintf(&newvalue, "%s=%s", name, value);
   }
   if (NULL == newvalue) {
      return OMPI_ERR_OUT_OF_RESOURCE;
   }

   /* Check the bozo case */

   if (NULL == env) {
      return OMPI_ERR_BAD_PARAM;
   } else if (NULL == *env) {
      i = 0;
      ompi_argv_append(&i, env, newvalue);
      return OMPI_SUCCESS;
   }

   /* Make something easy to compare to */

   asprintf(&compare, "%s=", name);
   if (NULL == compare) {
      free(newvalue);
      return OMPI_ERR_OUT_OF_RESOURCE;
   }
   len = strlen(compare);

   /* Look for a duplicate that's already set in the env */

   for (i = 0; (*env)[i] != NULL; ++i) {
      if (0 == strncmp((*env)[i], compare, len)) {
         if (overwrite) {
            free((*env)[i]);
            (*env)[i] = newvalue;
            free(compare);
            return OMPI_SUCCESS;
         } else {
            free(compare);
            free(newvalue);
            return OMPI_EXISTS;
         }
      }
   }

   /* If we found no match, append this value */

   i = ompi_argv_count(*env);
   ompi_argv_append(&i, env, newvalue);

   /* All done */

   free(compare);
   free(newvalue);
   return OMPI_SUCCESS;
}

int ompi_unsetenv(const char *name, char ***env)
{
   int i;
   char *compare;
   size_t len;
   bool found;

   /* Check for bozo case */

   if (NULL == *env) {
      return OMPI_SUCCESS;
   }

   /* Make something easy to compare to */

   asprintf(&compare, "%s=", name);
   if (NULL == compare) {
      return OMPI_ERR_OUT_OF_RESOURCE;
   }
   len = strlen(compare);

   /* Look for a duplicate that's already set in the env.  If we find
       it, free it, and then start shifting all elements down one in
       the array. */
   found = false;
   for (i = 0; (*env)[i] != NULL; ++i) {
      if (found) {
         (*env)[i] = (*env)[i + 1];
      } else if (0 == strncmp((*env)[i], compare, len)) {
         free((*env)[i]);
         (*env)[i] = (*env)[i + 1];
         found = true;
      }
   }

   /* All done */

   return (found) ? OMPI_SUCCESS : OMPI_ERR_NOT_FOUND;
}
