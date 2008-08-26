/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2008.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

/** 
 *  @file OTF_Platform_unix.h
 *
 *  @brief Deals with platform dependend issues.
 *
 *  \ingroup internal
 */


#ifndef OTF_PLATFORM_UNIX_H
#define OTF_PLATFORM_UNIX_H

#include "config.h"


#if defined(HAVE_FSEEKO) && HAVE_FSEEKO
#	define OTF_fseek fseeko
#else /* HAVE_FSEEKO */
#	define OTF_fseek fseek
#endif /* HAVE_FSEEKO */

#if defined(HAVE_FTELLO) && HAVE_FTELLO
#	define OTF_ftell ftello
#else /* HAVE_FTELLO */
#	define OTF_ftell ftell
#endif /* HAVE_FTELLO */

#define OTF_snprintf snprintf

#endif /* OTF_PLATFORM_UNIX_H */
