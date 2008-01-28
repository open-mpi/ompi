/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2007.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

/** 
 *  @file OTF_Platform.h
 *
 *  @brief Deals with platform dependend issues.
 *
 *  \ingroup internal
 */


/* THIS HEADER SHOULD NEVER BE INCLUDED INTO OTHER HEADERS OF THE OTF LIBRARY
   AND HAS TO BE INCLUDED INTO ALL .C-FILES OF THE OTF LIBARY */


#ifndef OTF_PLATFORM_H
#define OTF_PLATFORM_H

#ifdef __cplusplus
#define EXTERN extern "C"
#else
#define EXTERN extern
#endif /* __cplusplus */

/* if you know (for sure) of more compilers supporting __FUNCTION__,
   then add them here */
#if defined __GNUC__ /* gnu */
#elif defined _MSC_VER /* ms vs */
#elif defined __xlC__ /* ibm xlc */
#else

	/* set __FUNCTION__ to a dummy for compilers not supporting this macro */
	#define __FUNCTION__ "<unknown function>"

#endif


#ifndef __FILE__
	#define __FILE__ "<unknown file>"
#endif

#ifndef __LINE__
	#define __LINE__ 0
#endif



#if defined __linux

	#define OTF_fseek fseeko
	#define OTF_ftell ftello

	#define OTF_snprintf snprintf

#elif defined WIN32 /* windows */


	#if defined _MSC_VER /* vs */

		#define HAVE_IO_H

		#define OTF_ftell (uint64_t) _ftelli64
		#define OTF_fseek(f,off,orig) _fseeki64(f,(__int64)off,orig)

		#define OTF_snprintf _snprintf

		#pragma warning (disable : 4996) /* disable insecurity/deprication warnings */

	#else

		#error "You are using an unsupported compiler on windows."

	#endif

#else /* don't know what to put here */

	#define OTF_fseek fseek
	#define OTF_ftell ftell
	
	#define OTF_snprintf snprintf

#endif

EXTERN char *OTF_strdup( const char*s );

#endif /* OTF_PLATFORM_H */
