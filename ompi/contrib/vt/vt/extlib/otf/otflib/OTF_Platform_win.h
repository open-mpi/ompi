/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2008.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

/** 
 *  @file OTF_Platform_win.h
 *
 *  @brief Deals with platform dependend issues.
 *
 *  \ingroup internal
 */


#ifndef OTF_PLATFORM_WIN_H
#define OTF_PLATFORM_WIN_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#if defined(_MSC_VER) /* visual studio */

#	define HAVE_IO_H

#	undef ftello
#	define ftello (uint64_t) _ftelli64
#	undef fseeko
#	define fseeko(f,off,orig) _fseeki64(f,(__int64)off,orig)

#	define snprintf
#	define snprintf _snprintf

#	pragma warning (disable : 4996) /* disable insecurity/deprication warnings */

	int gettimeofday(struct timeval* tv, void* dummytimezone);

#else

#	error "You are using an unsupported compiler on windows."

#endif /* _MSC_VER */

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* OTF_PLATFORM_WIN_H */
