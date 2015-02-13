/*
 * Copyright (c) 2013-2014 Intel Corporation. All rights reserved.
 * Copyright (c) 2015 Cisco Systems, Inc. All rights reserved.
 *
 * This software is available to you under a choice of one of two
 * licenses.  You may choose to be licensed under the terms of the GNU
 * General Public License (GPL) Version 2, available from the file
 * COPYING in the main directory of this source tree, or the
 * BSD license below:
 *
 *     Redistribution and use in source and binary forms, with or
 *     without modification, are permitted provided that the following
 *     conditions are met:
 *
 *      - Redistributions of source code must retain the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer.
 *
 *      - Redistributions in binary form must reproduce the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer in the documentation and/or other materials
 *        provided with the distribution.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#ifndef _FI_ERRNO_H_
#define _FI_ERRNO_H_

#include <errno.h>

#ifdef __cplusplus
extern "C" {
#endif

/* FI directly mapped errno values */

#define	FI_SUCCESS		0

//#define	FI_EPERM		EPERM		/* Operation not permitted */
#define	FI_ENOENT		ENOENT		/* No such file or directory */
//#define	FI_ESRCH		ESRCH		/* No such process */
//#define	FI_EINTR		EINTR		/* Interrupted system call */
#define	FI_EIO		 	EIO		/* I/O error */
//#define	FI_ENXIO		ENXIO		/* No such device or address */
#define	FI_E2BIG		E2BIG		/* Argument list too long */
//#define	FI_ENOEXEC		ENOEXEC		/* Exec format error */
#define	FI_EBADF		EBADF		/* Bad file number */
//#define	FI_ECHILD		ECHILD		/* No child processes */
#define	FI_EAGAIN		EAGAIN		/* Try again */
#define	FI_ENOMEM		ENOMEM		/* Out of memory */
#define	FI_EACCES		EACCES		/* Permission denied */
//#define	FI_EFAULT		EFAULT		/* Bad address */
//#define	FI_ENOTBLK		ENOTBLK		/* Block device required */
#define	FI_EBUSY		EBUSY		/* Device or resource busy */
//#define	FI_EEXIST		EEXIST		/* File exists */
//#define	FI_EXDEV		EXDEV		/* Cross-device link */
#define	FI_ENODEV		ENODEV		/* No such device */
//#define	FI_ENOTDIR		ENOTDIR		/* Not a directory */
//#define	FI_EISDIR		EISDIR		/* Is a directory */
#define	FI_EINVAL		EINVAL		/* Invalid argument */
//#define	FI_ENFILE		ENFILE		/* File table overflow */
#define	FI_EMFILE		EMFILE		/* Too many open files */
//#define	FI_ENOTTY		ENOTTY		/* Not a typewriter */
//#define	FI_ETXTBSY		ETXTBSY		/* Text file busy */
//#define	FI_EFBIG		EFBIG		/* File too large */
#define	FI_ENOSPC		ENOSPC		/* No space left on device */
//#define	FI_ESPIPE		ESPIPE		/* Illegal seek */
//#define	FI_EROFS		EROFS		/* Read-only file system */
//#define	FI_EMLINK		EMLINK		/* Too many links */
//#define	FI_EPIPE		EPIPE		/* Broken pipe */
//#define	FI_EDOM			EDOM		/* Math argument out of domain of func */
//#define	FI_ERANGE		ERANGE		/* Math result not representable */
//#define	FI_EDEADLK		EDEADLK		/* Resource deadlock would occur */
//#define	FI_ENAMETOOLONG		ENAMETOLONG	/* File name too long */
//#define	FI_ENOLCK		ENOLCK		/* No record locks available */
#define	FI_ENOSYS		ENOSYS		/* Function not implemented */
//#define	FI_ENOTEMPTY		ENOTEMPTY	/* Directory not empty */
//#define	FI_ELOOP		ELOOP		/* Too many symbolic links encountered */
//#define	FI_EWOULDBLOCK		EWOULDBLOCK	/* Operation would block */
#define	FI_ENOMSG		ENOMSG		/* No message of desired type */
//#define	FI_EIDRM		EIDRM		/* Identifier removed */
//#define	FI_ECHRNG		ECHRNG		/* Channel number out of range */
//#define	FI_EL2NSYNC		EL2NSYCN	/* Level 2 not synchronized */
//#define	FI_EL3HLT		EL3HLT		/* Level 3 halted */
//#define	FI_EL3RST		EL3RST		/* Level 3 reset */
//#define	FI_ELNRNG		ELNRNG		/* Link number out of range */
//#define	FI_EUNATCH		EUNATCH		/* Protocol driver not attached */
//#define	FI_ENOCSI		ENOCSI		/* No CSI structure available */
//#define	FI_EL2HLT		EL2HLT		/* Level 2 halted */
//#define	FI_EBADE		EBADE		/* Invalid exchange */
//#define	FI_EBADR		EBADDR		/* Invalid request descriptor */
//#define	FI_EXFULL		EXFULL		/* Exchange full */
//#define	FI_ENOANO		ENOANO		/* No anode */
//#define	FI_EBADRQC		EBADRQC		/* Invalid request code */
//#define	FI_EBADSLT		EBADSLT		/* Invalid slot */
//#define	FI_EDEADLOCK		EDEADLOCK	/* Resource deadlock would occur */
//#define	FI_EBFONT		EBFONT		/* Bad font file format */
//#define	FI_ENOSTR		ENOSTR		/* Device not a stream */
#define	FI_ENODATA		ENODATA		/* No data available */
//#define	FI_ETIME		ETIME		/* Timer expired */
//#define	FI_ENOSR		ENOSR		/* Out of streams resources */
//#define	FI_ENONET		ENONET		/* Machine is not on the network */
//#define	FI_ENOPKG		ENOPKG		/* Package not installed */
//#define	FI_EREMOTE		EREMOTE		/* Object is remote */
//#define	FI_ENOLINK		ENOLINK		/* Link has been severed */
//#define	FI_EADV			EADV		/* Advertise error */
//#define	FI_ESRMNT		ESRMNT		/* Srmount error */
//#define	FI_ECOMM		ECOMM		/* Communication error on send */
//#define	FI_EPROTO		EPROTO		/* Protocol error */
//#define	FI_EMULTIHOP		EMULTIHOP	/* Multihop attempted */
//#define	FI_EDOTDOT		EDOTDOT		/* RFS specific error */
//#define	FI_EBADMSG		EBADMSG		/* Not a data message */
//#define	FI_EOVERFLOW		EOVERFLOW	/* Value too large for defined data type */
//#define	FI_ENOTUNIQ		ENOTUNIQ	/* Name not unique on network */
//#define	FI_EBADFD		EBADFD		/* File descriptor in bad state */
//#define	FI_EREMCHG		EREMCHG		/* Remote address changed */
//#define	FI_ELIBACC		ELIBACC		/* Can not access a needed shared library */
//#define	FI_ELIBBAD		ELIBBAD		/* Accessing a corrupted shared library */
//#define	FI_ELIBSCN		ELIBSCN		/* .lib section in a.out corrupted */
//#define	FI_ELIBMAX		ELIBMAX		/* Attempting to link in too many shared libraries */
//#define	FI_ELIBEXEC		ELIBEXEC	/* Cannot exec a shared library directly */
//#define	FI_EILSEQ		EILSEQ		/* Illegal byte sequence */
//#define	FI_ERESTART		ERESTART	/* Interrupted system call should be restarted */
//#define	FI_ESTRPIPE		ESTRPIPE	/* Streams pipe error */
//#define	FI_EUSERS		EUSERS		/* Too many users */
//#define	FI_ENOTSOCK		ENOTSOCK	/* Socket operation on non-socket */
//#define	FI_EDESTADDRREQ		EDESTADDRREQ	/* Destination address required */
#define	FI_EMSGSIZE		EMSGSIZE	/* Message too long */
//#define	FI_EPROTOTYPE		EPROTOTYPE	/* Protocol wrong type for endpoint */
#define	FI_ENOPROTOOPT		ENOPROTOOPT	/* Protocol not available */
//#define	FI_EPROTONOSUPPORT	EPROTONOSUPPORT	/* Protocol not supported */
//#define	FI_ESOCKTNOSUPPORT	ESOCKTNOSUPPORT	/* Socket type not supported */
#define	FI_EOPNOTSUPP		EOPNOTSUPP	/* Operation not supported on transport endpoint */
//#define	FI_EPFNOSUPPORT		EPFNOSUPPORT	/* Protocol family not supported */
//#define	FI_EAFNOSUPPORT		EAFNOSUPPORT	/* Address family not supported by protocol */
#define	FI_EADDRINUSE		EADDRINUSE	/* Address already in use */
#define	FI_EADDRNOTAVAIL	EADDRNOTAVAIL	/* Cannot assign requested address */
#define	FI_ENETDOWN		ENETDOWN	/* Network is down */
#define	FI_ENETUNREACH		ENETUNREACH	/* Network is unreachable */
//#define	FI_ENETRESET		ENETRESET	/* Network dropped connection because of reset */
#define	FI_ECONNABORTED		ECONNABORTED	/* Software caused connection abort */
#define	FI_ECONNRESET		ECONNRESET	/* Connection reset by peer */
//#define	FI_ENOBUFS		ENOBUFS		/* No buffer space available */
#define	FI_EISCONN		EISCONN		/* Transport endpoint is already connected */
#define	FI_ENOTCONN		ENOTCONN	/* Transport endpoint is not connected */
#define	FI_ESHUTDOWN		ESHUTDOWN	/* Cannot send after transport endpoint shutdown */
//#define	FI_ETOOMANYREFS		ETOOMANYREFS	/* Too many references: cannot splice */
#define	FI_ETIMEDOUT		ETIMEDOUT	/* Connection timed out */
#define	FI_ECONNREFUSED		ECONNREFUSED	/* Connection refused */
//#define	FI_EHOSTDOWN		EHOSTDOWN	/* Host is down */
#define	FI_EHOSTUNREACH		EHOSTUNREACH	/* No route to host */
#define	FI_EALREADY		EALREADY	/* Operation already in progress */
#define	FI_EINPROGRESS		EINPROGRESS	/* Operation now in progress */
//#define	FI_ESTALE		ESTALE		/* Stale NFS file handle */
//#define	FI_EUCLEAN		EUNCLEAN	/* Structure needs cleaning */
//#define	FI_ENOTNAM		ENOTNAM		/* Not a XENIX named type file */
//#define	FI_ENAVAIL		ENAVAIL		/* No XENIX semaphores available */
//#define	FI_EISNAM		EISNAM		/* Is a named type file */
#define	FI_EREMOTEIO		EREMOTEIO	/* Remote I/O error */
//#define	FI_EDQUOT		EDQUOT		/* Quota exceeded */
//#define	FI_ENOMEDIUM		ENOMEDIUM	/* No medium found */
//#define	FI_EMEDIUMTYPE		EMEDIUMTYPE	/* Wrong medium type */
#define	FI_ECANCELED		ECANCELED	/* Operation Canceled */

//#define	FI_EKEYEXPIRED		EKEYEXPIRED	/* Key has expired */
//#define	FI_EKEYREVOKED		EKEYREVOKED	/* Key has been revoked */
#define	FI_EKEYREJECTED		EKEYREJECTED	/* Key was rejected by service */
//#define	FI_EOWNERDEAD		EOWNERDEAD	/* Owner died */
//#define	FI_ENOTRECOVERABLE	ENOTRECOVERABLE	/* State not recoverable */

/* FI specific return values: >= 256 */

#define FI_EOTHER		256		/* Unspecified error */
#define FI_ETOOSMALL		257		/* Provided buffer is too small */
#define FI_EOPBADSTATE		258		/* Operation not permitted in current state */
#define FI_EAVAIL		259		/* Error available */
#define FI_EBADFLAGS		260		/* Flags not supported */
#define FI_ENOEQ		261		/* Missing or unavailable event queue */
#define FI_EDOMAIN		262		/* Invalid resource domain */
#define FI_ENOCQ		263		/* Missing or unavailable completion queue */
#define FI_ECRC			264		/* CRC error */
#define FI_ETRUNC		265		/* Truncation error */
#define FI_ENOKEY		266		/* Required key not available */

const char *fi_strerror(int errnum);

#ifdef __cplusplus
}
#endif

#endif /* _FI_ERRNO_H_ */
