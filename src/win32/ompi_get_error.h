/*
 * $HEADER$
 */
#ifndef OMPI_GET_ERROR_H
#define OMPI_GET_ERROR_H

#include <errno.h>

#define EWOULDBLOCK       WSAEWOULDBLOCK       
#define EINPROGRESS       WSAEINPROGRESS     
#define EALREADY          WSAEALREADY        
#define ENOTSOCK          WSAENOTSOCK        
#define EDESTADDRREQ      WSAEDESTADDRREQ    
#define EMSGSIZE          WSAEMSGSIZE        
#define EPROTOTYPE        WSAEPROTOTYPE      
#define ENOPROTOOPT       WSAENOPROTOOPT     
#define EPROTONOSUPPORT   WSAEPROTONOSUPPORT 
#define ESOCKTNOSUPPORT   WSAESOCKTNOSUPPORT 
#define EOPNOTSUPP        WSAEOPNOTSUPP      
#define EPFNOSUPPORT      WSAEPFNOSUPPORT    
#define EAFNOSUPPORT      WSAEAFNOSUPPORT    
#define EADDRINUSE        WSAEADDRINUSE      
#define EADDRNOTAVAIL     WSAEADDRNOTAVAIL   
#define ENETDOWN          WSAENETDOWN        
#define ENETUNREACH       WSAENETUNREACH     
#define ENETRESET         WSAENETRESET       
#define ECONNABORTED      WSAECONNABORTED    
#define ECONNRESET        WSAECONNRESET      
#define ENOBUFS           WSAENOBUFS         
#define EISCONN           WSAEISCONN         
#define ENOTCONN          WSAENOTCONN        
#define ESHUTDOWN         WSAESHUTDOWN       
#define ETOOMANYREFS      WSAETOOMANYREFS    
#define ETIMEDOUT         WSAETIMEDOUT       
#define ECONNREFUSED      WSAECONNREFUSED    
#define ELOOP             WSAELOOP           
#define EHOSTDOWN         WSAEHOSTDOWN       
#define EHOSTUNREACH      WSAEHOSTUNREACH    
#define EPROCLIM          WSAEPROCLIM        
#define EUSERS            WSAEUSERS          
#define EDQUOT            WSAEDQUOT          
#define ESTALE            WSAESTALE          
#define EREMOTE           WSAEREMOTE         


/*
 * pound define ompi_get_error() to be ompi_errno. so, in windows land
 * this simply defaults to being errno
 */

#define ompi_errno ompi_get_errno()

#if defined (cplusplus) || defined (__cplusplus)
extern "C" {
#endif
    
static __inline int ompi_get_errno(void) {
    int ret = WSAGetLastError();
    switch (ret) {
      case WSAEINTR: ret =EINTR; break;
      case WSAEBADF: ret =EBADF; break;
      case WSAEACCES: ret =EACCES; break;          
      case WSAEFAULT: ret =EFAULT; break;         
      case WSAEINVAL: ret =EINVAL; break;         
      case WSAEMFILE: ret =EMFILE; break;         
      case WSAEWOULDBLOCK: ret =EWOULDBLOCK; break;     
      case WSAEINPROGRESS: ret =EINPROGRESS; break;    
      case WSAEALREADY: ret =EALREADY; break;        
      case WSAENOTSOCK: ret =ENOTSOCK; break;       
      case WSAEDESTADDRREQ: ret =EDESTADDRREQ; break;    
      case WSAEMSGSIZE: ret =EMSGSIZE; break;      
      case WSAEPROTOTYPE: ret =EPROTOTYPE; break;      
      case WSAENOPROTOOPT: ret =ENOPROTOOPT; break;    
      case WSAEPROTONOSUPPORT: ret =EPROTONOSUPPORT; break; 
      case WSAESOCKTNOSUPPORT: ret =ESOCKTNOSUPPORT; break;
      case WSAEOPNOTSUPP: ret =EOPNOTSUPP; break;     
      case WSAEPFNOSUPPORT: ret =EPFNOSUPPORT; break;   
      case WSAEAFNOSUPPORT: ret =EAFNOSUPPORT; break;   
      case WSAEADDRINUSE: ret =EADDRINUSE; break;     
      case WSAEADDRNOTAVAIL: ret =EADDRNOTAVAIL; break;   
      case WSAENETDOWN: ret =ENETDOWN; break;      
      case WSAENETUNREACH: ret =ENETUNREACH; break;     
      case WSAENETRESET: ret =ENETRESET; break;     
      case WSAECONNABORTED: ret =ECONNABORTED; break;    
      case WSAECONNRESET: ret =ECONNRESET; break;     
      case WSAENOBUFS: ret =ENOBUFS; break;        
      case WSAEISCONN: ret =EISCONN; break;        
      case WSAENOTCONN: ret =ENOTCONN; break;       
      case WSAESHUTDOWN: ret =ESHUTDOWN; break;       
      case WSAETOOMANYREFS: ret =ETOOMANYREFS; break;   
      case WSAETIMEDOUT: ret =ETIMEDOUT; break;      
      case WSAECONNREFUSED: ret =ECONNREFUSED; break;   
      case WSAELOOP: ret =ELOOP; break;          
      case WSAENAMETOOLONG: ret =ENAMETOOLONG; break;   
      case WSAEHOSTDOWN: ret =EHOSTDOWN; break;      
      case WSAEHOSTUNREACH: ret =EHOSTUNREACH; break;   
      case WSAENOTEMPTY: ret =ENOTEMPTY; break;      
      case WSAEPROCLIM: ret =EPROCLIM; break;       
      case WSAEUSERS: ret =EUSERS; break;         
      case WSAEDQUOT: ret =EDQUOT; break;         
      case WSAESTALE: ret =ESTALE; break;         
      case WSAEREMOTE: ret =EREMOTE; break;        
      default: printf("Feature not implemented: %d %s\n", __LINE__, __FILE__);
    };                                
    return ret;
}

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* OMPI_GET_ERROR_H */
