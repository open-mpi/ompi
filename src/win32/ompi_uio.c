/*
 $HEADER$
 */

#include "ompi_config.h"
#include "win32/ompi_uio.h"
#include <errno.h>

/*
 Highly doubt if the windows sockets ever set errno to EAGAIN. There might
 be some weird conversion to map this or I might have to rewrite this piece
 of code to handle the windows error flags 
 */

int writev(int fd,struct iovec * iov,int cnt) {
    
    int i;
    int ret = 0;
    int rc; 
    
    for( i = 0; i < cnt; i++ ) {
        rc=writeconn( (SOCKET)fd, (char *)iov[i].iov_base, iov[i].iov_len );

        /* return 0 if this is a non-blocking socket and no data was available
           for reading. Else, return the error back to the users */
        if(rc < 0) {
             return (errno == EAGAIN) ? ret : rc;
         }
         ret += rc;
    }

    return ret;
} 


int readv(int fd,struct iovec * iov,int cnt) {
    
    int i;
    int ret = 0;
    int rc; 
    
    for( i = 0; i < cnt; i++ ) {
        rc=readconn( (SOCKET)fd, (char *)iov[i].iov_base, iov[i].iov_len );

        /* return 0 if this is a non-blocking socket and no data was available
           for reading. Else, return the error back to the users */
        if(rc < 0) {
             return (errno == EAGAIN) ? ret : rc;
         }
         ret += rc;
    }

    return ret;
} 

/*
 ANJU: This function does not handle the non-blocking IO at all. Yet
 to add modifications which will enable non-blocking IO */
int writeconn (int s, char *data, int len) {

    int i;
    int tosend = len;
    int gone = 0;
    int fluffed = 0;

    for (i=0; tosend > 0; ) {

        gone = write(s, &data[i], tosend);

        if (0 > gone) {
            fluffed++;
            if (RETRIES == fluffed) {
              /* There was code in here to close the socket. I think 
                 that would not be the best thing to do in case of  
                 nono-blocking sockets. In any case, I need to look
                 up hpw winsocks function before making concrete 
                 changes. This will do for compilation for now */
                return (i);
            }
        } else {
            i += gone; 
            tosend -= gone;
            fluffed = 0;
        }
    }

    return i;
}

/*
 ANJU: This function does not handle the non-blocking IO at all. Yet
 to add modifications which will enable non-blocking IO */
int readconn (int s, char *data, int len) {

    int i;
    unsigned int toget = (unsigned)len;
    int got, fluffed = 0;

    for (i=0; toget > 0; ) {

        got = read(s, &data[i], toget);
        
        /* Check if some error has occured */
        if (0 > got) {
            fluffed++;
            if (RETRIES == fluffed) {
              /* There was code in here to close the socket. I think 
                 that would not be the best thing to do in case of  
                 nono-blocking sockets. In any case, I need to look
                 up hpw winsocks function before making concrete 
                 changes. This will do for compilation for now */
                return (i);
            }
        } else {
            i += got; 
            toget -= got;
            fluffed = 0;
        }
    }

    return i;
}
