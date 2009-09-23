/* -*- C -*-
 *
 * $HEADER$
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <netinet/in.h>
#include <signal.h>
#include <pthread.h>
#include <unistd.h>
#include <time.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>

int main(int argc, char* argv[])
{
    int rc, i;
    char hostname[512];
    pid_t pid;
    int32_t i32=1;
    int xmit, recv;
    uint8_t ttl = 1;
    uint16_t port=0;
    struct sockaddr_in rx, inaddr;
    struct ip_mreq req;
    int addrlen;
    uint32_t channel;
    uint8_t bytes[256];
    struct timespec ts, tsrem;
    int flags;
    fd_set fdset;
    fd_set errset;
    struct timeval tv;
    char *nprocstr;
    int nprocs;
    
    gethostname(hostname, 512);
    pid = getpid();

    printf("orte_mcast: Node %s Pid %ld\n", hostname, (long)pid);

    nprocstr = getenv("OMPI_COMM_WORLD_SIZE");
    nprocs = strtol(nprocstr, NULL, 10);
    
    /* create a recv socket */
    recv = socket(PF_INET, SOCK_DGRAM, IPPROTO_UDP);
    if(recv < 0) {
        fprintf(stderr,"%d: rmcast:init: socket() failed\n", (int)pid);
        exit(1);
    }
    
    flags = 1;
    if (setsockopt (recv, SOL_SOCKET, SO_REUSEADDR, (const char *)&flags, sizeof(flags)) < 0) {
        fprintf(stderr, "rmcast:basic: unable to set the SO_REUSEADDR option\n");
        exit(1);
    }
    memset(&rx, 0, sizeof(rx));
    rx.sin_family = AF_INET;
    rx.sin_addr.s_addr = htonl(0xEFFF0001);
    rx.sin_port = htons(5002);
    
    /* bind the socket */
    if (bind(recv, (struct sockaddr*)&rx, sizeof(rx)) < 0) {
        fprintf(stderr, "%d: rmcast:init: bind()\n", (int)pid);
        exit(1);
    }
    
    /* set membership to "any" */
    memset(&req, 0, sizeof (req));
    req.imr_multiaddr.s_addr = htonl(0xEFFF0001);
    req.imr_interface.s_addr = htonl(INADDR_ANY);
    
    if ((setsockopt(recv, IPPROTO_IP, IP_ADD_MEMBERSHIP, 
                    (void *)&req, sizeof (req))) < 0) {
        fprintf(stderr, "%d: rmcast:init: sockopt()\n", (int)pid);
        exit(1);
    }

    /* create a xmit socket */
    xmit = socket(PF_INET, SOCK_DGRAM, IPPROTO_UDP);
    if(xmit < 0) {
        fprintf(stderr,"%d: rmcast:init: socket() failed\n", (int)pid);
        exit(1);
    }
    
    /* set the multicast flags */
    if ((setsockopt(xmit, IPPROTO_IP, IP_MULTICAST_TTL, 
                    (void *)&ttl, sizeof(ttl))) < 0) {
        fprintf(stderr,"%d: rmcast:init: socketopt() failed\n", (int)pid);
        exit(1);
    }
    
    flags = 1;
    if (setsockopt (xmit, SOL_SOCKET, SO_REUSEADDR, (const char *)&flags, sizeof(flags)) < 0) {
        fprintf(stderr, "rmcast:basic: unable to set the SO_REUSEADDR option\n");
        exit(1);
    }
    memset(&inaddr, 0, sizeof(inaddr));
    inaddr.sin_family = AF_INET;
    inaddr.sin_addr.s_addr = htonl(0xEFFF0001);
    inaddr.sin_port = htons(5002);
    addrlen = sizeof(struct sockaddr_in);

    /* set membership to "any" */
    memset(&req, 0, sizeof (req));
    req.imr_multiaddr.s_addr = htonl(0xEFFF0001);
    req.imr_interface.s_addr = htonl(INADDR_ANY);
    
    if ((setsockopt(xmit, IPPROTO_IP, IP_ADD_MEMBERSHIP, 
                    (void *)&req, sizeof (req))) < 0) {
        fprintf(stderr, "setsockopt() failed\n");
        exit(1);
    }
    if ((rc = sendto(xmit, (char*)bytes, 256, 0, (struct sockaddr *)&inaddr, sizeof(struct sockaddr_in))) != 256) {
        fprintf(stderr, "%d: send error %d\n", (int)pid, errno);
        exit(1);
    }
    fprintf(stderr, "%d: MESSAGE SENT\n", (int)pid);

    ts.tv_sec = 0;
    ts.tv_nsec = 450*1000;
    
    while (nanosleep(&ts, &tsrem) < 0) {
        ts = tsrem;
    }

    tv.tv_sec = 1;
    tv.tv_usec = 100*1000; 
    FD_ZERO(&fdset);
    FD_ZERO(&errset);
    FD_SET(recv, &fdset);
    
    for (i=0; i < nprocs; i++) {
        while ((rc = select(recv+1, 
                            &fdset /* read-fds */, 0 /* write-fds */,
                            &errset /* error-fds */, NULL)) < 0) {
            fprintf(stderr, "select\n");
        }
        if (0 == rc) {
            fprintf(stderr, "select failed to find anything - errno %d\n", errno);
            exit(0);
        }
        fprintf(stderr, "%d: MESSAGE ARRIVED...READING DATA\n", (int)pid);
        
        addrlen = sizeof(rx);
        rc = recvfrom(recv, bytes, 256, 0, (struct sockaddr *)&rx, &addrlen);
        
        fprintf(stderr, "%d: RECVD %d bytes\n", (int)pid, rc);
    }

    
    exit(0);
}
