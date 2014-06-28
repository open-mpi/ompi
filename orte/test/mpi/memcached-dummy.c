#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#include <mpi.h>


void do_server(void)
{
#if 0
    /* temporary hack, just use pid to identify service */
#define MAX_SERVER_ID_LEN (256)
    int server_id = getpid();
    char server_id_str[MAX_SERVER_ID_LEN];
    snprintf(server_id_str, MAX_SERVER_ID_LEN, "server-%d", server_id);
#endif
    char port_name[MPI_MAX_PORT_NAME];
    MPI_Comm accept_comm;
    int recvbuf = -1;

    MPI_Open_port(MPI_INFO_NULL, port_name);
    MPI_Publish_name("memcached", MPI_INFO_NULL, port_name);

    MPI_Comm_accept(port_name, MPI_INFO_NULL, 0, MPI_COMM_WORLD, &accept_comm);
    printf("accepted an incoming connection\n");
    MPI_Recv(&recvbuf, 1, MPI_INT, 0, 0, accept_comm, MPI_STATUS_IGNORE);
    printf("received '%d' from peer\n", recvbuf);
    MPI_Comm_disconnect(&accept_comm);

    MPI_Unpublish_name("memcached", MPI_INFO_NULL, port_name);
    MPI_Close_port(port_name);
}

void do_client(void)
{
    char port_name[MPI_MAX_PORT_NAME];
    MPI_Comm connect_comm;
    int sendbuf = -1;

    /* wait for server to publish */
    sleep(3);
    MPI_Lookup_name("memcached", MPI_INFO_NULL, port_name);
    fprintf(stderr, "GOT PORT %s", port_name);

    MPI_Comm_connect(port_name, MPI_INFO_NULL, 0, MPI_COMM_WORLD, &connect_comm);
    printf("connected to port name '%s'\n", port_name);
    sendbuf = getpid();
    MPI_Send(&sendbuf, 1, MPI_INT, 0, 0, connect_comm);
    MPI_Comm_disconnect(&connect_comm);
}

int main(int argc, char **argv)
{
    int wrank, wsize;
    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &wrank);
    MPI_Comm_size(MPI_COMM_WORLD, &wsize);

    /* expects to be called with one arg:
     * - the literal string "server" or "client"
     *
     * Extra arguments are ignored.
     */
    assert(argc >= 2);
    printf("memcached-dummy, MPI_COMM_WORLD process %d/%d, argv[1]=%s\n", wrank, wsize, argv[1]);

    if (0 == strcmp(argv[1], "server")) {
        do_server();
    }
    else {
        do_client();
    }

    MPI_Finalize();
    return 0;
}
