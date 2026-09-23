/*
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * A stranger dials the TCP BTL's listening socket and sends a handshake
 * the BTL must refuse: a well-formed one naming a process that does not
 * exist, one whose magic string is wrong, and one that is truncated.
 * All three have to be dropped, and -- the part worth a test -- the job
 * has to carry on over the connections it already had.
 *
 * The accept path decides whether an inbound socket belongs to a known
 * peer, and it is easy to make it either too strict (dropping a good
 * connection, the #3035 bug) or too lax (adopting a socket for a peer we
 * cannot identify).  A test that only checked the connection was refused
 * would pass against a BTL that refused every connection, so this one
 * sends real messages both before and after and verifies the payload.
 *
 * Run this with the TCP BTL forced and nothing to fall back to:
 *
 *   mpirun -n 2 --mca btl self,tcp ./tcp_stranger_connect
 *
 * Finding the listener is the awkward part, since only the BTL knows
 * where it bound.  Rather than reach into the component, the test pins
 * the port range itself, before MPI_Init, to a window that depends on
 * its rank -- so every rank's listener is in a range every other rank
 * can compute.  That also keeps a rank from ever dialling its own
 * listener, which would deadlock: the socket is refused by the progress
 * engine of the process that owns it, and a process sitting in read()
 * is not running its own progress engine.
 *
 * The rank has to be known before MPI_Init to set an MCA parameter, so
 * it comes from the environment, which means this test only works under
 * a launcher -- which is the premise of this directory.
 */

#include <mpi.h>

#include <arpa/inet.h>
#include <errno.h>
#include <netinet/in.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <unistd.h>

/* Mirrors mca_btl_tcp_endpoint_hs_msg_t.  Deliberately spelled out
 * rather than included: this test is compiled with the installed mpicc
 * and has no access to the BTL's internal headers.  The guid is two
 * 32-bit fields, and it only goes through htonl() on a build configured
 * for heterogeneous support -- which does not matter here, because no
 * byte order of these values names a real process.
 */
#define STRANGER_MAGIC_LEN 16
static const char stranger_magic[STRANGER_MAGIC_LEN] = "OPAL-TCP-BTL";

struct stranger_hs_msg {
    uint32_t jobid;
    uint32_t vpid;
    char magic_id[STRANGER_MAGIC_LEN];
};

/* A rejection can be slow: a well-formed handshake naming a process
 * that does not exist sends the receiver to the modex for it, and the
 * lookup for an unknown process is not a local operation.  Only a peer
 * that has gone away should ever hit this, and the test fails on the
 * timeout rather than hanging on it. */
#define STRANGER_TIMEOUT_SEC 30

/* Each rank gets its own window, so no rank ever dials its own
 * listener.  One listening socket per rank per address family is all
 * the BTL needs, so the window only has to be wide enough to absorb a
 * port in the window being taken by something else. */
#define STRANGER_PORT_BASE 39000
#define STRANGER_PORT_STRIDE 8

enum stranger_flavor {
    STRANGER_BAD_GUID = 0, /* correct magic, a process that cannot exist */
    STRANGER_BAD_MAGIC,    /* not an Open MPI peer at all */
    STRANGER_TRUNCATED,    /* correct magic, handshake cut short */
    STRANGER_FLAVOR_COUNT
};

static const char *stranger_flavor_name(enum stranger_flavor flavor)
{
    switch (flavor) {
    case STRANGER_BAD_GUID:
        return "bogus guid";
    case STRANGER_BAD_MAGIC:
        return "bad magic string";
    case STRANGER_TRUNCATED:
        return "truncated handshake";
    default:
        return "unknown";
    }
}

static int stranger_dial(int port)
{
    struct sockaddr_in sin;
    struct timeval tv;
    int sd = socket(AF_INET, SOCK_STREAM, 0);

    if (0 > sd) {
        return -1;
    }

    memset(&sin, 0, sizeof(sin));
    sin.sin_family = AF_INET;
    sin.sin_port = htons((uint16_t) port);
    /* The BTL binds INADDR_ANY, and these tests all run on one node. */
    sin.sin_addr.s_addr = htonl(INADDR_LOOPBACK);

    if (0 > connect(sd, (struct sockaddr *) &sin, sizeof(sin))) {
        close(sd);
        return -1;
    }

    tv.tv_sec = STRANGER_TIMEOUT_SEC;
    tv.tv_usec = 0;
    (void) setsockopt(sd, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));
    return sd;
}

static int stranger_write_all(int sd, const void *buf, size_t len)
{
    size_t off = 0;

    while (off < len) {
        ssize_t w = write(sd, (const char *) buf + off, len - off);
        if (0 >= w) {
            return -1;
        }
        off += (size_t) w;
    }
    return 0;
}

/* Returns 1 if the peer demonstrably dropped us, 0 if we could not
 * tell, and -1 if it did something a BTL must never do.  Every port that
 * accepted us is reported, so that a run which finds no listener at all
 * is distinguishable from one whose listener misbehaved. */
static int stranger_probe(int port, enum stranger_flavor flavor)
{
    struct stranger_hs_msg hs;
    char scratch[64];
    ssize_t r;
    int sd = stranger_dial(port);

    if (0 > sd) {
        return 0; /* nothing listening here */
    }
    printf("  port %d: dialled, sending %s\n", port, stranger_flavor_name(flavor));
    fflush(stdout);

    memset(&hs, 0, sizeof(hs));
    hs.jobid = 0xdeadbeefu;
    hs.vpid = 0x0000c0deu;
    if (STRANGER_BAD_MAGIC == flavor) {
        memcpy(hs.magic_id, "NOT-OPAL-TCP", 12);
    } else {
        memcpy(hs.magic_id, stranger_magic, STRANGER_MAGIC_LEN);
    }

    if (STRANGER_TRUNCATED == flavor) {
        /* Send a fragment and hang up.  Leaving the socket idle instead
         * would cost the peer btl_tcp_handshake_timeout (1s) of its
         * progress thread before it gave up, which is bounded but not
         * something a test should spend. */
        (void) stranger_write_all(sd, &hs, 4);
        close(sd);
        return 0; /* we hung up first, so there is nothing to observe */
    }

    if (0 > stranger_write_all(sd, &hs, sizeof(hs))) {
        close(sd);
        return 0;
    }

    r = read(sd, scratch, sizeof(scratch));
    close(sd);

    if (0 == r) {
        printf("  port %d: refused the %s\n", port, stranger_flavor_name(flavor));
        fflush(stdout);
        return 1; /* clean hang-up: refused, as required */
    }
    if (0 > r) {
        /* A timeout means we are dialling our own listener and cannot
         * progress it; a reset means it went away.  Neither is a
         * failure, but neither is evidence either. */
        printf("  port %d: no verdict on the %s (%s)\n", port, stranger_flavor_name(flavor),
               strerror(errno));
        fflush(stdout);
        return 0;
    }

    /* It answered us.  The BTL has no reply to make to a handshake it
     * cannot attribute, so this is a real finding. */
    fprintf(stderr, "ERROR: port %d answered a %s with %ld bytes\n", port,
            stranger_flavor_name(flavor), (long) r);
    return -1;
}

/* Exchange a verified payload with every other rank, both directions. */
static int stranger_exchange(int rank, int size, int tag)
{
    unsigned char *out = malloc(8192);
    unsigned char *in = malloc(8192);
    int peer, i, rc = 0;

    if (NULL == out || NULL == in) {
        free(out);
        free(in);
        return -1;
    }

    for (peer = 0; peer < size; peer++) {
        if (peer == rank) {
            continue;
        }
        for (i = 0; i < 8192; i++) {
            out[i] = (unsigned char) (i + rank + tag);
        }
        memset(in, 0, 8192);
        MPI_Sendrecv(out, 8192, MPI_BYTE, peer, tag, in, 8192, MPI_BYTE, peer, tag,
                     MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        for (i = 0; i < 8192; i++) {
            if (in[i] != (unsigned char) (i + peer + tag)) {
                fprintf(stderr, "ERROR: rank %d got corrupt byte %d from rank %d\n", rank, i, peer);
                rc = -1;
                break;
            }
        }
        if (0 != rc) {
            break;
        }
    }

    free(out);
    free(in);
    return rc;
}

int main(int argc, char *argv[])
{
    char buf[32];
    const char *env_rank = getenv("OMPI_COMM_WORLD_RANK");
    int rank, size, peer, port;
    int confirmed = 0, answered = 0, refused_guid = 0, refused_magic = 0;

    if (NULL == env_rank) {
        fprintf(stderr, "ERROR: OMPI_COMM_WORLD_RANK is unset; run this under mpirun\n");
        return 1;
    }

    /* Pin our own listener into a window derived from our rank, so that
     * every rank knows where every other rank is listening.  This has to
     * happen before MPI_Init, which is why the rank comes from the
     * environment rather than from MPI_Comm_rank. */
    snprintf(buf, sizeof(buf), "%d", STRANGER_PORT_BASE + atoi(env_rank) * STRANGER_PORT_STRIDE);
    setenv("OMPI_MCA_btl_tcp_port_min_v4", buf, 1);
    snprintf(buf, sizeof(buf), "%d", STRANGER_PORT_STRIDE);
    setenv("OMPI_MCA_btl_tcp_port_range_v4", buf, 1);

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if (2 > size) {
        if (0 == rank) {
            fprintf(stderr, "ERROR: this test requires at least 2 ranks\n");
        }
        MPI_Abort(MPI_COMM_WORLD, 1);
        return 1;
    }
    if (rank != atoi(env_rank)) {
        fprintf(stderr, "ERROR: rank %d was launched as %s, so the port windows do not line up\n",
                rank, env_rank);
        MPI_Abort(MPI_COMM_WORLD, 1);
        return 1;
    }

    /* Bring the real endpoints up before anybody dials a stranger, so
     * that what follows is tested against a wired-up job. */
    if (0 != stranger_exchange(rank, size, 1)) {
        MPI_Abort(MPI_COMM_WORLD, 1);
        return 1;
    }

    /* Rank 0 dials every other rank's window; everybody else waits in
     * the barrier below, where they progress their own listener and so
     * do the refusing. */
    if (0 == rank) {
        for (peer = 1; peer < size; peer++) {
            int base = STRANGER_PORT_BASE + peer * STRANGER_PORT_STRIDE;
            for (port = base; port < base + STRANGER_PORT_STRIDE; port++) {
                enum stranger_flavor flavor;
                for (flavor = 0; flavor < STRANGER_FLAVOR_COUNT; flavor++) {
                    int rc = stranger_probe(port, flavor);
                    if (0 > rc) {
                        answered++;
                        continue;
                    }
                    confirmed += rc;
                    if (0 < rc && STRANGER_BAD_GUID == flavor) {
                        refused_guid++;
                    }
                    if (0 < rc && STRANGER_BAD_MAGIC == flavor) {
                        refused_magic++;
                    }
                }
            }
        }
    }

    MPI_Barrier(MPI_COMM_WORLD);

    /* The whole point: the connections we already had are still good. */
    if (0 != stranger_exchange(rank, size, 2)) {
        MPI_Abort(MPI_COMM_WORLD, 1);
        return 1;
    }
    MPI_Barrier(MPI_COMM_WORLD);

    if (0 == rank) {
        if (0 != answered) {
            fprintf(stderr, "ERROR: %d stranger handshake(s) were answered\n", answered);
            MPI_Abort(MPI_COMM_WORLD, 1);
            return 1;
        }
        /* Both of these must be refused by every peer.  Requiring them
         * separately is what keeps the test honest: a BTL that refused
         * on the magic string alone would still adopt a well-formed
         * handshake naming a process it cannot identify, and a count of
         * "some connection was refused" would not notice. */
        if (refused_guid < size - 1 || refused_magic < size - 1) {
            fprintf(stderr,
                    "ERROR: expected %d peers to refuse both handshakes, got "
                    "%d bogus-guid and %d bad-magic refusals; is the TCP BTL in use?\n",
                    size - 1, refused_guid, refused_magic);
            MPI_Abort(MPI_COMM_WORLD, 1);
            return 1;
        }
        printf("tcp stranger connect: PASSED (%d refused across %d ranks)\n", confirmed, size);
    }

    MPI_Finalize();
    return 0;
}
