/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004 The Ohio State University.
 *                    All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * MPI ping program
 *
 * Patterned after the example in the Quadrics documentation
 */

#include "ompi_config.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/select.h>

#include <getopt.h>

#include "mpi.h"

static int str2size(char *str)
{
    int size;
    char mod[32];

    switch (sscanf(str, "%d%1[mMkK]", &size, mod)) {
    case 1:
        return (size);

    case 2:
        switch (*mod) {
        case 'm':
        case 'M':
            return (size << 20);

        case 'k':
        case 'K':
            return (size << 10);

        default:
            return (size);
        }

    default:
        return (-1);
    }
}


static void usage(void)
{
    fprintf(stderr,
            "Usage: mpi-ping [flags] <min bytes> [<max bytes>] [<inc bytes>]\n"
            "       mpi-ping -h\n");
    exit(EXIT_FAILURE);
}


static void help(void)
{
    printf
        ("Usage: mpi-ping [flags] <min bytes> [<max bytes>] [<inc bytes>]\n"
         "\n" "   Flags may be any of\n"
         "      -B                use blocking send/recv\n"
         "      -C                check data\n"
         "      -O                overlapping pings\n"
         "      -W                perform warm-up phase\n"
         "      -r number         repetitions to time\n"
         "      -h                print this info\n" "\n"
         "   Numbers may be postfixed with 'k' or 'm'\n\n");

    exit(EXIT_SUCCESS);
}


int main(int argc, char *argv[])
{
    MPI_Status status;
    MPI_Request recv_request;
    MPI_Request send_request;
    char *rbuf;
    char *tbuf;
    int c;
    int i;
    int bytes;
    int nproc;
    int peer;
    int proc;
    int r;
    int tag = 0x666;

    /*
     * default options / arguments
     */
    int reps = 10000;
    int blocking = 0;
    int check = 0;
    int overlap = 0;
    int warmup = 0;
    int inc_bytes = 0;
    int max_bytes = 0;
    int min_bytes = 0;
 
    setenv("OMPI_MCA_ptl_base_exclude", "tcp", 1);
 
    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &proc);
    MPI_Comm_size(MPI_COMM_WORLD, &nproc);

    printf("FINISHED MPI_Comm_SIZE: size is %d\n",nproc);
    fflush(stdout);

    while ((c = getopt(argc, argv, "BCOWr:h")) != -1) {
        switch (c) {

        case 'B':
            blocking = 1;
            break;

        case 'C':
            check = 1;
            break;

        case 'O':
            overlap = 1;
            break;

        case 'W':
            warmup = 1;
            break;

        case 'r':
            if ((reps = str2size(optarg)) <= 0) {
                usage();
            }
            break;

        case 'h':
            help();

        default:
            usage();
        }
    }

    if (optind == argc) {
        min_bytes = 0;
    } else if ((min_bytes = str2size(argv[optind++])) < 0) {
        usage();
    }

    if (optind == argc) {
        max_bytes = min_bytes;
    } else if ((max_bytes = str2size(argv[optind++])) < min_bytes) {
        usage();
    }

    if (optind == argc) {
        inc_bytes = 0;
    } else if ((inc_bytes = str2size(argv[optind++])) < 0) {
        usage();
    }

    if (nproc == 1) {
        exit(EXIT_SUCCESS);
    }

    if ((rbuf = (char *) malloc(max_bytes ? max_bytes : 8)) == NULL) {
        perror("malloc");
        exit(EXIT_FAILURE);
    }

    if ((tbuf = (char *) malloc(max_bytes ? max_bytes : 8)) == NULL) {
        perror("malloc");
        exit(EXIT_FAILURE);
    }

    if (check) {
        for (i = 0; i < max_bytes; i++) {
            tbuf[i] = i & 255;
            rbuf[i] = 0;
        }
    }

    if (proc == 0) {
        if (overlap) {
            printf("mpi-ping: overlapping ping-pong\n");
        } else if (blocking) {
            printf("mpi-ping: ping-pong (using blocking send/recv)\n");
        } else {
            printf("mpi-ping: ping-pong\n");
        }
        if (check) {
            printf("data checking enabled\n");
        }
        printf("nprocs=%d, reps=%d, min bytes=%d, max bytes=%d inc bytes=%d\n",
               nproc, reps, min_bytes, max_bytes, inc_bytes);
        fflush(stdout);
    }

    MPI_Barrier(MPI_COMM_WORLD);

    peer = proc ^ 1;

    if ((peer < nproc) && (peer & 1)) {
        printf("%d pings %d\n", proc, peer);
        fflush(stdout);
    }

    MPI_Barrier(MPI_COMM_WORLD);

    if (warmup) {

        if (proc == 0) {
            puts("warm-up phase");
            fflush(stdout);
        }

        for (r = 0; r < reps; r++) {
            if (peer >= nproc) {
                break;
            }
            MPI_Irecv(rbuf, max_bytes, MPI_BYTE, peer, tag, MPI_COMM_WORLD,
                      &recv_request);
            MPI_Isend(tbuf, max_bytes, MPI_BYTE, peer, tag, MPI_COMM_WORLD,
                      &send_request);
            MPI_Wait(&send_request, &status);
            MPI_Wait(&recv_request, &status);
        }

        if (proc == 0) {
            puts("warm-up phase done");
            fflush(stdout);
        }
    }

    MPI_Barrier(MPI_COMM_WORLD);

    /*
     * Main loop
     */

    for (bytes = min_bytes; bytes <= max_bytes;
         bytes = inc_bytes ? bytes + inc_bytes : bytes ? 2 * bytes : 1) {

        double t = 0.0;
        double tv[2];

        r = reps;

        MPI_Barrier(MPI_COMM_WORLD);

        if (peer < nproc) {

            if (overlap) {

                /*
                 * MPI_Isend / MPI_Irecv overlapping ping-pong
                 */

                tv[0] = MPI_Wtime();

                for (r = 0; r < reps; r++) {

                    MPI_Irecv(rbuf, bytes, MPI_BYTE, peer, tag,
                              MPI_COMM_WORLD, &recv_request);
                    MPI_Isend(tbuf, bytes, MPI_BYTE, peer, tag,
                              MPI_COMM_WORLD, &send_request);
                    MPI_Wait(&send_request, &status);
                    MPI_Wait(&recv_request, &status);

                    if (check) {
                        for (i = 0; i < bytes; i++) {
                            if (rbuf[i] != (i & 255)) {
                                puts("mpi-ping: Error: Invalid data received");
                            }
                            rbuf[i] = 0;
                        }
                    }
                }

                tv[1] = MPI_Wtime();

            } else if (blocking) {

                /*
                 * MPI_Send / MPI_Recv ping-pong
                 */

                tv[0] = MPI_Wtime();

                if (peer < nproc) {
                    if (proc & 1) {
                        r--;
                        MPI_Recv(rbuf, bytes, MPI_BYTE, peer, tag,
                                 MPI_COMM_WORLD, &status);

                        if (check) {
                            for (i = 0; i < bytes; i++) {
                                if (rbuf[i] != (i & 255)) {
                                    puts("mpi-ping: Error: Invalid data received");
                                }
                                rbuf[i] = 0;
                            }
                        }
                    }

                    while (r-- > 0) {

                        MPI_Send(tbuf, bytes, MPI_BYTE, peer, tag,
                                 MPI_COMM_WORLD);
                        MPI_Recv(rbuf, bytes, MPI_BYTE, peer, tag,
                                 MPI_COMM_WORLD, &status);

                        if (check) {
                            for (i = 0; i < bytes; i++) {
                                if (rbuf[i] != (i & 255)) {
                                    puts("mpi-ping: Error: Invalid data received");
                                }
                                rbuf[i] = 0;
                            }
                        }
                    }

                    if (proc & 1) {
                        MPI_Send(tbuf, bytes, MPI_BYTE, peer, tag,
                                 MPI_COMM_WORLD);
                    }
                }

                tv[1] = MPI_Wtime();

            } else {

                /*
                 * MPI_Isend / MPI_Irecv ping-pong
                 */

                tv[0] = MPI_Wtime();

                if (peer < nproc) {
                    if (proc & 1) {
                        r--;
                        MPI_Irecv(rbuf, bytes, MPI_BYTE, peer, tag,
                                  MPI_COMM_WORLD, &recv_request);
                        MPI_Wait(&recv_request, &status);

                        if (check) {
                            for (i = 0; i < bytes; i++) {
                                if (rbuf[i] != (i & 255)) {
                                    puts("mpi-ping: Error: Invalid data received");
                                }
                                rbuf[i] = 0;
                            }
                        }
                    }

                    while (r-- > 0) {

                        MPI_Isend(tbuf, bytes, MPI_BYTE, peer, tag,
                                  MPI_COMM_WORLD, &send_request);
                        MPI_Wait(&send_request, &status);
                        MPI_Irecv(rbuf, bytes, MPI_BYTE, peer, tag,
                                  MPI_COMM_WORLD, &recv_request);
                        MPI_Wait(&recv_request, &status);

                        if (check) {
                            for (i = 0; i < bytes; i++) {
                                if (rbuf[i] != (i & 255)) {
                                    puts("mpi-ping: Error: Invalid data received");
                                }
                                rbuf[i] = 0;
                            }
                        }
                    }

                    if (proc & 1) {
                        MPI_Isend(tbuf, bytes, MPI_BYTE, peer, tag,
                                  MPI_COMM_WORLD, &send_request);
                        MPI_Wait(&send_request, &status);
                    }
                }

                tv[1] = MPI_Wtime();
            }

            /*
             * Calculate time interval in useconds (half round trip)
             */

            t = (tv[1] - tv[0]) * 1000000.0 / (2 * reps);

        }

        MPI_Barrier(MPI_COMM_WORLD);

        if ((peer < nproc) && (peer & 1)) {
            printf("%3d pinged %3d: %8d bytes %9.2f uSec %8.2f MB/s\n",
                   proc, peer, bytes, t, bytes / (t));
            fflush(stdout);
        }
    }

    MPI_Barrier(MPI_COMM_WORLD);
    MPI_Finalize();

    return EXIT_SUCCESS;
}
