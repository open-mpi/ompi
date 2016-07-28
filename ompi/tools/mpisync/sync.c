/*
 * Copyright (C) 2014      Artem Polyakov <artpol84@gmail.com>
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <stdio.h>
#include <mpi.h>
#include <unistd.h>
#include <getopt.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>

#include "hpctimer.h"
#include "mpigclock.h"

typedef enum { Gen, Chk } prog_mode_t;

char *filename = NULL;
prog_mode_t mode = Gen;
void print_help(char *progname);
int parse_opts(int rank, int argc, char **argv);

void print_help(char *progname)
{
    printf("%s: ./%s -o <output file>\n", progname, progname);
}

int parse_opts(int rank, int argc, char **argv)
{
    while (1) {
        int option_index = 0;
        static struct option long_options[] = {
            {"output", required_argument, 0, 'o' },
            {"help",   required_argument, 0, 'h' },
            { 0,       0,                 0, 0   } };

        int c = getopt_long(argc, argv, "o:h",
            long_options, &option_index);
        if (c == -1)
            break;
        switch (c) {
        case 'h':
            if( rank == 0 )
                print_help(argv[0]);
            return 1;
        case 'o':
            filename = strdup(optarg);
            if( filename == NULL ){
                perror("Cannot allocate memory");
                return -1;
            }
            break;
        default:
            return -1;
        }
    }
    return 0;
}

int main(int argc, char **argv)
{
    MPI_Init(&argc, &argv);
    MPI_Comm comm = MPI_COMM_WORLD;
    int rank, commsize;
    double offs = 0, rtt = 0;
    char hname[OPAL_MAXHOSTNAMELEN];

    MPI_Comm_rank(comm, &rank);
    MPI_Comm_size(comm, &commsize);

    int ret = parse_opts(rank, argc, argv);
    if( ret < 0 ){
        // Error exit
        MPI_Finalize();
        exit(1);
    }else if( ret > 0 ){
        // Normal exit after help printout
        MPI_Finalize();
        exit(0);
    }

    if( filename == NULL ){
        if( rank == 0 ){
            print_help(argv[0]);
        }
        MPI_Finalize();
        exit(1);
    }

    if( gethostname(hname, sizeof(hname)) ){
        perror("Cannot get hostname. Abort");
        MPI_Abort(MPI_COMM_WORLD, 1);
    }

    int rc = hpctimer_initialize("gettimeofday");

    if( rc == HPCTIMER_FAILURE ){
        fprintf(stderr, "Fail to initialize hpc timer. Abort\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
    }

    offs = mpigclock_sync_linear(comm, 0, &rtt);

    double send[2] = { rtt, offs };
    if( rank == 0 ){
        double *measure = malloc(commsize*2*sizeof(double));
        if( measure == NULL ){
             fprintf(stderr, "Fail to allocate memory. Abort\n");
             MPI_Abort(MPI_COMM_WORLD, 1);
        }
        char *hnames = malloc(OPAL_MAXHOSTNAMELEN * commsize);
        if( hnames == NULL ){
             fprintf(stderr, "Fail to allocate memory. Abort\n");
             MPI_Abort(MPI_COMM_WORLD, 1);
        }

        MPI_Gather(hname,sizeof(hname),MPI_CHAR,hnames,sizeof(hname),MPI_CHAR, 0, MPI_COMM_WORLD);
        MPI_Gather(send,2,MPI_DOUBLE,measure,2, MPI_DOUBLE, 0, MPI_COMM_WORLD);
        char tmpname[128];
        FILE *fp = fopen(filename,"w");
        if( fp == NULL ){
             fprintf(stderr, "Fail to open the file %s. Abort\n", filename);
             MPI_Abort(MPI_COMM_WORLD, 1);
        }
        double (*m)[2] = (void*)measure;
        char (*h)[OPAL_MAXHOSTNAMELEN] = (void*)hnames;
        int i;
        for(i=0; i<commsize;i++){
            fprintf(fp, "%s %lf %lf\n", h[i], m[i][0], m[i][1]);
        }
        fclose(fp);
    } else {
        MPI_Gather(hname, sizeof(hname), MPI_CHAR, NULL, sizeof(hname), MPI_CHAR, 0, MPI_COMM_WORLD);
        MPI_Gather(send,2, MPI_DOUBLE, NULL, 2, MPI_DOUBLE, 0, MPI_COMM_WORLD);
    }

    MPI_Finalize();
    return 0;
}
