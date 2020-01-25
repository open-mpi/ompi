/*
 * Copyright (C) 2014      Artem Polyakov <artpol84@gmail.com>
 * Copyright (c) 2014-2017 Intel, Inc. All rights reserved.
 * Copyright (c) 2019      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/runtime/opal.h"

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

static char *filename = NULL;
static int alg = 0;
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
            {"alg", required_argument, 0, 'a' },
            {"help",   required_argument, 0, 'h' },
            { 0,       0,                 0, 0   } };

        int c = getopt_long(argc, argv, "o:a:h",
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
        case 'a':
            alg = atoi(optarg);
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
    const char *local_hname;
 
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
    /* All error checking for getting the hostname is done with the initial
       populating of opal_process_info.nodename inside opal/runtime/opal_init.c */
    local_hname = opal_gethostname();

    /* Truncate hostname if it is longer than OPAL_MAXHOSTNAMELEN,
       since we are only reading that length with the MPI_Gather.
       If full and complete hostnames are necessary in all cases,
       this could be implemented with MPI_Gatherv rather than
       MPI_Gather, instead of using the longer but more
       accurate OPAL_LOCAL_MAXHOSTNAMELEN value, because for very
       large task counts there is a risk of running out of memory
       if OPAL_LOCAL_MAXHOSTNAMELEN is used. */
    strncpy(hname, local_hname, OPAL_MAXHOSTNAMELEN - 1);
    hname[OPAL_MAXHOSTNAMELEN - 1] = '\0';

    int rc = hpctimer_initialize("gettimeofday");

    if( rc == HPCTIMER_FAILURE ){
        fprintf(stderr, "Fail to initialize hpc timer. Abort\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
    }


    if (commsize < 2) {
        rtt = 0.0;
        offs = 0.0;
    } else {
        if (1 == alg) {
            offs = mpigclock_sync_log(comm, 0, &rtt);
        } else {
            offs = mpigclock_sync_linear(comm, 0, &rtt);
        }
    }

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
        FILE *fp = fopen(filename,"w");
        if( fp == NULL ){
             fprintf(stderr, "Fail to open the file %s. Abort\n", filename);
             MPI_Abort(MPI_COMM_WORLD, 1);
        }
        double (*m)[2] = (void*)measure;
        char (*h)[OPAL_MAXHOSTNAMELEN] = (void*)hnames;
        int i;
        fprintf(fp, "# Used algorithm: %s\n", (alg ? "binary tree" : "linear"));
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
