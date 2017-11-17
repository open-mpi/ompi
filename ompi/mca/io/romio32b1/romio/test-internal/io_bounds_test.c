/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *   Copyright (C) 2008 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "../adio/include/adio.h"
#include "../adio/include/adioi.h"
#include "../adio/include/adio_extern.h"
#include "mpi.h"

#define PREDEF_TESTS 5
#define MAX_OFF_LENS 4

typedef struct {
    ADIO_Offset offset;
    int count;
    int type_blocklens[MAX_OFF_LENS];
    int type_indices[MAX_OFF_LENS];
    MPI_Datatype type_oldtypes[MAX_OFF_LENS];
    int type_count;

    ADIO_Offset correct_st_offset;
    ADIO_Offset correct_end_offset;
} test_param_t;

int run_test (test_param_t *test);
int setup_predefined (test_param_t *tests_arr, int count);
int print_usage (void);
int print_test_params (test_param_t *test);

int main (int argc, char **argv) {
    int rank;
    int run_test_number = 0;
    int failed;
    int while_condition;
    int i;

    test_param_t predefined_tests[PREDEF_TESTS];

    MPI_Init (&argc, &argv);
    MPI_Comm_rank (MPI_COMM_WORLD, &rank);
    
    if (argc != 1) {
	if (!rank) {
	    printf ("Use only one process\n");
	    print_usage ();
	}
	MPI_Finalize();
	return 1;
    }
    i = 1;
    while (i < argc) {
        if (!strcmp (argv[i], "-A")) {
            run_test_number = 0;
            i++;
        }
        else if (!strcmp (argv[i], "-T")) {
            run_test_number = atoi (argv[i+1]);
	    if ((run_test_number > PREDEF_TESTS) || (run_test_number < 1)) {
		if (!rank)
		    printf ("Invalid test number, only %d tests\n",
			    PREDEF_TESTS);
		MPI_Finalize ();
		return 1;
	    }
            i += 2;
        }
        else {
	    if (!rank) {
		printf ("Invalid Argument: %s\n", argv[i]);
		print_usage ();
	    }
            i++;
        }
    }

    setup_predefined (predefined_tests, PREDEF_TESTS);

    if (!run_test_number) {
	i = 0;
	while_condition = PREDEF_TESTS;
    }
    else {
	i = run_test_number - 1;
	while_condition = run_test_number;
    }
    while (i < while_condition) {
	printf ("***** Test %d *****\n", i+1);
	failed = run_test (&predefined_tests[i]);
	printf ("******************\n");
	i++;
    }

    MPI_Finalize ();

    return 0;
}

int run_test (test_param_t *test) {
    ADIO_Offset st_offset, end_offset;
    MPI_File fh;
    int is_contig;
    int ind_err = 0, exp_err = 0;

    MPI_Datatype filetype;

    MPI_Type_struct (test->type_count, test->type_blocklens,
		     test->type_indices, test->type_oldtypes, &filetype);
    MPI_Type_commit (&filetype);

    MPI_File_open (MPI_COMM_WORLD, "test_file.txt" , MPI_MODE_RDWR,
		   MPI_INFO_NULL, &fh);

    MPI_File_set_view (fh, 0, MPI_BYTE, filetype, "native", MPI_INFO_NULL);

    MPI_File_seek (fh, test->offset, MPI_SEEK_SET);
    ADIOI_Calc_bounds ((ADIO_File) fh, test->count, MPI_BYTE, ADIO_INDIVIDUAL,
		       test->offset, &st_offset, &end_offset);    

    ind_err = 0;
    if (st_offset != test->correct_st_offset) {
	printf ("Individual st_offset = %lld end_offset = %lld\n",
		st_offset, end_offset);
	ind_err = 1;
    }
    if (end_offset != test->correct_end_offset) {
	printf ("Individual st_offset = %lld end_offset = %lld\n",
		st_offset, end_offset);
	ind_err = 1;
    }
    MPI_File_close (&fh);
    if (ind_err)
	printf ("Individual Calc FAILED\n");

    MPI_File_open (MPI_COMM_WORLD, "test_file.txt" , MPI_MODE_RDWR,
		   MPI_INFO_NULL, &fh);

    if (!is_contig)
	MPI_File_set_view (fh, 0, MPI_BYTE, filetype, "native", MPI_INFO_NULL);

    MPI_File_seek (fh, 0, MPI_SEEK_SET);
    ADIOI_Calc_bounds ((ADIO_File) fh, test->count, MPI_BYTE,
		       ADIO_EXPLICIT_OFFSET, test->offset, &st_offset,
		       &end_offset);

    exp_err = 0;
    if (st_offset != test->correct_st_offset) {
	printf ("Explicit   st_offset = %lld end_offset = %lld\n",
		st_offset, end_offset);
	exp_err = 1;
    }
    if (end_offset != test->correct_end_offset) {
	printf ("Explicit   st_offset = %lld end_offset = %lld\n",
		st_offset, end_offset);
	exp_err = 1;
    }
    if (exp_err)
	printf ("Explicit Calc FAILED\n");

    MPI_File_close (&fh);

    if (!is_contig)
	MPI_Type_free (&filetype);

    return (exp_err || ind_err);
}

int print_usage ()
{
    printf (
	"Usage:\n"
	"   io_bounds_test -A -T <test #>\n");
}

int print_test_params (test_param_t *test) 
{
    int i;
    printf (
	"I/O offset:     %lld\n"
	"bytes:          %d\n"
	"Filetype [n](disp, lens, type):\n",
	test->offset, test->count);
    
    for (i=0; i<test->type_count; i++) {
	printf (
	    "    [%d](%lld, %d, ",
	    i,
	    test->type_blocklens[i],
	    test->type_indices[i]);
	if (test->type_oldtypes[i] == MPI_BYTE) {
	    printf ( "%s)\n", "MPI_BYTE");
	}
	else if (test->type_oldtypes[i] == MPI_UB) {
	    printf ( "%s)\n", "MPI_UB");
	}
	else if (test->type_oldtypes[i] == MPI_LB) {
	    printf ( "%s)\n", "MPI_LB");
	}
    }
    printf (
	"Expected Start offset:  %lld\n"
	"Expected End offset:    %lld\n",
	test->correct_st_offset,
	test->correct_end_offset);
}

int setup_predefined (test_param_t *tests_arr, int count)
{
    int i;
    for (i=0; i < PREDEF_TESTS; i++) {
	switch (i)
	    {
	    case 0:
		tests_arr[i].offset            = 0;
		tests_arr[i].count             = 0;
		tests_arr[i].type_count        = 0;
		tests_arr[i].type_indices[0]   = 0;
		tests_arr[i].type_blocklens[0] = 0;
		tests_arr[i].type_oldtypes[0]  = MPI_BYTE;
		tests_arr[i].type_indices[1]   = 0;
		tests_arr[i].type_blocklens[1] = 0;
		tests_arr[i].type_oldtypes[1]  = MPI_BYTE;
		tests_arr[i].type_indices[2]   = 0;
		tests_arr[i].type_blocklens[2] = 0;
		tests_arr[i].type_oldtypes[2]  = MPI_BYTE;
		tests_arr[i].type_indices[3]   = 0;
		tests_arr[i].type_blocklens[3] = 0;
		tests_arr[i].type_oldtypes[3]  = MPI_BYTE;
		break;
	    case 1:
		tests_arr[i].offset            = 0;
		tests_arr[i].count             = 0;
		tests_arr[i].type_count        = 0;
		tests_arr[i].type_indices[0]   = 0;
		tests_arr[i].type_blocklens[0] = 0;
		tests_arr[i].type_oldtypes[0]  = MPI_BYTE;
		tests_arr[i].type_indices[1]   = 0;
		tests_arr[i].type_blocklens[1] = 0;
		tests_arr[i].type_oldtypes[1]  = MPI_BYTE;
		tests_arr[i].type_indices[2]   = 0;
		tests_arr[i].type_blocklens[2] = 0;
		tests_arr[i].type_oldtypes[2]  = MPI_BYTE;
		tests_arr[i].type_indices[3]   = 0;
		tests_arr[i].type_blocklens[3] = 0;
		tests_arr[i].type_oldtypes[3]  = MPI_BYTE;
		break;
	    case 2:
		tests_arr[i].offset            = 0;
		tests_arr[i].count             = 0;
		tests_arr[i].type_count        = 0;
		tests_arr[i].type_indices[0]   = 0;
		tests_arr[i].type_blocklens[0] = 0;
		tests_arr[i].type_oldtypes[0]  = MPI_BYTE;
		tests_arr[i].type_indices[1]   = 0;
		tests_arr[i].type_blocklens[1] = 0;
		tests_arr[i].type_oldtypes[1]  = MPI_BYTE;
		tests_arr[i].type_indices[2]   = 0;
		tests_arr[i].type_blocklens[2] = 0;
		tests_arr[i].type_oldtypes[2]  = MPI_BYTE;
		tests_arr[i].type_indices[3]   = 0;
		tests_arr[i].type_blocklens[3] = 0;
		tests_arr[i].type_oldtypes[3]  = MPI_BYTE;
		break;
	    case 3:
		tests_arr[i].offset            = 0;
		tests_arr[i].count             = 0;
		tests_arr[i].type_count        = 0;
		tests_arr[i].type_indices[0]   = 0;
		tests_arr[i].type_blocklens[0] = 0;
		tests_arr[i].type_oldtypes[0]  = MPI_BYTE;
		tests_arr[i].type_indices[1]   = 0;
		tests_arr[i].type_blocklens[1] = 0;
		tests_arr[i].type_oldtypes[1]  = MPI_BYTE;
		tests_arr[i].type_indices[2]   = 0;
		tests_arr[i].type_blocklens[2] = 0;
		tests_arr[i].type_oldtypes[2]  = MPI_BYTE;
		tests_arr[i].type_indices[3]   = 0;
		tests_arr[i].type_blocklens[3] = 0;
		tests_arr[i].type_oldtypes[3]  = MPI_BYTE;
		break;
	    case 4:
		tests_arr[i].offset            = 0;
		tests_arr[i].count             = 0;
		tests_arr[i].type_count        = 0;
		tests_arr[i].type_indices[0]   = 0;
		tests_arr[i].type_blocklens[0] = 0;
		tests_arr[i].type_oldtypes[0]  = MPI_BYTE;
		tests_arr[i].type_indices[1]   = 0;
		tests_arr[i].type_blocklens[1] = 0;
		tests_arr[i].type_oldtypes[1]  = MPI_BYTE;
		tests_arr[i].type_indices[2]   = 0;
		tests_arr[i].type_blocklens[2] = 0;
		tests_arr[i].type_oldtypes[2]  = MPI_BYTE;
		tests_arr[i].type_indices[3]   = 0;
		tests_arr[i].type_blocklens[3] = 0;
		tests_arr[i].type_oldtypes[3]  = MPI_BYTE;
		break;
	    }
    }
    return 0;
}
