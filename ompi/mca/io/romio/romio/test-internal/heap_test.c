#include "../adio/include/heap-sort.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define PREDEF_TESTS 2
/* test types */
#define ALL     0
#define RANDOM -1
#define CUSTOM -2

/* ACTIONS */
#define BUILD          0
#define INSERT         1
#define EXTRACT        2
#define EXTRACT_INSERT 3

typedef struct {
    char name[64];
    int heap_size;
    int print;
    int verify;
    int action_arr_sz;
    int *action_arr;
    int *action_count_arr;
    ADIO_Offset *offsets;
    ADIO_Offset *correct_order;
} test_params_t;

void print_usage();
void print_keys(ADIO_Offset* offsets, int size);
void print_params(test_params_t *params);
int run_test(test_params_t *test);
void fill_random_test(test_params_t *params);
void init_predefined_test(test_params_t *params, int index);
void dumb_sort(test_params_t *params);

int main(int argc, char **argv) {
    int i, print = 1, verify = 1;
    int adding_elements;
    int curr_add_idx;
    int test_type = RANDOM;
    test_params_t predefined_tests[PREDEF_TESTS];
    test_params_t test;
    
    /* parse args */
    adding_elements = 0;
    curr_add_idx = 0;
    if (argc == 1) {
	print_usage();
	return 1;
    }
    i = 1;
    while (i < argc) {
	if (!strcmp("-A", argv[i])) {
	    adding_elements = 0;
	    test_type = ALL;
	    i++;
	}
	else if (!strcmp("-T", argv[i])) {
	    adding_elements = 0;
	    test_type = atoi(argv[i+1]);
	    i += 2;
	}
	else if (!strcmp("-r", argv[i])) {
	    adding_elements = 0;
	    test.heap_size = atoi(argv[i+1]);
	    if (test.heap_size <= 0) {
		printf("heap size should be a positive integer\n");
		return 1;
	    }
	    test.offsets = (ADIO_Offset *) malloc(test.heap_size*sizeof(ADIO_Offset));
	    test_type = RANDOM;
	    i += 2;
	}
	else if (!strcmp("-e", argv[i])) {
	    test.heap_size = argc - 2;
	    if (test.heap_size <= 0) {
		printf("need at least one key\n");
		return 1;
	    }
	    test.offsets = (ADIO_Offset *) malloc(test.heap_size*sizeof(ADIO_Offset));
	    adding_elements = 1;
	    test_type = CUSTOM;
	    i++;
	}
	else if (!strcmp("-v", argv[i])) {
	    verify = 1;
	    i++;
	}
	else if (!strcmp("-p", argv[i])) {
	    print = 1;
	    i++;
	}
	else if (!strcmp("-V", argv[i])) {
	    verify = 0;
	    i++;
	}
	else if (!strcmp("-P", argv[i])) {
	    print = 0;
	    i++;
	}
	else if (adding_elements) {
	    test.offsets[curr_add_idx] = atoi(argv[i]);
	    curr_add_idx++;
	    i++;
	}
	else {
	    printf("Illegal argument: %s", argv[i]);
	    print_usage();
	    return 1;
	}
    }

    if (test_type == RANDOM) {
	fill_random_test(&test);
	strcpy(test.name, "RANDOMIZED TEST");
    }
    else if (test_type == CUSTOM)
	strcpy(test.name, "CUSTOM TEST");
    if ((test_type == CUSTOM) || (test_type == RANDOM)) {
	test.print = print;
	test.verify = verify;
	test.action_arr_sz = 2;
	test.action_arr = (int *) malloc(test.action_arr_sz*sizeof(int));
	test.action_count_arr = (int *) malloc(test.action_arr_sz*sizeof(int));
	/* build the entire heap */
	/* test.action_arr[0]       = BUILD;
	   test.action_count_arr[0] = 1; */
	/* insert keys one at a time */
	test.action_arr[0]       = INSERT;
	test.action_count_arr[0] = test.heap_size;
	/* extract all the keys */
	test.action_arr[1]       = EXTRACT;
	test.action_count_arr[1] = test.heap_size;

	if (verify) {
	    test.correct_order = (ADIO_Offset *)malloc(test.heap_size*sizeof(ADIO_Offset));
	    dumb_sort(&test);
	}
	if (print)
	    print_params(&test);
	run_test(&test);
    }
    else {
	if (test_type == ALL) {
	    for (i=0; i<PREDEF_TESTS; i++) {
		predefined_tests[i].print = print;
		predefined_tests[i].verify = verify;
		init_predefined_test(&predefined_tests[i], i);
		if (print)
		    print_params(&test);
		run_test(&predefined_tests[i]);
	    }
	}
	else {
	    predefined_tests[test_type-1].print = print;
	    predefined_tests[test_type-1].verify = verify;
	    init_predefined_test(&predefined_tests[test_type-1], test_type-1);
	    if (print)
		print_params(&predefined_tests[test_type-1]);
	    run_test(&predefined_tests[test_type-1]);
	}
    }

    return 0;
}

void print_usage() {
    printf(
	"Usage: test <options>\n"
	"   -r <size>  Create a random test and verify of size <size>\n"
	"   -e <keys>  test with the space delimited list of keys\n"
	"   -p         print parameters and keys (default)\n"
	"   -P         do not print parameters and keys\n"
	"   -v         verify keys (default)\n"
	"   -V         do not verify keys\n"
	);
}

void print_keys(ADIO_Offset *offsets, int size) {
    int i;
    for (i=0; i < size; i++)
	printf("%lld ", offsets[i]);
}

void print_params(test_params_t *params) {
    int i;
    static char action_map[3][8] = {"BUILD", "INSERT", "EXTRACT"};

    printf("----------------Test Parameters---------------\n");
    printf("Actions:\n");
    for (i=0; i<params->action_arr_sz; i++) {
	printf("%sx%d\n", action_map[params->action_arr[i]],
		params->action_count_arr[i]);
    }

    printf("Initial order :\n");
    print_keys(params->offsets, params->heap_size);
    printf("\n");

    if (params->verify) {
	printf("Expected order:\n");
	print_keys(params->correct_order, params->heap_size);
	printf("\n");
    }
    printf("----------------------------------------------\n");
}

void fill_random_test(test_params_t *params) {
    int i;
    int max_key;
    time_t seed;
    int order = 0;

    time(&seed);
    srand(seed);

    order = 0;
    max_key = 1;
    while (order < 25) {
	max_key *= 10;
	if (!((int) (params->heap_size / max_key)))
	    break;
	order++;
    }
    for (i=0; i < params->heap_size; i++)
	params->offsets[i] = (rand() % max_key);
}

void dumb_sort(test_params_t *params) {
    ADIO_Offset *offsets, tmp_offset;
    int i, j;

    offsets = params->correct_order;
    memcpy(offsets, params->offsets, params->heap_size*sizeof(ADIO_Offset));
    for (i=0; i < params->heap_size; i++) {
	for (j=i; j < params->heap_size; j++) {
	    if (offsets[j] < offsets[i]) {
		tmp_offset = offsets[i];
		offsets[i] = offsets[j];
		offsets[j] = tmp_offset;
	    }
	}
    }
}

int run_test(test_params_t *test) {
    heap_t myheap;
    ADIO_Offset *extracted;
    int stored_proc;
    ADIO_Offset stored_reg_max_len;
    int i, j, k, err_flag = 0;
    int curr_insert_idx  = 0;
    int curr_extract_idx = 0;
    
    create_heap(&myheap, test->heap_size);
    myheap.size = 0;
    
    extracted = (ADIO_Offset *) malloc(test->heap_size * sizeof(ADIO_Offset));
    for (i=0; i < test->action_arr_sz; i++) {
	for (j=0; j<test->action_count_arr[i]; j++) {
	    switch (test->action_arr[i])
	    {
		case BUILD:
		    myheap.size = test->heap_size;
		    for (k=0; k < test->heap_size; k++) {
			myheap.nodes[k].offset   = test->offsets[k];
			myheap.nodes[k].proc = k;
		    }
		    build_heap(&myheap);		    
		    break;
		case INSERT:
		    ADIOI_Heap_insert(&myheap, test->offsets[curr_insert_idx],
				curr_insert_idx, curr_insert_idx);
		    curr_insert_idx++;
		    break;
		case EXTRACT:
		    heap_extract_min(&myheap, &extracted[curr_extract_idx],
				     &stored_proc, &stored_reg_max_len);
		    if (test->verify && (extracted[curr_extract_idx] !=
			test->correct_order[curr_extract_idx]))
			err_flag++;
		    curr_extract_idx++;
		    break;
		case EXTRACT_INSERT:
		    heap_extract_min(&myheap, &extracted[curr_extract_idx],
				     &stored_proc, &stored_reg_max_len);
		    if (test->verify &&(extracted[curr_extract_idx] !=
					test->correct_order[curr_extract_idx]))
			err_flag++;
		    curr_extract_idx++;

		    ADIOI_Heap_insert(&myheap, test->offsets[curr_insert_idx],
				curr_insert_idx, curr_insert_idx);
		    curr_insert_idx++;
		    break;
		default:
		    break;
	    }
	}
    }

    if (test->verify) {
	if (err_flag) {
	    printf("***%s FAILED***\n", test->name);
	    if (test->print) {
		printf("Min extraction:\n");
		print_keys(extracted, test->heap_size);
		printf("\n");
	    }
	}
	else
	    printf("***%s PASSED***\n", test->name);
    }

    free_heap(&myheap);
    free(extracted);
    /* clean up test params */
    free(test->offsets);
    if (test->verify)
	free(test->correct_order);
    free(test->action_arr);
    free(test->action_count_arr);

    return err_flag;
}

void init_predefined_test(test_params_t *params, int index) {

    switch (index)
    {
	case 0:
	    strcpy(params->name, "TEST 1");
	    params->heap_size = 15;
	    params->action_arr_sz = 3;
	    
	    /* allocate space */
	    params->action_arr =
		(int *) malloc (params->action_arr_sz*sizeof(int));
	    params->action_count_arr =
		(int *) malloc (params->action_arr_sz*sizeof(int));
	    params->offsets = (ADIO_Offset *) malloc(params->heap_size*sizeof(ADIO_Offset));
	    if (params->verify)
		params->correct_order =
		    (ADIO_Offset *) malloc(params->heap_size*sizeof(ADIO_Offset));

	    /* Set procs */
	    params->offsets[0]  = 65;
	    params->offsets[1]  = 53;
	    params->offsets[2]  = 51;
	    params->offsets[3]  = 74;
	    params->offsets[4]  = 1;
	    params->offsets[5]  = 3;
	    params->offsets[6]  = 86;
	    params->offsets[7]  = 82;
	    params->offsets[8]  = 42;
	    params->offsets[9]  = 62;
	    params->offsets[10] = 33;
	    params->offsets[11] = 12;
	    params->offsets[12] = 79;
	    params->offsets[13] = 13;
	    params->offsets[14] = 28;

	    if (params->verify) {
		params->correct_order[0]  = 1;
		params->correct_order[1]  = 3;
		params->correct_order[2]  = 12;
		params->correct_order[3]  = 33;
		params->correct_order[4]  = 13;
		params->correct_order[5]  = 28;
		params->correct_order[6]  = 42;
		params->correct_order[7]  = 51;
		params->correct_order[8]  = 53;
		params->correct_order[9]  = 62;
		params->correct_order[10] = 65;
		params->correct_order[11] = 74;
		params->correct_order[12] = 79;
		params->correct_order[13] = 82;
		params->correct_order[14] = 86;
	    }

	    params->action_arr[0]  = INSERT;
	    params->action_arr[1]  = EXTRACT_INSERT;
	    params->action_arr[11] = EXTRACT;

	    params->action_count_arr[0]  = 10;
	    params->action_count_arr[1]  = 5;
	    params->action_count_arr[11] = 10;
	    break;
	case 1:
	    strcpy(params->name, "TEST 1");
	    params->heap_size = 15;
	    params->action_arr_sz = 3;
	    
	    /* allocate space */
	    params->action_arr =
		(int *) malloc (params->action_arr_sz*sizeof(int));
	    params->action_count_arr =
		(int *) malloc (params->action_arr_sz*sizeof(int));
	    params->offsets = (ADIO_Offset *) malloc(params->heap_size*sizeof(ADIO_Offset));
	    if (params->verify)
		params->correct_order =
		    (ADIO_Offset *) malloc(params->heap_size*sizeof(ADIO_Offset));
	    
	    /* Set values */
	    params->offsets[0]  = 65;
	    params->offsets[1]  = 53;
	    params->offsets[2]  = 51;
	    params->offsets[3]  = 74;
	    params->offsets[4]  = 1;
	    params->offsets[5]  = 3;
	    params->offsets[6]  = 86;
	    params->offsets[7]  = 82;
	    params->offsets[8]  = 42;
	    params->offsets[9]  = 62;
	    params->offsets[10] = 33;
	    params->offsets[11] = 12;
	    params->offsets[12] = 79;
	    params->offsets[13] = 13;
	    params->offsets[14] = 28;

	    if (params->verify) {
		params->correct_order[0]  = 1;
		params->correct_order[1]  = 3;
		params->correct_order[2]  = 12;
		params->correct_order[3]  = 33;
		params->correct_order[4]  = 13;
		params->correct_order[5]  = 28;
		params->correct_order[6]  = 42;
		params->correct_order[7]  = 51;
		params->correct_order[8]  = 53;
		params->correct_order[9]  = 62;
		params->correct_order[10] = 65;
		params->correct_order[11] = 74;
		params->correct_order[12] = 79;
		params->correct_order[13] = 82;
		params->correct_order[14] = 86;
	    }

	    params->action_arr[0]  = INSERT;
	    params->action_arr[1]  = EXTRACT_INSERT;
	    params->action_arr[11] = EXTRACT;

	    params->action_count_arr[0]  = 10;
	    params->action_count_arr[1]  = 5;
	    params->action_count_arr[11] = 10;
	    break;
	default:
	    break;
    }
}
