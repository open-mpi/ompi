#include "mca/mem/base/mem_base_allocator.h"
#include <stdlib.h>
#include <stdio.h>

/* the multiple to allocate -- for testing purposes */
#define NUM_TIMES_ALLOC 5

void * allocate(size_t *);


void* allocate(size_t * size) {
    *size *= NUM_TIMES_ALLOC;
    return((void *)malloc(*size));
}

int main(int argc, char ** argv) {
    void * orig = (void *) 1;
    int choice;
    mca_mem_options_t * mem_options = mca_mem_init(30, allocate, free);
    printf("Address of mem_options_t: %p \n", (void *)mem_options);
    while(choice)
    {
        printf("allocate: 1 free: 2 cleanup: 3 exit: 0     ");
        scanf("%d", &choice);
        if(1 == choice) {
            printf("enter size: ");
            scanf("%d", &choice);
            orig = mca_mem_alloc(mem_options, choice);
            printf("the pointer of the allocated region: %p\n", orig);
        } else if (2 == choice) {
            printf("enter pointer to free: ");
            scanf("%p", &orig);
            mca_mem_free(mem_options, orig);
        } else if (3 == choice) {
            mca_mem_cleanup(mem_options);
        } else if (0 != choice) {
             printf("invalid option\n");
        }
    }
    return(0);
}
