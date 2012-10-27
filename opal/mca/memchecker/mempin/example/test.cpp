#include <stdio.h>
#ifdef WIN32
#  include <windows.h>
#endif
#include "MemPin.h"


int read_count = 0, write_count = 0;

int n[5];

#define BIT_SET(a,b) ((a) |= (1<<(b)))
#define BIT_CLEAR(a,b) ((a) &= ~(1<<(b)))
#define BIT_FLIP(a,b) ((a) ^= (1<<(b)))
#define BIT_CHECK(a,b) ((a) & (1<<(b)))

#define BITS_PER_BYTE  8
#define SET_BIT(bitmap, pos)   (bitmap[(pos)/BITS_PER_BYTE] |=  (1<<((pos)%BITS_PER_BYTE)))
#define CLEAR_BIT(bitmap, pos) (bitmap[(pos)/BITS_PER_BYTE] &= ~(1<<((pos)%BITS_PER_BYTE)))
#define TEST_BIT(bitmap, pos)  (bitmap[(pos)/BITS_PER_BYTE] &   (1<<((pos)%BITS_PER_BYTE)))

int read_cb(void* addr, size_t size, int offset, int is_write, void* cb_info, void* ip)
{
    int int_size = size;
    printf("ip:%p is_write:%d size:%d offset:%d\n", ip, is_write, int_size, offset);
    if (is_write)
        write_count++;
    else
        read_count++;

    return MEMPIN_CALLBACK_PRINT_CALLSTACK_5;
}

void func3()
{
        int test;
        printf("read n: %p %d\n",  n, n[2]); //read
        test = n[2];                         //read
        test = n[0];                         //read
        n[4] = 19+test;                      //write
}
void func2()
{
        func3();
}

void func1()
{
        func2();
}

int main()
{
    int is_in_pin = 0;
    size_t index[10];
    unsigned int i;
    
    for(i=0; i < sizeof(n)/sizeof(n[0]); i++) {
        n[i]=i;
    }

    // check whether we are in Pin
    MEMPIN_RUNNING_WITH_PIN(&is_in_pin);
    if (is_in_pin) {
        printf("we are running with pin! \n");
    } else {
        printf("we are not running with pin! \n");
    }

    MEMPIN_REG_MEM_WATCH(n, 5*sizeof(int), MEMPIN_WATCH_RW, read_cb, NULL);

    func1();

    // example how to use MEMPIN_SEARCH_MEM_INDEX
    MEMPIN_SEARCH_MEM_INDEX(n, 5*sizeof(int), index);
    printf("index %ld, %ld\n", index[0], index[1]);

    //printf("index %0x: %d, %d \n", index, index[0], index[1]);
    MEMPIN_UNREG_MEM_WATCH(n, 5*sizeof(int));

    printf("memory reads %d time(s).\n", read_count);
    printf("memory writes %d time(s).\n", write_count);

    return 0;
}
