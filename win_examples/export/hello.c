#include "hello.h"
#include <stdio.h>

int hello(void);
int hell(void);

hello_t anju = {hello, hell};
bool aaa = true;

static int hello(void) {
    return printf("Hello World\n");
}
static int hell(void) {
    return printf("Hell with you World\n");
}
