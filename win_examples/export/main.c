#include "hello.h"
#include <stdio.h>

struct new_anju {
  hello_t *hell;
} ;
struct new_anju boo = {&anju};

int main() {

    boo.hell->i();
    boo.hell->j();
    return 0;
}
