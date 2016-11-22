#include "tm_verbose.h"
static unsigned int verbose_level = ERROR;

void set_verbose_level(unsigned int level){
  verbose_level = level;
}


unsigned int get_verbose_level(){
  return verbose_level;
}
