#define NONE     0
#define CRITICAL 1
#define ERROR    2
#define WARNING  3
#define INFO     4
#define DEBUG    5

void         set_verbose_level(unsigned int level);
unsigned int get_verbose_level(void);


