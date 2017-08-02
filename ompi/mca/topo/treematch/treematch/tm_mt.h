void init_genrand(unsigned long s);
void init_by_array(unsigned long init_key[], int key_length);

/* generates a random number on the interval [0,0x7fffffff] */
unsigned long genrand_int32(void);

long genrand_int31(void);
double genrand_real1(void);
double genrand_real2(void);
double genrand_real3(void);
double genrand_res53(void);
