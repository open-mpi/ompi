PMIX_BASE = <pmix-path>
PMIX_INC= -I$(PMIX_BASE)/include/
PMIX_LIB= -L$(PMIX_BASE)/lib/ -lpmix

PMI2_BASE = /usr/
#PMI2_INC= -I$(PMI2_BASE)/include/
#PMI2_LIB= -L$(PMI2_BASE)/lib/ -lpmi2
PMI2_LIB= -lpmi2

CFLAGS = -O2 -g

all: pmix pmi2

pmix: pmi_intra_perf.c pmi.h pmix.c
	gcc $(PMIX_INC) $(CFLAGS) -o pmix_intra_perf pmi_intra_perf.c pmix.c $(PMIX_LIB) -lrt

pmi2: pmi_intra_perf.c pmi.h pmi2.c pmi2_pmap_parser.c pmi2_pmap_parser.h pmi2_utils.c pmi2_utils.h
	gcc $(PMI2_INC) $(CFLAGS) -o pmi2_intra_perf pmi_intra_perf.c pmi2.c pmi2_utils.c pmi2_pmap_parser.c -lrt $(PMI2_LIB)

clean:
	rm -f pmix_intra_perf pmi2_intra_perf
