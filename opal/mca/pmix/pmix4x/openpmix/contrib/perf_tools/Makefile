PMIX_BASE = /tmp/artemp/pmix
SLURM_BASE = /tmp/artemp/slurm

PMIX_INC= -I$(PMIX_BASE)/include/
PMIX_LIB= -L$(PMIX_BASE)/lib/ -L$(PMIX_BASE)/lib64/ -lpmix

PMI2_BASE = $(SLURM_BASE)
PMI2_INC= -I$(PMI2_BASE)/include/
PMI2_LIB= -L$(PMI2_BASE)/lib/ -L$(PMI2_BASE)/lib64/ -lpmi2

PMI1_BASE = $(SLURM_BASE)
PMI1_INC= -I$(PMI1_BASE)/include/
PMI1_LIB= -L$(PMI1_BASE)/lib/ -L$(PMI1_BASE)/lib64/ -lpmi

CFLAGS = -O2 -g

all: pmix_intra_perf pmi2_intra_perf pmi1_intra_perf

pmix_intra_perf: pmi_intra_perf.c pmi.h pmix.c
	gcc $(PMIX_INC) $(CFLAGS) -o pmix_intra_perf pmi_intra_perf.c pmix.c $(PMIX_LIB) -lrt

pmi2_intra_perf: pmi_intra_perf.c pmi.h pmi2.c pmi2_pmap_parser.c pmi2_pmap_parser.h pmi2_utils.c pmi2_utils.h
	gcc $(PMI2_INC) $(CFLAGS) -o pmi2_intra_perf pmi_intra_perf.c pmi2.c pmi2_utils.c pmi2_pmap_parser.c -lrt $(PMI2_LIB)

pmi1_intra_perf: pmi_intra_perf.c pmi.h pmi1.c pmi2_utils.c pmi2_utils.h
	gcc $(PMI1_INC) $(CFLAGS) -o pmi1_intra_perf pmi_intra_perf.c pmi1.c pmi2_utils.c -lrt $(PMI1_LIB)

clean:
	rm -f pmix_intra_perf pmi2_intra_perf pmi1_intra_perf
