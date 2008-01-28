include Makefile.config

TARGETS = $(HELLO_TGT1) $(HELLO_TGT2) $(HELLO_TGT3) \
          $(PI_TGT1) $(PI_TGT2) $(PI_TGT3) \
          $(RING_TGT1) $(RING_TGT2) $(RING_TGT3)

.PHONY: all
all: $(TARGETS)


### hello-man ###
hello-man: hello.c
	$(MPICC) -vt:inst manual $(MPICFLAGS) -DMANUAL $^ -o $@ -DVTRACE $(MPILIB)
### hello-comp ###
hello-comp: hello.c
	$(MPICC) $(MPICFLAGS) $^ -o $@ $(MPILIB)
### hello-dyn ###
hello-dyn: hello.c
	$(MPICC) -vt:inst dyninst $(MPICFLAGS) $^ -o $@ $(MPILIB)


### pi-man ###
pi-man: pi.c
	$(MPICC) -vt:inst manual $(MPICFLAGS) -DMANUAL $^ -o $@ -lm -DVTRACE $(MPILIB)
### pi-comp ###
pi-comp: pi.c
	$(MPICC) $(MPICFLAGS) $^ -o $@ -lm $(MPILIB)
### pi-dyn ###
pi-dyn: pi.c
	$(MPICC) -vt:inst dyninst $(MPICFLAGS) $^ -o $@ -lm $(MPILIB)


### ring-man ###
ring-man: ring.c
	$(MPICC) -vt:inst manual $(MPICFLAGS) -DMANUAL $^ -o $@ -DVTRACE $(MPILIB)
### ring-comp ###
ring-comp: ring.c
	$(MPICC) $(MPICFLAGS) $^ -o $@ $(MPILIB)
### ring-dyn ###
ring-dyn: ring.c
	$(MPICC) -vt:inst dyninst $(MPICFLAGS) $^ -o $@ $(MPILIB)


.PHONY: clean
clean:
	-rm -f $(TARGETS) *~ *.o
