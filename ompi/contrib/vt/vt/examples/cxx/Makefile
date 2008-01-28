include Makefile.config

TARGETS = $(HELLO_TGT1) $(HELLO_TGT2) $(HELLO_TGT3) \
          $(PI_TGT1) $(PI_TGT2) $(PI_TGT3) \
          $(RING_TGT1) $(RING_TGT2) $(RING_TGT3)

.PHONY: all
all: $(TARGETS)


### hello-man ###
hello-man: hello.cc
	$(MPICXX) -vt:inst manual $(MPICXXFLAGS) -DMANUAL $^ -o $@ -DVTRACE $(MPILIB)
### hello-comp ###
hello-comp: hello.cc
	$(MPICXX) $(MPICXXFLAGS) $^ -o $@ $(MPILIB)
### hello-dyn ###
hello-dyn: hello.cc
	$(MPICXX) -vt:inst dyninst $(MPICXXFLAGS) $^ -o $@ $(MPILIB)


### pi-man ###
pi-man: pi.cc
	$(MPICXX) -vt:inst manual $(MPICXXFLAGS) -DMANUAL $^ -o $@ -lm $(MPILIB)
### pi-comp ###
pi-comp: pi.cc
	$(MPICXX) $(MPICXXFLAGS) $^ -o $@ -lm $(MPILIB)
### pi-dyn ###
pi-dyn: pi.cc
	$(MPICXX) -vt:inst dyninst $(MPICXXFLAGS) $^ -o $@ -lm $(MPILIB)


### ring-man ###
ring-man: ring.cc
	$(MPICXX) -vt:inst manual $(MPICXXFLAGS) -DMANUAL $^ -o $@ -DVTRACE $(MPILIB)
### ring-comp ###
ring-comp: ring.cc
	$(MPICXX) $(MPICXXFLAGS) $^ -o $@ $(MPILIB)
### ring-dyn ###
ring-dyn: ring.cc
	$(MPICXX) -vt:inst dyninst $(MPICXXFLAGS) $^ -o $@ $(MPILIB)


.PHONY: clean
clean:
	-rm -f $(TARGETS) *~ *.o
