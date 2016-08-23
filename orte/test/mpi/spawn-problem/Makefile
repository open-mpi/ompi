CC=mpicc 

all: start ch_rec 


start:start.o
	$(CC) -o start start.o

ch_rec: ch_rec.o
	$(CC) -o ch_rec ch_rec.o

clean: 
	rm -f *.o ch_rec start 

run:
	mpirun -np 1 start
