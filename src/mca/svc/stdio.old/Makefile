#
# $HEADER$
#

SNL_OBJECTS = ./snipe_lite/msg.c \
	./snipe_lite/msgbuf2.c \
	./snipe_lite/memory.c \
	./snipe_lite/syslog.c \
   ./snipe_lite/snipe_lite.c 

all : sioapp cioapp

sioapp : sioapp.c
	gcc -o sioapp sioapp.c libsio.c $(SNL_OBJECTS) -I./snipe_lite

cioapp : cioapp.c
	gcc -o cioapp cioapp.c libcio.c $(SNL_OBJECTS) -I./snipe_lite

clean :
	rm -f *.o sioapp cioapp
