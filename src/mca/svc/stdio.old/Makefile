#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
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
