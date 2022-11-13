CC=gcc
CFLAGS=-Wall -w -pedantic -ansi -std=c11
TARGET=dut

hare:

c:
	${CC} ${CC_OPTIONS}	main.c -o ${TARGET}
