CFLAGS=-g -Iinclude -Wall
vpath %.c src

cpu: common.c phivm.c
	$(CC) $(CFLAGS) -o cpu $^

.PHONY : clean

clean:
	-rm -rf a.out* *.o
