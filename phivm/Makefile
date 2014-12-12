CFLAGS=-g -Iinclude -Wall -O3 -DPHIVM_DEBUG_CPU # -DPHIVM_DEBUG_DRAW
vpath %.c src

phivm: common.c phivm.c draw.c phistack.c draw.c bmpio.c phimem.c
	$(CC) $(CFLAGS) -o $@ $^

.PHONY : clean

clean:
	-rm -rf a.out* *.o
