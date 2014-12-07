CFLAGS=-g -Iinclude -Wall # -DPHIVM_DEBUG_DRAW -DPHIVM_DEBUG_CPU
vpath %.c src

cpu: common.c phivm.c draw.c phistack.c draw.c bmpio.c
	$(CC) $(CFLAGS) -o cpu $^

.PHONY : clean

clean:
	-rm -rf a.out* *.o
