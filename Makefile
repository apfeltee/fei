
INCFLAGS = -I.

withdyn ?= 1
withtypes ?= 0

CC = clang -Wall -Wextra
# ricing intensifies
#CFLAGS = $(INCFLAGS) -Ofast -march=native -flto -ffast-math -funroll-loops
CFLAGS = $(INCFLAGS) -O0 -g3 -ggdb3 -DUSE_DYNVALUE=$(withdyn) -DDEBUG_PRINTTYPES=$(withtypes)
LDFLAGS = -flto -ldl -lm  -lreadline -lpthread
#LDFLAGS = -lwinmm
target = run



src = \
	$(wildcard *.c) \

obj = $(src:.c=.o)
dep = $(obj:.o=.d)


$(target): $(obj)
	$(CC) -o $@ $^ $(LDFLAGS)

-include $(dep)

# rule to generate a dep file by using the C preprocessor
# (see man cpp for details on the -MM and -MT options)
%.d: %.c
	$(CC) $(CFLAGS) $< -MM -MT $(@:.d=.o) -MF $@

%.o: %.c
	$(CC) $(CFLAGS) -c $(DBGFLAGS) -o $@ $<

.PHONY: clean
clean:
	rm -f $(obj) $(target)

.PHONY: cleandep
cleandep:
	rm -f $(dep)

.PHONY: rebuild
rebuild: clean cleandep $(target)

.PHONY: sanity
sanity:
	./run sanity.msl
