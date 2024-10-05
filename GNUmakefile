ARCH := $(shell uname -m)

jinx-mod-$(ARCH).dylib: jinx-mod.c
	gcc -I. -O2 -Wall -Wextra -fPIC -shared -o $@ $< $$(pkg-config --cflags --libs enchant-2)
