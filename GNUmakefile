jinx-mod-x86_64.dylib: jinx-mod.c
	gcc -I. -O2 -Wall -Wextra -fPIC -shared -o $@ $< $$(pkg-config --cflags --libs enchant-2)
