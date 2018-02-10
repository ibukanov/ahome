.PHONY: compile clean test

compile: build/localkit

build/localkit: main.c build/crypt_blowfish/ready
	gcc -Wall -Wextra -Werror -g -o $@ main.c build/crypt_blowfish/*.o

build/crypt_blowfish/ready: $(wildcard /vendor/crypt_blowfish/*) | build/crypt_blowfish
	cp -s -f $(PWD)/vendor/crypt_blowfish/* build/crypt_blowfish && \
	$(MAKE) -C build/crypt_blowfish && \
	touch $@

build/crypt_blowfish:
	mkdir -p $@

clean:
	rm -rf build

test: compile
	./test.sh
