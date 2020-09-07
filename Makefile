.PHONY : clean

out =
libj = /home/jrn/.guix-profile/bin/libj.so

build : juniper.so

juniper.so : juniper.sls code/*.scm
	scheme --script build.ss $(libj)

juniper-module.so : juniper-module.c
	gcc -shared -fpic -L/home/jrn/.guix-profile/bin -lj $< -o $@

install :
	mkdir -p $(out)
	cp juniper.so $(out)

clean :
	rm -rf *~ *.so *.o
