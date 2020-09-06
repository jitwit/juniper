.PHONY : clean

out =
libj = /home/jrn/.guix-profile/bin/libj.so

build : juniper.so

juniper.so : juniper.sls code/*.scm
	scheme --script build.ss $(libj)

juniper.o : juniper.c juniper.h
	gcc -l $(libj) -c $< 

install :
	mkdir -p $(out)
	cp juniper.so $(out)

clean :
	rm -rf *~ *.so
