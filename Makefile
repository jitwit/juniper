.PHONY : clean

out =
j-bin = /home/jrn/.guix-profile/bin

build : juniper.so juniper-module.so

juniper.so : juniper.sls code/*.scm
	scheme --script build.ss $(j-bin)/libj.so

juniper-module.so : juniper-module.c
	gcc -Wall -ggdb3 -shared -fpic -L$(j-bin) -lj $< -o $@

install :
	mkdir -p $(out)
	cp juniper.so $(out)

clean :
	rm -rf *~ *.so *.o juniper.txt
