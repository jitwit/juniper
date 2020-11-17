.PHONY : clean

out =
j-bin = /home/jrn/.guix-profile/bin

build : juniper.so

juniper.so : juniper.sls code/*.scm
	scheme --script build.ss $(j-bin)/libj.so

install :
	mkdir -p $(out)
	cp juniper.so $(out)

clean :
	rm -rf *~ *.so *.o juniper.txt
