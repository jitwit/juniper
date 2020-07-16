.PHONY : clean

out =
libj = 

build : juniper.so

juniper.so : juniper.sls code/*.scm
	scheme --script build.ss $(libj)

install :
	mkdir -p $(out)
	cp juniper.so $(out)

clean :
	rm -rf *~ *.so
