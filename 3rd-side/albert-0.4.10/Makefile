CLEANFILES = *.x86f *.ufsl *.fsl *.lib *.err *.fasl *~ *.fas

all:
	cd expat && $(MAKE) all

clean:
	-cd apispec && $(RM) $(CLEANFILES)
	-cd base && $(RM) $(CLEANFILES)
	-cd lisp2csf && $(RM) $(CLEANFILES)
	-cd specs && $(RM) $(CLEANFILES)
	-cd spres && $(RM) $(CLEANFILES)
	-cd spres/rules && $(RM) $(CLEANFILES)
	-cd tools && $(RM) $(CLEANFILES)
	-cd data && $(RM) $(CLEANFILES)
	-cd expat && $(MAKE) clean
	-$(RM) $(CLEANFILES) configure-stamp build-stamp
	-rm -rf debian/albert*
