all:ulong test-find-objects
ulong:ulong.c
	gcc $(DEBUG_FLAGS) -I. -L. -o $@ $<
run::ulong
	@ ./ulong && echo status = $$?
clean::
	-rm -f ulong test-find-objects
	-rm -f core
	-rm -f *.lx64fsl *.dx64fsl *.o

DEBUG_FLAGS = -O0 -g3 -ggdb3 -g -ggdb -gdwarf
INCLUDES    = -I$(HOME)/opt/include -I/usr/local/include -I. -I/usr/include/pkcs11-helper-1.0
LIBDIRS     = -L$(HOME)/opt/lib     -L/usr/local/lib     -L.
LIBRARIES   = -ldl
LDFLAGS    += $(DEBUG_FLAGS) $(LIBDIRS)
CFLAGS     += $(DEBUG_FLAGS) $(INCLUDES)
test-find-objects.o:test-find-objects.c
	gcc $(DEBUG_FLAGS) $(CFLAGS)  -c -o test-find-objects.o test-find-objects.c
test-find-objects:test-find-objects.o
	gcc $(DEBUG_FLAGS) $(LDFLAGS)    -o test-find-objects test-find-objects.o $(LIBRARIES)
run::test-find-objects
	gdb test-find-objects

TEST_CERT_DIR=%TEST_CERT_DIR%
TESTCERT=test-cert
TESTKEY=test-key
certificate:
	openssl req -config $(TESTCERT).options -new \
		-x509 -sha256 -newkey rsa:2048 -nodes \
		-keyout $(TESTKEY).pem -out $(TESTCERT).pem -days 3650
	 openssl x509 -in $(TESTCERT).pem -text -noout

	@echo '# to install certificate use:'
	@echo cp "$(TESTCERT).pem"                  "$(TEST_CERT_DIR)/Test_x509_cert_info.pem"
	@echo cp Certificat-d-authentification1.pem "$(TEST_CERT_DIR)/Test_x509_cert_info.pem"
