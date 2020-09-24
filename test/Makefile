SHELL           = /bin/sh

INCFLAG = -I
INCPATH += $(INCFLAG).
FCFLAGS += -g

SRCPATH = ../src
HASHPATH = $(SRCPATH)/hash

# Make sure we have a log file
ifeq ($(LOGFILE),)
LOGFILE := hist_test.log
endif

# TARGETS

hist_hash_table.o: $(HASHPATH)/hist_hash_table.F90
	@echo "${FC} -c ${FCFLAGS} ${INCPATH} $^" 2>&1 >> $(LOGFILE)
	@${FC} -c ${FCFLAGS} ${INCPATH} $^ 2>&1 >> $(LOGFILE)

hist_hashable.o: $(HASHPATH)/hist_hashable.F90
	@echo "${FC} -c ${FCFLAGS} ${INCPATH} $^" 2>&1 >> $(LOGFILE)
	@${FC} -c ${FCFLAGS} ${INCPATH} $^ 2>&1 >> $(LOGFILE)

hist_field.o: $(SRCPATH)/hist_field.F90 hist_buffer.o hist_hashable.o
	@echo "${FC} -c ${FCFLAGS} ${INCPATH} $^" 2>&1 >> $(LOGFILE)
	@${FC} -c ${FCFLAGS} ${INCPATH} $^ 2>&1 >> $(LOGFILE)

hist_buffer.o: $(SRCPATH)/hist_buffer.F90 hist_hashable.o
	@echo "${FC} -c ${FCFLAGS} ${INCPATH} $^" 2>&1 >> $(LOGFILE)
	@${FC} -c ${FCFLAGS} ${INCPATH} $^ 2>&1 >> $(LOGFILE)

test_hash_table: test_hash.F90 hist_hashable.o hist_hash_table.o
	@echo "${FC} ${FCFLAGS} ${INCPATH} -o $@ $^" 2>&1 >> $(LOGFILE)
	@${FC} ${FCFLAGS} ${INCPATH} -o $@ $^ 2>&1 >> $(LOGFILE)

test_hist_buffer: test_hist_buffer.F90 hist_hashable.o hist_buffer.o hist_field.o
	@echo "${FC} ${FCFLAGS} ${INCPATH} -o $@ $^" 2>&1 >> $(LOGFILE)
	@${FC} ${FCFLAGS} ${INCPATH} -o $@ $^ 2>&1 >> $(LOGFILE)

# CLEAN
clean:
	@rm -f *.o *.mod
	@rm -f test_hash_table, test_hist_buffer
