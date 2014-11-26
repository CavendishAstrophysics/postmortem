# Makefile for MRAO POSTMORTEM and related programs
#
# DJT, 9/7/92; GP/DJT ... 9 May 2000

FFLAGC=-u
FCOMPL=f77

BINDIR=/mrao/post/bin

.f.o:
	$(FCOMPL) -c $(FFLAGC) $<

INCLUDE_FILES=\
	iolib_common.inc iolib_constants.inc iolib_devices.inc\
	iolib_errors.inc iolib_functions.inc

LIBRARY_FILES=\
	calsys.a \
	ionsys.a \
	lsfsys.a \
	mapsys.a \
	remsys.a \
	/mrao/post/src/libmon.a \
	/mrao/post/src/liblsf.a \
	/mrao/post/src/libmon.a \
	/mrao/lib/libplanet.a \
	/mrao/post/src/libplot.a \
	/mrao/post/src/libsamp.a

OBJECT_FILES=\
	post_main.o block_data.o basic_commands.o \
	calib_system.o lsf_system.o lsfun1.o ion_system.o \
	monitor_system.o mon_sys_clfst.o mon_sys_ryle.o \
	mapping_system.o remove_system.o

# object for relinking everything

all : postmortem calibration mapper remove ion_correction

# object for linking postmortem

postmortem : $(OBJECT_FILES) $(LIBRARY_FILES)
	f77 -o postmortem \
	post_main.o basic_commands.o \
	calib_system.o lsf_system.o ion_system.o \
	monitor_system.o mon_sys_clfst.o mon_sys_ryle.o \
	mapping_system.o remove_system.o \
	calsys.a ionsys.a lsfsys.a lsfun1.o mapsys.a remsys.a \
	-L/mrao/post/src -L/mrao/lib -L/usr/local/lib -L/usr/openwin/lib \
	-lmon -llsf -lplot -lpg -lsamp -lplanet -lsla -lmap -lfft \
	-lcmd -lutil -lio -lch \
	`pgplotlink` -lnag
	/usr/bin/rm /mrao/post/post_date_link
	/usr/bin/date > /mrao/post/post_date_link
	mv postmortem /mrao/bin/post


# object for calibration sub-program

calibration : calib_program.o $(OBJECT_FILES) $(LIBRARY_FILES)
	f77 -o calibration \
	calib_program.o calsys.a \
	-L/mrao/post/src -L/mrao/lib -llsf -lsamp -lmap -lplanet -lsla -lnag \
	-lcmd -lutil -lio -lch
	mv calibration $(BINDIR)

# object for ionospheric correction sub-program

ion_correction : ion_program.o $(OBJECT_FILES) $(LIBRARY_FILES)
	f77 -o ion_correction \
	ion_program.o ionsys.a \
	-L/mrao/post/src -L/mrao/lib -llsf -lsamp -lmap -lplanet -lsla -lnag \
	-lfft -lcmd -lutil -lio -lch
	mv ion_correction $(BINDIR)

# object for mapper sub-program

mapper : mapping_program.o $(OBJECT_FILES) $(LIBRARY_FILES)
	f77 -o mapper \
	mapping_program.o mapsys.a \
	-L/mrao/post/src -L/mrao/lib -llsf -lsamp -lmap \
	-lmon -lplanet -lsla -lnag \
	-lfft -lcmd -lutil -lio -lch
	mv mapper $(BINDIR)/mapper

# object for remove sub-program

remove : remove_program.o $(OBJECT_FILES) $(LIBRARY_FILES)
	f77 -o remove \
	remove_program.o remsys.a \
	-L/mrao/post/src -L/mrao/lib -llsf -lsamp -lmap -lplanet -lsla -lnag \
	-lcmd -lutil -lio -lch
	mv remove $(BINDIR)

# object for exsamp utility program

exsamp : exsamp.o
	f77 -o exsamp exsamp.o \
	-L/mrao/post/src -L/mrao/lib -lsamp -lmap -lutil -lio -lch
	mv exsamp $(BINDIR)

