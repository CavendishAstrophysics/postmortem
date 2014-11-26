C  Main program for the EXAMINE-SAMP utility
C
        character*80 parameters
	integer length,status
	status=0
	call io_initio
        call io_enqcli(parameters,length)
	call ex_samp(parameters(1:length),status)
	end
