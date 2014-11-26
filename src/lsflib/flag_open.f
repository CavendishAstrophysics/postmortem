*+ flag_open

       subroutine flag_open( flag_file, flag_id, status )
C      --------------------------------------------------
C
C Open a flag file
C
C Given:
C   file name of flag file
       character*(*)   flag_file
C Returned:
C   flag file identifier
       integer         flag_id
C Updated:
C   error return
       integer         status
C
C A flag table file of the specified name is opened.  More than one
C flag table may be open at any one time, but it is the responsibility
C of the application to manage the opening and closing of the files.
C FLAG_ID, the flag-file identifier, is used in all subsequent requests
C to access the file (it is in fact the unit number of the open flag-table
C file).
C
C [PA, 12/8/91] [save_sf removed 8 Feb 93  GP]
C-

       include '/mrao/post/include/flag_definition.inc'
       include '/mrao/post/include/flag_errors.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_errors.inc'
c      include '/mrao/include/cmd_functions.inc'

C local variables
       integer   iunit
       logical   exists

C check status
       if (status.ne.0 .and. status.ne.no_file) return

C check for the existence of the flag file
       if (status.eq.no_file) then
         status = 0
         exists = .false.
       else
         inquire (file=flag_file(1:chr_lenb(flag_file)), exist=exists)
       end if

C create the file if it does not exist
       if (.not.exists) call flag_create( flag_file, status )
       if (status.ne.0) goto 999

C open file
       call io_nxtlun( iunit, status )
       flag_id = iunit
       if (flag_id .gt. flg_max_unit) then
          status = ill_flgunit
          goto 999
       endif
       flg_list_open(iunit) = .true.
       flg_list_name(iunit) = flag_file(1:chr_lenb(flag_file))
c      if (cmd_dblev(3)) then
c        print *,'.. opening flag file: ',
c    *                        flag_file(1:chr_lenb(flag_file))
c        print *,'.. iunit/flag_id = ',iunit
c      end if
       open (unit=iunit, file=flag_file(1:chr_lenb(flag_file)),
     *       access='DIRECT', recl=flg_recl, status='OLD',
     *       iostat=status )
       if (status.ne.0) goto 999
C read header record
       read(unit=iunit,rec=1,iostat=status) flg_head_rec


C report errors and exit
999    call flag_err( status,'FLAG_OPEN','Unable to access flag table')

       end
C
C
