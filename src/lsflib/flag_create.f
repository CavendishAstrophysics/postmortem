C
C
C
*$ Section 1: Routines to Access the Flag-Table File
*  -------------------------------------------------

*+ flag_create

       subroutine flag_create( flag_file, status )
C      -------------------------------------------
C
C Create a flag file
C
C Given:
C   file name of flag file
       character*(*)   flag_file
C Updated:
C   error return
       integer         status
C
C A flag file of the specified name is created and initialised.  This
C routine will NOT overwrite an existing flag file.  After initialisation
C only a header record is present in the flag file.
C
C [PA, 12/8/91]
C last mod 12 Apr 2000 GP [io_setacc]
C-

C include files
       include '/mrao/post/include/flag_definition.inc'
       include '/mrao/post/include/flag_errors.inc'
       include '/mrao/include/chrlib_functions.inc'

C local variables
       integer   iunit
       logical   exists

       if (status.ne.0) return

C check that the file does not exist
       inquire (file=flag_file(1:chr_lenb(flag_file)), exist=exists)

C choose a suitable unit number for file operations
       call io_nxtlun( iunit, status )

       if (.not.exists) then
C .. create file if it does not exist
         open (unit=iunit, file=flag_file(1:chr_lenb(flag_file)),
     *         access='DIRECT', recl=flg_recl, status='NEW',
     *         iostat=status )
         call io_setacc(flag_file(1:chr_lenb(flag_file)),
     *                                'r', 'rw', 'rw', status)
C .. add header record to this file
         flg_head_version = 1
         flg_head_file_name = flag_file
         call util_enqtim(flg_head_create_time)
         call util_enqdat(flg_head_create_date)
         flg_entries = 0
         flg_last_version = 0
         write(unit=iunit,rec=1,iostat=status) flg_head_rec
         close (iunit)

       else
C .. error - flag table already exists
        status = ill_flgexist

       end if

C report errors and exit
       call flag_err( status,'FLAG_CREATE','Flag file not created')

       end
