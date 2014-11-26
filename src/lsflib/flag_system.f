*+ flag_system

       subroutine flag_system( lsf_num, sf_lun, status )
C      -------------------------------------------------
C
C Enter the flagging sub-system
C
C Given:
C   Logical sample file number
       integer   lsf_num
C   Sample file logical unit number
       integer   sf_lun
C
C Updated:
C   Error return
       integer   status
C
C [PA, 14/8/91]
C-

       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/post/include/lsf_definition.inc'
       include '/mrao/post/include/flag_definition.inc'
       include '/mrao/post/include/post_sys_pars.inc'

C
C Local variables
C   flagging file name and ID
       character*64     ff_name
       integer          flag_id
C local error status
       integer          istat
C current command line
       character*120    command_line
       integer          len_cli
C number of commands defined in main level and number of sub-options
       integer          number_commands
       parameter       (number_commands = 7)
C list of commands defined at this level
       character*80     liscom(number_commands)
C command-line parameters to pass to commands & current command name
       character*80     command
C command number found in parsing routine
       integer          icom
C length of command
       integer          len_c
C flag to indicate exit on completion of command
       logical          exit_at_end
C output unit and catalogue entry
       integer          iout
C list of entries
       character*80     clist
       integer          vlist(32), nvlist
       integer          lv, iv
C options to commands
       integer          nlist_options
       parameter       (nlist_options = 3)
       character*60     list_options(nlist_options)
       data list_options(1) /
     * 'full ............. full listing of flag table (4 lines) '/
       data list_options(2) /
     * 'brief ............ brief listing of flag table (2 lines) '/
       data list_options(3) /
     * 'short ............ short listing of flag table (1 line)'/
       character*20     option

C
C-----------------------------------------------------------------------
C
       data liscom(1) /
     *  'help ......................... obtain help information'
     *                /
       data liscom(2) /
     *  'set-flag-file ................ set name of flag file'
     *                /
       data liscom(3) /
     *  'list-flag-table .............. list entries in flag table'
     *                /
       data liscom(4) /
     *  'list-lsf-flagging ............ list info in LSF flagging'
     *                /
       data liscom(5) /
     *  'add-entry .................... add entry to flag table'
     *                /
       data liscom(6) /
     *  'copy-entries ................. copy entries in flag table'
     *                /
       data liscom(7) /
     *  'show-array-status ............ show status of flagging array'
     *                /
C
C-----------------------------------------------------------------------

C check status on entry
       if (status.ne.0) return

C find flag-file name and open
       flg_lsf_num = lsf_num
       flg_sf_lun = sf_lun
       call enq_namfil( sf_lun, 'FLAG', ff_name, status )
       call flag_open( ff_name, flag_id, status )
       if (status.ne.0) goto 999

C check command line on entry
       call io_enqcli( command_line, len_cli )
       exit_at_end = len_cli.gt.0

C  Command line interpretation
C  ---------------------------
1000   continue

C reset error return
         status = 0
         call cmd_setparam('%sub-system','FLAG-SYSTEM',status)

C scan command line
         call cmd_getcmd('Flag> ',liscom,
     *                   number_commands,icom,status)

C check for error
         if (status.ne.0) then
           call cmd_err(status,'FLAG-SYSTEM',' ')
           goto 1000
         end if

C execute command
         if (icom.eq.0) goto 5000

C .. normal command
         len_c = chr_lenw(liscom(icom))
         command = liscom(icom)
         call chr_chucas(command)

         if (command(1:len_c).eq.'HELP') then
           call hlp_setfil( help_file, status )
           call io_enqcli( command_line, len_cli )
           call hlp_system( command_line(1:len_cli),
     *                      'terminal', .true., .true., status)
           call cmd_err(status,'help',' ')

         elseif (chr_cmatch(command(1:len_c),'set-flag-file'))then
           call io_getfil('Flag-file : ','*',ff_name,status)
           call flag_close( flag_id, status )
           call flag_open( ff_name, flag_id, status )

         elseif (chr_cmatch(command(1:len_c),'list-flag-table'))then
           call io_getopt('Listing-option (?=list) : ','SHORT',
     *                  list_options,nlist_options,option,status)
           call flag_enq_last_version( flag_id, lv, status )
           clist = ' '
           do iv=0,lv
             vlist(iv+1) = iv
             clist(2*iv+1:2*iv+1) = char(ichar('0') + iv)
           end do
           nvlist = lv + 1
           call io_getlst('Version(s) to list : ','*',clist,
     *                 vlist, lv+1, nvlist, status )
           call io_enqout(iout)
           call flag_list_entries( flag_id, iout, option,
     *                             vlist, nvlist, status)

         elseif (chr_cmatch(command(1:len_c),'list-lsf-flagging'))then
           call flag_get_list( flag_id, flag_key, vlist, nvlist, status)
           call io_enqout(iout)
           write(iout,101) flag_flag, flag_record,
     *                     (vlist(iv), iv=1,nvlist)
 101       format(1X/1X,'Flagging in current LSF: '/
     *               1X,'Flag  (=1 flagging selected) = ',I4/
     *               1X,'Use entries to record number = ',I4/
     *               1X,'List of versions: ',8(4I4/19X) )

         elseif (chr_cmatch(command(1:len_c),'add-entry'))then
           call flag_add_entry( flag_id, lsf_num, sf_lun,
     *                          'FLAG-SYSTEM', status )

         elseif (chr_cmatch(command(1:len_c),'copy-entries'))then
           call io_getlst('List entries to copy : ','0',clist,
     *                 vlist, 32, nvlist, status )
           call io_geti(
     *               'Version to write entries to (-1=new-version) : ',
     *               '-1',iv,status)
           call flag_copy_entries( flag_id, vlist, nvlist, iv, status)

         elseif (chr_cmatch(command(1:len_c),'show-array-status'))then
           call io_enqout(iout)
           write(iout,102) flg_calculation, flg_lsf
102        format(1X,'Do array calculation = ',L2,' for lsf = ',I10)

         end if
         if (exit_at_end) goto 5000
       goto 1000

5000   continue
999    call flag_err( status, 'FLAG_SYSTEM', ' ')
       istat = 0
       call flag_close( flag_id, istat )
       end
