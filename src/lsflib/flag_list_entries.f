*+ flag_list_entries

       subroutine flag_list_entries( flag_id, iout, option,
     *                               list, nlist, status  )
C      ----------------------------------------------------
C
C List entries in the flag table
C
C Given:
C   flag file identifier
       integer         flag_id
C   output unit for listing
       integer         iout
C   option for the output style
       character*(*)   option
C   list of versions to list
       integer         list(*)
C   length of the list
       integer         nlist
C
C Updated:
C   error status code
       integer         status
C
C Entries are listed to unit IOUT.  All the entries for each of the
C versions listed in LIST are listed, grouped by version number.
C
C [PA, 14/8/91] [string-lengths & formats edited 9 Feb 93 GP]
C-

       include '/mrao/post/include/flag_definition.inc'
       include '/mrao/post/include/flag_errors.inc'
       include '/mrao/include/chrlib_functions.inc'

C local variables
       integer         n, nn, i, i0, i1, i2, i3, i4, ls
       character*192   flg_spac_list, flg_time_range, flg_comment
       character*10    flg_operation
C option code
       integer         iopt

       if (status.ne.0) return

C check style option
       iopt = 0
       if (chr_cmatch(option(1:chr_lenb(option)),'FULL')) then
         iopt = 1
       elseif (chr_cmatch(option(1:chr_lenb(option)),'BRIEF')) then
         iopt = 2
       elseif (chr_cmatch(option(1:chr_lenb(option)),'SHORT')) then
         iopt = 3
       end if
       if (iopt.eq.0) then
         status = ill_floption
         goto 999
       end if

C check the file is open
       if (flg_list_open(flag_id)) then

         read (flag_id, rec=1, iostat=status) flg_head_rec
         if (status.ne.0) goto 999

C .. loop for each version in the list
         i1 = chr_lenb(flg_head_file_name)
         write(iout,50) flg_head_file_name(1:i1),
     *                  flg_head_create_time(3),flg_head_create_time(2),
     *                  (flg_head_create_date(i), i=1,3)
50       format(1X/1X,'Flag table: ',A/
     *             1X,'Created   : ',I4,'.',I2.2,
     *                ' on ',I2,':',I2.2,':',I4/
     *             1X,80('-'))
         do n=1,nlist
           do nn=1,flg_entries
             read (flag_id, rec=nn+1, iostat=status) flg_record
             if (status.ne.0) goto 999
             ls = chr_lenb(flg_string)
             i1 = 1
             i2 = chr_lend(flg_string(i1:ls),flg_char)+i1-1
             flg_spac_list = flg_string(i1:i2)
             i1 = i2+2
             i2 = chr_lend(flg_string(i1:ls),flg_char)+i1-1
             flg_time_range= flg_string(i1:i2)
             i1 = i2+2
             i2 = chr_lend(flg_string(i1:ls),flg_char)+i1-1
             flg_operation = flg_string(i1:i2)
             i1 = i2+2
             i2 = chr_lend(flg_string(i1:ls),flg_char)+i1-1
             flg_comment   = flg_string(i1:i2)
             if (flg_version.eq.list(n)) then
               i0 = chr_lenb(flg_updated)
               i1 = chr_lenb(flg_spac_list)
               i2 = chr_lenb(flg_time_range)
               i3 = chr_lenb(flg_operation)
               i4 = chr_lenb(flg_comment)
               if (iopt.eq.1)
     *         write(iout,101) nn, flg_version,
     *                         flg_timdat(3),flg_timdat(2),
     *                        (flg_timdat(i), i=4,6),
     *                         flg_updated(1:i0),
     *                         flg_spac_list(1:i1),
     *                         flg_time_range(1:i2),
     *                         flg_operation(1:i3),
     *                         flg_comment(1:i4)
101      format(1X,'Entry: ',I3,' Version     : ',I2,
     *          ' of ',I2.2,'.',I2.2,' on ',I2,':',I2.2,':',I4,
     *          ' by ',A/
     *          1X,'           Spacing-list: ',A/
     *          1X,'           Sample-range: ',A/
     *          1X,'           Operation   : ',A/
     *          1X,'           Comment     : ',A)
               if (iopt.eq.2)
     *         write(iout,102) nn, flg_version,
     *                         flg_timdat(3),flg_timdat(2),
     *                        (flg_timdat(i), i=4,6),
     *                         flg_updated(1:i0),
     *                         flg_operation(1:i3),
     *                         flg_spac_list(1:i1),
     *                         flg_time_range(1:i2)
102      format(1X,'Entry: ',I3,' Version     : ',I2,
     *          ' of ',I2.2,'.',I2.2,' on ',I2,':',I2.2,':',I4,
     *          ' by ',A/
     *          1X,'           Flagging    : ',A,' % ',A,' % ',A)
               if (iopt.eq.3)
     *         write(iout,103) nn, flg_version,
     *                         flg_operation(1:i3),
     *                         flg_spac_list(1:i1),
     *                         flg_time_range(1:i2)
103      format(1X,'Flag: ',I3,' Vs: ',I2,
     *          ' % ',A,' % ',A,' % ',A)
             end if
           end do
         end do
         write(iout,*)' '
       else
         status = ill_flgopen
       end if

999    call flag_err( status, 'FLAG_LIST_ENTRIES', 'Failed' )
       end
C
C
