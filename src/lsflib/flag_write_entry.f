*+ flag_write_entry

       subroutine flag_write_entry( flag_id, lsf_num, sf_lun,
     *                              flag_version, program,
     *                              flag_spac_list, flag_time_range,
     *                              flag_operation, flag_comment,
     *                              status                         )
C      ------------------------------------------------------------
C
C Write an entry to the specified flag file
C
C Given:
C   flag file identifier
       integer         flag_id
C   logical sample file number
       integer         lsf_num
C   sample file logical unit number
       integer         sf_lun
C Updated:
C   version number for new entry (-1 = latest version)
       integer         flag_version
C Given:
C   program name of program writing entry
       character*(*)   program
C   spacing list
       character*(*)   flag_spac_list
C   time range
       character*(*)   flag_time_range
C   operation requested
       character*(*)   flag_operation
C   comment
       character*(*)   flag_comment
C
C Updated:
C   error status code
       integer         status
C
C A new entry is written to the flag table file.
C The entry is written at version FLAG_VERSION;
C if FLAG_VERSION=-1 it is written at the latest version,
C and if FLAG_VERSION>= latest_version, then it is written with
C a new version number.  FLAG_VERSION is updated by this routine to
C return the version actually written.
C
C [PA, 12/8/91] [save_sf added 8 Feb 93 GP]
C-

       include '/mrao/post/include/global_constants.inc'
       include '/mrao/post/include/flag_definition.inc'
       include '/mrao/post/include/flag_errors.inc'
       include '/mrao/include/chrlib_functions.inc'

C local variables
       integer         mode, termno, i1, i2
       character*64    flag_file
C list buffer
       integer         max_list
       parameter      (max_list = max(max_samp,max_vis) )
       integer         list(max_list), nlist


       if (status.ne.0) return

C check the file is open
       if (flg_list_open(flag_id)) then

C .. read header entry
         read (flag_id, rec=1, iostat=status) flg_head_rec
         if (status.ne.0) goto 999

C .. check the version to write
         if (flag_version.lt.0) then
           flg_version = flg_last_version
         else if (flag_version.gt.flg_last_version) then
           flg_version = flg_last_version + 1
           flg_last_version = flg_last_version + 1
         else
           flg_version = flag_version
         end if
         flag_version = flg_version

C .. check the information supplied in the calling routine
         i1 = chr_intlc(flag_spac_list)
         i2 = chr_lenb(flag_spac_list)
         call set_spacings( sf_lun,
     *                      flag_spac_list(i1:i2),
     *                      list, max_list, nlist, status )
         if (status.ne.0) then
           call flag_err(status,'FLAG_WRITE_ENTRY',
     *                   'Illegal spacing list'   )
           status = ill_flgdata
           goto 999
         end if
         i1 = chr_intlc(flag_time_range)
         i2 = chr_lenb(flag_time_range)
         call lsf_set_slist( lsf_num,
     *                       flag_time_range(i1:i2),
     *                       list, nlist, status )
         i1 = chr_intlc(flag_operation)
         i2 = chr_lenb(flag_operation)
         if (status.ne.0) then
           call flag_err(status,'FLAG_WRITE_ENTRY',
     *                   'Illegal sample range'   )
           status = ill_flgdata
           goto 999
         end if
         if ( .not.(chr_cmatch(flag_operation(i1:i2),'SET') .or.
     *              chr_cmatch(flag_operation(i1:i2),'UNSET')) ) then
           status = ill_flgdata
           call flag_err(status,'FLAG_WRITE_ENTRY',
     *                   'Illegal operation'      )
           goto 999
         end if

C .. update entries in the record
         call util_enqtim(flg_timdat(1))
         call util_enqdat(flg_timdat(4))
         if (status.ne.0) goto 999
         flg_updated(1:16) = program
         call io_enqexe(flg_updated(17:32),mode,termno)
         flg_string = flag_spac_list // flg_char //
     *                flag_time_range // flg_char //
     *                flag_operation // flg_char //
     *                flag_comment// flg_char
         flg_entries = flg_entries + 1
         write (flag_id, rec=flg_entries+1, iostat=status )
     *          flg_record
         write (flag_id, rec=1, iostat=status )
     *          flg_head_rec
         flag_file = flg_list_name(flag_id)
c        call save_sf(flag_file, status)  ! add name to save-lsf:list

       else
         status = ill_flgopen
       end if

999    call flag_err( status, 'FLAG_WRITE_ENTRY', 'Failed' )
       end
C
C
