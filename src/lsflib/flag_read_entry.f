C
C
*+ flag_read_entry

       subroutine flag_read_entry( flag_id, flag_entry,
     *                             flag_version, flag_timdat,
     *                             flag_updated,
     *                             flag_spac_list, flag_time_range,
     *                             flag_operation, flag_comment,
     *                             status                         )
C      ------------------------------------------------------------
C
C Read an entry from the specified flag file
C
C Given:
C   flag file identifier
       integer         flag_id
C   entry number to read
       integer         flag_entry
C Returned:
C   version number for this entry
       integer         flag_version
C   time and date when this entry written
       integer         flag_timdat(6)
C   updated by information
       character*(*)   flag_updated
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
C The specified entry is read from the flag table file.  The calling
C program must specify a valid flag-file identifier and also a valid
C entry number.
C
C [PA, 12/8/91]
C-

       include '/mrao/post/include/flag_definition.inc'
       include '/mrao/post/include/flag_errors.inc'
       include '/mrao/include/chrlib_functions.inc'

C local variables
       integer         i, i1, i2

       if (status.ne.0) return

C check the file is open
       if (flg_list_open(flag_id)) then

C .. read header entry
         read (flag_id, rec=1, iostat=status) flg_head_rec
         if (status.ne.0) goto 999

C .. check the entry to read
         if (flag_entry.le.0 .or. flag_entry.gt.flg_entries) then
           status = ill_flgentry
           goto 999
         end if

C .. read the requested record and associate output items
         read (flag_id, rec=flag_entry+1, iostat=status )
     *         flg_record
         flag_version = flg_version
         do i=1,6
           flag_timdat(i) = flg_timdat(i)
         end do
         if (status.ne.0) goto 999
         flag_updated = flg_updated
         i1 = 1
         i2 = chr_lend(
     *              flg_string(i1:chr_lenb(flg_string)),flg_char)+i1-1
         flag_spac_list = flg_string(i1:i2)
         i1 = i2+2
         i2 = chr_lend(
     *              flg_string(i1:chr_lenb(flg_string)),flg_char)+i1-1
         flag_time_range= flg_string(i1:i2)
         i1 = i2+2
         i2 = chr_lend(
     *              flg_string(i1:chr_lenb(flg_string)),flg_char)+i1-1
         flag_operation = flg_string(i1:i2)
         i1 = i2+2
         i2 = chr_lend(
     *              flg_string(i1:chr_lenb(flg_string)),flg_char)+i1-1
         flag_comment   = flg_string(i1:i2)

       else
         status = ill_flgopen
       end if

999    call flag_err( status, 'FLAG_READ_ENTRY', 'Failed' )
       end
