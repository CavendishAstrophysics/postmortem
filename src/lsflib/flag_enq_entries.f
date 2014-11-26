C
C
*+ flag_enq_entries

       subroutine flag_enq_entries( flag_id, flag_version,
     *                              num_entries, list_entries, status )
C      -----------------------------------------------------------------
C
C Enquire a list of entries for the specified flag table version
C
C Given:
C   flag file identifier
       integer     flag_id
C Updated:
C   version number for entries
       integer     flag_version
C Returned:
C   number of entries found
       integer     num_entries
C   list of entries for this version 1 --> num_entries
       integer     list_entries(*)
C Updated:
C   error status code
       integer     status
C
C A list of the entries in the flag-table for the specified version
C is returned.  The calling routine must specify a flag file ID and
C a version number to search.  FLAG_VERSION may be updated by this
C routine if the specified version  number does not correspond to a
C version present in the flag-table, in this case the entries for
C the last version are returned.
C
C [PA, 13/8/91]
C-

       include '/mrao/post/include/flag_definition.inc'
       include '/mrao/post/include/flag_errors.inc'

C Local variables
       integer   n

       if (status.ne.0) return

C find the last version number from the flag file
       if (flg_list_open(flag_id)) then
         read (flag_id, rec=1, iostat=status) flg_head_rec
         if (status.ne.0) goto 999
         if (flag_version.lt.0 .or.
     *       flag_version.gt.flg_last_version ) then
           flag_version = flg_last_version
         end if
         num_entries = 0
         do n=1,flg_entries
           read (flag_id, rec=n+1, iostat=status) flg_record
           if (flg_version.eq.flag_version) then
             num_entries = num_entries + 1
             list_entries(num_entries) = n
           end if
         end do
       else
         status = ill_flgopen
       end if
999    call flag_err( status, 'FLAG_ENQ_ENTRIES', 'Failed' )
       end
