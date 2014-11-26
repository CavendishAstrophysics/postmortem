C
C
*+ flag_get_key

       subroutine flag_get_key( flag_id, list, nlist,
     *                          key, last_rec, status )
C      ------------------------------------------------
C
C Return a key and record number defining the flag-tables versions
C
C Given:
C   flag table id
       integer       flag_id
C   list of flag table versions to include in key
       integer       list(*)
C   length of the list
       integer       nlist
C
C Returned:
C   key to list of versions
       integer       key
C   last record of the last version accessed
       integer       last_rec
C
C Updated:
C   error return code
       integer       status
C
C The specified flag table is examined and a key determined which
C specifies all of the flag table versions listed in the supplied
C list; the key is in fact just a bit-map of the versions included.
C Version 0 in the flag table is always assumed to be included.
C In addition to the key, the last record number in the flag table
C which would be accessed by any of the flag table version numbers
C in the supplied list, is also returned.
C
C [PA, 14/8/91]
C-

       include '/mrao/post/include/flag_definition.inc'
       include '/mrao/post/include/flag_errors.inc'

C Local variables
       integer       n

C check status on entry
       if (status.ne.0) return

C check flag table is open
       if (flg_list_open(flag_id)) then
         read (flag_id, rec=1, iostat=status) flg_head_rec

C .. prepare key
         call util_clrbfd(key,1,32)
C .. check each version number in the list and set the bit
         do n=1,nlist
           if (list(n).gt.0 .and. list(n).le.flg_last_version) then
             if (list(n).le.32) then
               call util_setbit(key,list(n))
             end if
           end if
         end do
C .. set the last record number
         last_rec = flg_entries

       else
         status = ill_flgopen
       end if

999    call flag_err( status, 'FLAG_GET_KEY', 'Failed' )
       end
