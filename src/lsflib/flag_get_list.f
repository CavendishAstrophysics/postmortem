C
C
*+ flag_get_list

       subroutine flag_get_list( flag_id, key, list, nlist, status )
C      -------------------------------------------------------------
C
C Return a list of version numbers from a key
C
C Given:
C   flag table id
       integer       flag_id
C   key to list of versions
       integer       key
C
C Returned:
C   list of flag table versions to include in key
       integer       list(*)
C   length of the list
       integer       nlist
C
C Updated:
C   error return code
       integer       status
C
C The specified flag table is examined and from the key a list of
C versions to include; the key is in fact just a bit-map of the
C versions included. Version 0 in the flag table is always included
C in the returned list.
C
C [PA, 14/8/91]
C-

       include '/mrao/post/include/flag_definition.inc'
       include '/mrao/post/include/flag_errors.inc'
c      include '/mrao/include/cmd_functions.inc'

C Local variables
       integer       n
C Function to test bit in the key
       logical       util_tstbit

C check status on entry
       if (status.ne.0) return

c      if (cmd_dblev(3)) then
c        print *,'..(FLAG-GET-LIST) flag_id = ',flag_id
c        print *,'..(FLAG-GET-LIST) flg_list_open: '
c        print *,flg_list_open
c      end if

C check flag table is open
       if (flg_list_open(flag_id)) then
c        if (cmd_dblev(3)) then
c          print *,'.. reading header from unit ',flag_id
c        end if
         read (flag_id, rec=1, iostat=status) flg_head_rec
         if (status.ne.0) goto 999
c        if (cmd_dblev(3)) then
c          print *,'.. read header OK'
c        end if
C .. prepare list
         nlist = 1
         list(nlist) = 0
C .. check each bit (version number) in the key and set the list
         do n=1,32
           if (util_tstbit(key,n)) then
             nlist = nlist + 1
             list(nlist) = n
           end if
         end do
       else
         status = ill_flgopen
       end if

999    call flag_err( status, 'FLAG_GET_LIST', 'Failed' )
       end
