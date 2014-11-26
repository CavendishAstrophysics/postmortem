*+ flag_close

       subroutine flag_close( flag_id, status )
C      ----------------------------------------
C
C Close the flag file by identifier
C
C Given:
C   flag file identifier
       integer     flag_id
C Updated:
C   error status code
       integer     status
C
C The flag file is closed.
C
C [PA, 12/8/91; GP 8 Feb 93]
C-

       include '/mrao/post/include/flag_definition.inc'
       include '/mrao/post/include/flag_errors.inc'

       if (status.ne.0) return

C close this flag file if open
       if (flg_list_open(flag_id)) then
         close (flag_id)
         flg_list_open(flag_id) = .false.
         flg_list_name(flag_id) = ' '
       else
         status = ill_flgopen
       end if
       call flag_err( status, 'FLAG_CLOSE', 'Failed' )
       end
C
C
