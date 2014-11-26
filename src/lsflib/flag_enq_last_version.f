C
C
*+  flag_enq_last_version

       subroutine flag_enq_last_version( flag_id, last_version, status )
C      -----------------------------------------------------------------
C
C Enquire the last version number used in the flag file
C
C Given:
C   flag file identifier
       integer     flag_id
C Returned:
C   last version number used
       integer     last_version
C Updated:
C   error status code
       integer     status
C
C The last version number used in the flag table file is found and
C returned via argument to the calling (sub)program.  The calling
C routine must specify a flag file ID.
C
C [PA, 13/8/91]
C-

       include '/mrao/post/include/flag_definition.inc'
       include '/mrao/post/include/flag_errors.inc'

       if (status.ne.0) return

C find the last version number from the flag file
       if (flg_list_open(flag_id)) then
         read (flag_id, rec=1, iostat=status) flg_head_rec
         if (status.ne.0) goto 999
         last_version = flg_last_version
       else
         status = ill_flgopen
       end if
999    call flag_err( status, 'FLAG_ENQ_LAST_VERSION', 'Failed' )
       end
