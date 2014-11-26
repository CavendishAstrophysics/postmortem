*+ enq_v2_mon_cryo

       subroutine enq_v2_mon_cryo( iae, tcryo, s)
C      ------------------------------------------
C
C Returns the cryo temperature for the current sample
C
C Input
C   iae          -      i4      -     aerial number
C
C Returned
C   tcryo        -      r4      -     cryo temperature
C   s            -      i4      -     error return
C
C Readings are 12-bit integers, normalised here to values in the range
C 0 to 100.
C
C CT Version 2 support for ENQ_MON_CRYO
C
C [DJT, 15/2/94]
*-

       integer     iae, s
       real        tcryo

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v2.inc'
       include '/mrao/post/include/samplib_errors.inc'
       include '/mrao/post/include/mon_v2_block.inc'

C check status on entry
       if ( s .ne. 0 ) return

       if ( iae.ge.1 .and. iae.le.max_aes ) then
         tcryo = mon_tcryo(iae)/40.96
       else
         s=ILL_AERIAL
         goto 999
       end if

       return

 999   call mon_wrerr( s, 'in subroutine ENQ_V2_MON_CRYO')

       end


