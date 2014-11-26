*+ enq_v2_mon_phe

       subroutine enq_v2_mon_phe( iae, phe, s)
C      ---------------------------------------
C
C Returns the Helium pressure for the current sample
C
C Input
C   iae          -      i4      -     aerial number
C
C Returned
C   phe          -      r4      -     Helium pressure
C   s            -      i4      -     error return
C
C Readings are 12-bit integers, normalised here to values in the range
C 0 to 100.
C
C CT Version 2 support for ENQ_MON_PHE
C
C [DJT, 23/9/94]
*-

       integer     iae, s
       real        phe

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v2.inc'
       include '/mrao/post/include/samplib_errors.inc'
       include '/mrao/post/include/mon_v2_block.inc'

C check status on entry
       if ( s .ne. 0 ) return

       if ( iae.ge.1 .and. iae.le.max_aes ) then
         phe = mon_PHe(iae)/40.96
       else
         s=ILL_AERIAL
         goto 999
       end if

       return

 999   call mon_wrerr( s, 'in subroutine ENQ_V2_MON_PHE')

       end


