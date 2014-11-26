*+ enq_v2_mon_misc

       subroutine enq_v2_mon_misc( ipar, value, s)
C      -------------------------------------------
C
C Returns the rain gauge reading for the current sample
C
C Input
C   ipar         -      i4      -     parameter index
C
C Returned
C   value        -      r4      -     parameter reading
C   s            -      i4      -     error return
C
C Parameter readings are recorded as voltages in the range 0-10V,
C represented by 12-bit integers (0-4095).  Parameters readings are
C returned as voltages.
C
C CT Version 2 support for ENQ_MON_MISC
C
C [DJT, 22/2/93]
*-

       integer     ipar, s
       real        value

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v2.inc'
       include '/mrao/post/include/samplib_errors.inc'
       include '/mrao/post/include/mon_v2_block.inc'

C check status on entry
       if ( s .ne. 0 ) return

       if ( ipar.ge.1 .and. ipar.le.num_misc ) then
         value = mon_misc( ipar )/409.6
       else
         s=ILL_AERIAL
         goto 999
       end if

       return

 999   call mon_wrerr( s, 'in subroutine ENQ_V2_MON_MISC')

       end

