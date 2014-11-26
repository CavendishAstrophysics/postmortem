*+ enq_v2_mon_wind

       subroutine enq_v2_mon_wind( wind, s)
C      ------------------------------------
C
C Returns the wind gauge reading for the current sample
C
C Returned
C   wind         -      r4      -     wind gauge reading (knots)
C   s            -      i4      -     error return
C
C CT Version 2 support for ENQ_MON_WIND
C
C Wind gauge readings are recorded as voltages in the range 0-100V,
C represented by 12-bit integers, with 1V equivalent to a windspeed
C of 10 knots
C
C [DJT, 19/1/93]
*-

       real        wind
       integer     s

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v2.inc'
       include '/mrao/post/include/samplib_errors.inc'
       include '/mrao/post/include/mon_v2_block.inc'

C check status on entry
       if ( s .ne. 0 ) return

       wind = mon_wind/40.96

       return

 999   call mon_wrerr( s, 'in subroutine ENQ_V2_MON_WIND')

       end

