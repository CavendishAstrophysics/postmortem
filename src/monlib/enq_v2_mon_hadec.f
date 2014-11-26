

*+ enq_v2_mon_hadec

       subroutine enq_v2_mon_hadec( iae, iha, idec, s)
C      -----------------------------------------------
C
C Returns the aerial pointing errors
C
C Input
C   iae             -           i4         -        aerial number
C
C Returned
C   iha              -        2*i4         -        ha values
C   idec             -        2*i4         -        dec values
C   s                -          i4         -        error return
C
C CT Version 2 support for ENQ_MON_HADEC.  The values returned are mean
C and mean absolute pointing errors for HA and Dec, recorded during the
C integration time of the current sample, in arcsecs.
C
C [DJT, 11/3/91]
*-

       integer     iae, iha(2), idec(2), s
       integer     ii

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v2.inc'
       include '/mrao/post/include/samplib_errors.inc'
       include '/mrao/post/include/mon_v2_block.inc'

C check status on entry
       if ( s .ne. 0 ) return

       if ( iae.ge.1 .and. iae.le.max_aes ) then
         do ii = 1,2
           iha(ii)  = mon_ha( iae , ii )
           idec(ii) = mon_dec( iae, ii )
         end do
       else
         s=ill_aerial
         goto 999
       end if

       return

 999   call mon_wrerr( s, 'in subroutine ENQ_V2_MON_HADEC')

       end
