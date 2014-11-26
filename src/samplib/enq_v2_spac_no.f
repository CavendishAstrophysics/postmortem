C+ENQ_V2_SPAC_NO

       subroutine enq_v2_spac_no ( n, spac_no, s )
C
C Returns the spacing number of the n'th visibility in the sample file
C
C Input
C   N            -      I4      -     visibility index number
C
C Returned
C   SPAC_NO      -      R4      -     spacing number
C   S            -      I4      -     error return
C
C CT Version 2 support for ENQ_SPAC_NO
C
C [DJT, 17/8/93]
*-

       integer     s
       integer     n
       real*4      spac_no

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v2.inc'
       include '/mrao/post/include/samplib_errors.inc'

       integer     isp

C check status on entry
       if ( s .ne. 0 ) return

C determine spacing index number
       isp = (n - 1)/(nsubb*nchannel) + 1

       if ( isp.gt.0 .and. isp.lt.nsp) then
         spac_no = float(isp_code(isp))
       else
         s=ILL_SPAC
         goto 999
       end if

       return

 999   call smp_wrerr( s, 'in subroutine ENQ_V2_SPAC_NO' )

       end


