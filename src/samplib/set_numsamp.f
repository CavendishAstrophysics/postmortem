

C+SET_NUMSAMP

       subroutine set_numsamp (lun, last_samp, sid_time, max_byte, s)
C
C  Resets the number of samples in a sample file.
C
C  Given:
C      LUN           integer       sample file logical unit number
C      LAST_SAMP     integer       last sample number
C      SID_TIME      integer       sidereal time for last sample (1/10s)
C
C  Returned:
C      MAX_BYTE      integer       length of file in bytes
C      S             integer       status value
C
C  Resets the total sample count in the current sample file. The file
C  packing parameters and stop times are also updated and the run stop
C  flag is set.
C
C  DJT  28 October 91.
C
*-
       integer    lun, last_samp, sid_time, max_byte
       integer    sf_type
       integer    s
c
       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/samplib_errors.inc'

       if ( s .ne. 0 ) return

       call read_ct( lun, s )
       if ( s .ne. 0 ) goto 999

       call enq_sftype( lun, sf_type, s )
       if (sf_type .eq. 1) then
         if (ct_vers .le. 1) then
           call set_v1_nsamp( last_samp, sid_time, max_byte, s )
         elseif (ct_vers .eq. 2) then
           call set_v2_nsamp( lun, last_samp, sid_time, max_byte, s )
         else
           s = ILL_CONTTAB
           goto 999
         endif
       else
         s = NOT_PHYSF
         goto 999
       endif

       return

 999   call smp_wrerr( s, 'in subroutine SET_NUMSAMP' )

       end
