

*+SET_V1_NSAMP

       subroutine set_v1_nsamp (last_samp, sid_time, max_byte, status)
C
C  Resets the number of samples in a sample file.
C
C  Given:
C      LAST_SAMP     integer       last sample number
C      SID_TIME      integer       sidereal time for last sample (1/10s)
C
C  Returned:
C      MAX_BYTE      integer       length of file in bytes
C      STATUS        integer       status value
C
C  Control tables version 1 support routine for SET-NUMSAMP.
C  Resets the total sample count in the current sample file. The file
C  packing parameters and stop times are also updated and the run stop
C  flag is set.
C
C  NPR  6 January 87.
C  DJT  17 November 88.
C
*-
       integer    last_samp, sid_time, max_byte
       integer    status
c
       include '/mrao/include/constants.inc'
       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v0.inc'
c
      integer     start, stop, samp_time, ut_duration
c
      if (status.ne.0) return
c
      istop = 1
c
c    Reset the number of samples in the control tables.
      nsamp   = last_samp
c
c    Set the sidereal stop time
      samp_time = intsam*(itab(1)+itab(2))/1000
      stop = mod( ((sid_time+samp_time)/10 + 1), 86400 )
      istim2(3) = stop/3600
      istim2(2) = stop/60 - istim2(3)*60
      istim2(1) = stop - (istim2(3)*60+istim2(2))*60
c
c    Set the UT/GMT stop time.
      start = (istim1(3)*60 + istim1(2))*60 + istim1(1)
      if ( stop.lt.start ) stop = stop + 86400
      ut_duration = dint( dble(stop-start)/const_sut2sst )
      start = (itim1(3)*60 + itim1(2))*60 + itim1(1)
      stop  = mod(start+ut_duration,86400)
      itim2(3) = stop/3600
      itim2(2) = stop/60 - itim2(3)*60
      itim2(1) = stop - (itim2(3)*60+itim2(2))*60
c
c    Reset the max byte pointer - the space allocated for samples must
c    be a multiple of eight pages for historic compatibility reasons.
      max_byte = 4*page*(ictab+8*(int((nsamp*lrecb/(2*page)-1)/8)+1))
c
      end
