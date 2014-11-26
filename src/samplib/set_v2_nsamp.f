
*+SET_V2_NSAMP

       subroutine set_v2_nsamp (lun, last_samp, sid_time, max_byte,
     :                                                           status)
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
C      STATUS        integer       status value
C
C  Control tables version 2 support routine for SET-NUMSAMP.
C  Resets the total sample count in the current sample file. The file
C  packing parameters and stop times are also updated and the run stop
C  flag is set.
C
C  DJT  28/10/91.
C
*-
       integer    lun, last_samp, sid_time, max_byte
       integer    status
c
       include '/mrao/include/constants.inc'
       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v2.inc'
       include '/mrao/post/include/src_pack.inc'
c
      integer     start, stop, samp_time, ut_duration

      if (status.ne.0) return

      istop = 1

c    Get source packing information
      call enq_src_pack( lun, 1, src_pack, status )

c    Reset the number of samples in the control tables.
      Nsamp = last_samp
      src_num_samp = last_samp

c    Set the sidereal stop time
      samp_time = integration*10
      src_stop_time = sid_time+samp_time/2
      stop = mod( (src_stop_time)/10 + 1, 86400 )
      istim2(3) = stop/3600
      istim2(2) = stop/60 - istim2(3)*60
      istim2(1) = stop - (istim2(3)*60+istim2(2))*60

c    Set the UT/GMT stop time.
      start = (istim1(3)*60 + istim1(2))*60 + istim1(1)
      if ( stop.lt.start ) stop = stop + 86400
      ut_duration = dint( dble(stop-start)/const_sut2sst )
      start = (itim1(3)*60 + itim1(2))*60 + itim1(1)
      stop  = mod(start+ut_duration,86400)
      itim2(3) = stop/3600
      itim2(2) = stop/60 - itim2(3)*60
      itim2(1) = stop - (itim2(3)*60+itim2(2))*60

c    Store source packing information
      call set_src_pack( lun, 1, src_pack, status )

c    Reset the max byte pointer
      max_byte = 4*(src_samp_ptr+src_num_samp*src_samp_len)

      end
