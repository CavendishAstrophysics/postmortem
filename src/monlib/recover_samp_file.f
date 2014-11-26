

*+RECOVER_SAMP_FILE

       subroutine recover_samp_file (file, status)
C      -------------------------------------------
C
C  Executes the RECOVER-SAMPLE-FILE command.
C
C  Given:
C      FILE          char*(*)      current sample file name
C      STATUS        integer       status value
C
C  Routine to reset the total sample count in the current sample file,
C  by reading sample redtape to identify the last valid sample.  The
C  file packing parameters and stop times are also updated and the run
C  stop flag is set.
C
C  DJT  18/3/92.
C
*-
       character  file*(*)
       integer    status
c
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/samplib_errors.inc'
c
      integer     last_samp
      integer     i, lun, max_byte, sf_type
      integer     integ_time, sid_time, samp_time
      real*8      ra, dec, ra1, dec1
c
      if (status.ne.0) return
c
      call io_wrout( ' ' )
c
c    Open the sample file and read redtape of the first sample.
      call open_sf( lun, file, 'write', 1, status )
      call open_source( lun, 1, 4, status )
      call read_rt( lun, 1, 1, ra1, dec1, sid_time, status )
      call close_source( lun, 1, status )
      call enq_sftype( lun, sf_type, status )
      call enq_integ( lun, integ_time, status )
      if (sf_type .ne. 1) status = ILL_CONTTAB
      if (status .ne. 0) goto 9999
c
c    Reset the sample count for 12 hours
      last_samp = 864000/integ_time + 1
      call set_numsamp( lun, last_samp, sid_time, max_byte, status )
c
      last_samp = 1
      call open_source( lun, 1, 4, status )
c
c     Find the last valid sample by checking sample redtape.
  100 continue
          last_samp = last_samp + 1
          call read_rt( lun, 1, last_samp, ra, dec, samp_time, status )
          if (status .ne. 0) then
              goto 101
c [This test removed 18/3/92 ... does not work for multi-centre obs!]
c         else if (ra.ne.ra1 .or. dec.ne.dec1) then
c             goto 101
          else if (samp_time.lt.sid_time) then
              goto 101
          else if ((samp_time-sid_time).gt.2*integ_time) then
              goto 101
          end if
          sid_time = samp_time
      if (status .eq. 0) goto 100
c
  101 status = 0
      last_samp = last_samp - 1
      call close_source( lun, 1, status )
c
c    Reset the last sidereal time in the control tables.
      call set_numsamp( lun, last_samp, sid_time, max_byte, status )
      if (status .ne. 0) goto 9999
c
c    Write out updated control tables
      call write_ct( lun, status )
      call close_sf( lun, status )

      call exs_print( 'PARAMETERS', status )
c
 9999 if (status.ne.0 .and. status .ne. USR_BREAK) then
          call mon_wrerr( status, 'in routine RECOVER_SAMP_FILE' )
      end if
c
      if ( status .ne. 0 ) then
          i = 0
          call close_sf( lun, i )
      end if
c
      return
      end
