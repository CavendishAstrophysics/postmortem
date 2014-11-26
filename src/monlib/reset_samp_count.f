
*+RESET_SAMP_COUNT

       subroutine reset_samp_count (file, status)
C      ------------------------------------------
C
C  Executes the RESET-SAMPLE-COUNT command.
C
C  Given:
C      FILE          char*(*)      current sample file name
C      STATUS        integer       status value
C
C  Routine to reset the total sample count in the current sample file.
C  The file packing parameters and stop times are also updated and
C  run stop flag is set.
C
C  NPR  6 January 1987.
C  DJT  29 October 1991.
C
*-
       character  file*(*)
       integer    status
c
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/samplib_errors.inc'
c
      integer     i, lun, max_byte, sf_type
      integer     sid_time, num_samp, last_samp
      real*8      ra, dec
c
      if (status.ne.0) return
c
c    Open the sample file and find the maximum sample count.
      call open_sf( lun, file, 'write', 0, status )
      call open_source( lun, 1, 0, status )
      call enq_sftype( lun, sf_type, status )
      call enq_numsamp( lun, 1, num_samp, status )
      call read_rt( lun, 1, num_samp, ra, dec, sid_time, status )
      call close_source( lun, 1, status )
      if (sf_type .ne. 1) status = ILL_CONTTAB
      if (status .ne. 0) goto 9999
c
c     Determine the new last sample.
  100 continue
          status = 0
          last_samp = num_samp
          call io_geti( 'last sample:', '*', last_samp, status )
          if (status .ne. 0) goto 9999
c
c        Reset the sample count in the control tables.
          call set_numsamp( lun, last_samp, sid_time, max_byte, status )
c
c        Read the correct sidereal time for the new last sample.
          call open_source( lun, 1, 0, status )
          call read_rt( lun, 1, last_samp, ra, dec, sid_time, status )
          call close_source( lun, 1, status )
c
c        Reset the last sidereal time in the control tables.
          call set_numsamp( lun, last_samp, sid_time, max_byte, status )
c
          if (status .ne. 0) then
              call io_wrout( '*** illegal sample number, try again' )
          end if
      if (status .ne. 0) goto 100
c
c    Write out updated control tables
      call write_ct( lun, status )
      call close_sf( lun, status )

      call exs_print( 'PARAMETERS', status )
c
 9999 if (status.ne.0 .and. status .ne. USR_BREAK) then
          call mon_wrerr( status, 'in routine RESET_SAMP_COUNT' )
      end if
c
      if ( status .ne. 0 ) then
          i = 0
          call close_sf( lun, i )
      end if
c
      return
      end
