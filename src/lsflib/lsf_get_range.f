C
C+lsf_get_range
C
      SUBROUTINE lsf_get_range(   lsf_num,
     *                            first_buffer,
     *                            last_buffer,
     *                            s                    )

C
C     Gets an lsf buffer range from the command line.
C
C     Given:
C         Logical sample file number
              integer*4           lsf_num
C
C     Returned:
C         First and last buffer in range.
              integer*4           first_buffer, last_buffer
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     The buffer range is got from the command line, prompting if
C     necessary. The range may be prefixed by an alphabetic code
C     with the following implications :
C
C     Code
C     'HA'    - Range is specified as hour angles
C     'ST'    - Range is specified as sidereal times
C     'UT'    - Range is specified as Universal Times
C     'PS'    - Range is specified as physical sample numbers
C     other   - Range is specified as LSF buffer numbers
C
C     NPR, 18 September 1987.
C     DJT, 24 June 1994.
C
C-
C     ****************************************************************
C
C     Function declarations
C
      include  '/mrao/include/chrlib_functions.inc'
C
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/constants.inc'
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/post/include/lsflib_errors.inc'
      include  '/mrao/post/include/samplib_errors.inc'
      include  '/mrao/post/include/lsf_definition.inc'
      include  '/mrao/post/include/lsf_runtime.inc'

C     ****************************************************************
C
C     Constants
C         Number of sidereal time units (10ths sec) in a sidereal day.
              integer             day2st
              parameter         ( day2st = 864000 )

C     Local variables, equivalences and commons
C         General purpose integer - loop counter or dummy variable
              integer             i
C         User's response
              character*(80)      response
C         Command line and length
              character*(128)     cline
              integer             l
C         Number of buffers in LSF
              integer             num_buff
C         Flags to indicate whether the range is specified as
C         hour angle, sidereal time, local time, sample or buffer number
              logical             ha, st, ut, ps, bn
C         General purpose time variable (hours)
              real*8              time
C         Start and end sidereal time of LSF (hours)
              real*8              start_time, stop_time
C         Offset to apply to sidereal time to get hour angle.
              real*8              ha_offset
C         Modified Julian date for zero of sidereal time
              real*8              mjd0
C         UT at zero of sidereal time (hours)
              real*8              ut0_hrs
C         Start and end sidereal time of LSF - as 10ths of second.
              integer             lsf_start_sid, lsf_end_sid
C         Start and end sidereal time of range - as 10ths of second.
              integer             start_sid, end_sid
C         First and last physical sample of range.
              integer             first_sample, last_sample
C         Parameters for read_rt_sid
              real*8              ra, dec
              integer             sid

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

      if ( lsf_num .ne. curr_lsf_num ) then
          call get_lsf( lsf_num, s )
          if ( s .ne. 0 ) goto 9999
      end if

C     ****************************************************************
C
C         Main Code
C         ---------
C
      response = ' '

      call io_getc( 'First buffer : ', '1', response, s )

      ha = chr_cmatch( response(1:2), 'HA' )
      st = chr_cmatch( response(1:2), 'ST' )
      ut = chr_cmatch( response(1:2), 'UT' )
      ps = chr_cmatch( response(1:2), 'PS' )
      bn = .not. ( ha .or. st .or. ut .or. ps )

C     Restore the rest of the command line.
      if (bn) then
          i = chr_lenb(response)
          call io_enqcli( cline, l )
          call io_setcli( response(1:i)//' '//cline(1:l) )
      end if

C     Find the last buffer number in the LSF (the first buffer is no. 1)
      call buffer_num( num_samp, num_buff, i, s )
      if ((s .ne. 0) .and. (s .ne. ILL_BUFFER)) goto 9999
      s = 0

C     Find the first and last sample numbers in the LSF, if required.
      if ( .not. bn ) then
          call sample_num( 1, i, first_sample, s )
          call sample_num( num_buff, i, last_sample, s )
          last_sample = min( (last_sample+samp_rate-1), num_samp )
      end if

C     Find first and last sidereal times in the LSF, also if required.
      if ( ha .or. st .or. ut ) then
          call read_rt( sf_lun, src_num, first_sample,
     *                  ra, dec, lsf_start_sid, s         )
          start_time = dfloat(lsf_start_sid)/36000.0D+0
          call read_rt( sf_lun, src_num, last_sample,
     *                  ra, dec, lsf_end_sid, s           )
          stop_time  = dfloat(lsf_end_sid)/36000.0D+0
      end if
      if ( s .ne. 0 ) goto 9999

      if ( ha ) then
          ha_offset = (baseln_skew - epoch_ra)/const_h2r
          time = start_time + ha_offset
          call io_getsd( 'Start hour angle : ', '*', time, s )
          start_sid= int((time-ha_offset)*36000.0D+0)

          time = stop_time  + ha_offset
          call io_getsd( 'End hour angle   : ', '*', time, s )
          end_sid  = int((time-ha_offset)*36000.0D+0)
      else if ( st ) then
          time = start_time
          call io_getsd( 'Start sidereal time : ', '*', time, s )
          start_sid = int(time*36000.0D+0)

          time = stop_time
          call io_getsd( 'End sidereal time   : ', '*', time, s )
          end_sid   = int(time*36000.0D+0)
      else if ( ut ) then
          call enq_mjd_st0( sf_lun, mjd0, s )
          ut0_hrs  = (mjd0-int(mjd0))*24.0D+0
          time = dmod((ut0_hrs+start_time/const_sut2sst), 24.0D+0 )
          call io_getsd( 'Start universal time : ', '*', time, s )
          if ( time .lt. ut0_hrs ) time = time + 24.D+0
          start_sid = int((time-ut0_hrs)*const_sut2sst*36000.0D+0)

          time = dmod((ut0_hrs+stop_time/const_sut2sst), 24.0D+0 )
          call io_getsd( 'End universal time   : ', '*', time, s )
          if ( time .lt. ut0_hrs ) time = time + 24.D+0
          end_sid   = int((time-ut0_hrs)*const_sut2sst*36000.0D+0)
      else if ( ps ) then
          call io_geti( 'First sample : ', '*', first_sample, s )
          call io_geti( 'Last sample  : ', '*', last_sample, s )
      else
          first_buffer = 1
          call io_geti( 'First buffer : ', '*', first_buffer, s )
          last_buffer  = num_buff
          call io_geti( 'Last buffer  : ', '*', last_buffer, s )
      end if
      if ( s .ne. 0 ) goto 9999

C     Convert times to sample numbers
      if ( ha .or. st .or. ut ) then
C         Ensure the specified times are before the end of the LSF.
          if (start_sid.gt.lsf_end_sid) start_sid= mod(start_sid,day2st)
          if (end_sid  .gt.lsf_end_sid) end_sid  = mod(end_sid  ,day2st)

C         Ensure the start time is after the start of the LSF
  100     if (start_sid .ge. lsf_start_sid ) goto 200
              start_sid = start_sid + day2st
          goto 100
  200     continue
C         Ensure the stop time is after the start time
  300     if (end_sid .gt. start_sid ) goto 400
              end_sid = end_sid + day2st
          goto 300
  400     continue

C         Maximise the overlap of the times with the LSF
          if ((end_sid-day2st-max(start_sid-day2st,lsf_start_sid)) .gt.
     *        (min(lsf_end_sid, end_sid)-start_sid)               ) then
              start_sid = max( start_sid-day2st, lsf_start_sid )
              end_sid   = min( end_sid  -day2st, lsf_end_sid )
          else
              start_sid = max( start_sid, lsf_start_sid )
              end_sid   = min( end_sid,   lsf_end_sid   )
          end if

          call read_rt_sid( sf_lun, src_num, start_sid,
     *                      first_sample, ra, dec, sid, s )
          if ((s .ne. 0) .and. (s .ne. ILL_SAMPLE)) goto 9999
          s = 0

          call read_rt_sid( sf_lun, src_num, end_sid,
     *                      last_sample, ra, dec, sid, s )
          if ((s .ne. 0) .and. (s .ne. ILL_SAMPLE)) goto 9999
          s = 0
      end if

C     Convert sample numbers to buffer numbers.
      if ( ps .or. ha .or. st .or. ut ) then
          call buffer_num( first_sample, first_buffer, i, s )
          if ((s .ne. 0) .and. (s .ne. ILL_BUFFER)) goto 9999
          s = 0

          call buffer_num( last_sample, last_buffer, i, s )
          if ((s .ne. 0) .and. (s .ne. ILL_BUFFER)) goto 9999
          s = 0
      end if

C     Ensure a range of at least one.
      first_buffer = max( 1, min(first_buffer, num_buff-1) )
      last_buffer  = min( num_buff, last_buffer )

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          if ( s .ne. USR_BREAK ) then
              call lsf_wrerr( s, 'in subroutine LSF_GET_RANGE' )
          end if
          return
      end
