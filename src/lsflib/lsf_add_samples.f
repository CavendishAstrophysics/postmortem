C
C+lsf_add_samples
C
      SUBROUTINE lsf_add_samples (lsf_num, s)

C
C     Asks the user to select the samples to include in the lsf.
C     This routine is identical to lsf_set_samples apart from not 
C     clearing the currently-selected spacings.

C     Given:
C         Logical sample file number
              integer*4           lsf_num
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     At present only a first sample and a last sample, with constant
C     sampling in between is allowed.
C
C     NPR, 1987.
C     DJT, 24 June 1994.
C     GGP  31 March 2000
C
C-
C     ****************************************************************
C
C     Function declarations
C
      include  '/mrao/include/chrlib_functions.inc'

C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/constants.inc'
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/post/include/lsf_definition.inc'
      include  '/mrao/post/include/lsf_runtime.inc'
      include  '/mrao/post/include/lsflib_errors.inc'
      include  '/mrao/post/include/samplib_errors.inc'

C     ****************************************************************
C
C     Constants
C         Number of sidereal time units (10ths sec) in a sidereal day.
              integer         day2st
              parameter     ( day2st = 864000 )

C     Local variables, equivalences and commons
C         Loop control variable
              integer*4       i
C         Sample list default and user's selection
              character*80    default
              character*128   response
C         Command line and length
              character*128   cline
              integer         l
C         Sample list as an integer array, and its length.
              integer         list( max_samp )
              integer         list_len
C         Count of valid samples in the list.
              integer         valid_samps
C         Number of buffers in the current LSF.
              integer         num_buff
C         Flags to indicate HA, ST, or UT input.
              logical         ha, st, ut
C         General purpose time variable (hours).
              real*8          time
C         Start and end sidereal time of LSF (hours).
              real*8          start_time, stop_time
C         Offset to apply to sidereal time to get hour angle.
              real*8          ha_offset
C         Modified Julian date for zero of sidereal time.
              real*8          mjd0
C         UT at zero of sidereal time (hours)
              real*8          ut0_hrs
C         Start and end sidereal time of current LSF - as 10ths of second.
              integer         lsf_start_sid, lsf_end_sid
C         Start and end sidereal time of sample file - as 10ths of second.
              integer         sf_start_sid, sf_end_sid
C         Start and end sidereal time of range - as 10ths of second.
              integer         start_sid, end_sid
C         First and last sample in the sample range selected.
              integer         first_sample, last_sample
C         Parameters for read_rt, read_rt_sid.
              real*8          ra, dec
              integer         sid

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
      lsf_key  = 1
      lsf_name = ' '

C     Construct default string from current lsf.
      call bits_to_string( samp_list, num_samp, .false., default, s )
      call io_getc( 'Sample list : ', default, response, s )

C     Parse response for keyword prefix.
      ha = chr_cmatch( response(1:2), 'HA' )
      st = chr_cmatch( response(1:2), 'ST' )
      ut = chr_cmatch( response(1:2), 'UT' )

C     Restore the rest of the command line.
      if ( .not. (ha .or. st .or. ut) ) then
          i = chr_lenb( response )
          call io_enqcli( cline, l )
          call io_setcli( response(1:i)//' '//cline(1:l) )
      end if

      if ( ha .or. st .or. ut ) then
C         Sample list is a sample time range.

C         Find the last buffer number in the current lsf (first is no 1)
          call buffer_num( num_samp, num_buff, i, s )
          if ((s .ne.0) .and. (s .ne. ILL_BUFFER)) goto 9999
          s = 0

C         Find the first and last sample numbers in the current lsf.
          call sample_num( 1, i, first_sample, s )
          call sample_num( num_buff, i, last_sample, s )

C         Find first and last sidereal times in the current lsf.
          call read_rt( sf_lun, src_num, first_sample,
     *                        ra, dec, lsf_start_sid, s )
          start_time = dfloat(lsf_start_sid)/36000.0D+0
          call read_rt( sf_lun, src_num, last_sample,
     *                        ra, dec, lsf_end_sid, s )
          stop_time  = dfloat(lsf_end_sid)/36000.0D+0

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
          end if
          if ( s .ne. 0 ) goto 9999

C         Convert times to sample numbers

C         Find first and last sidereal times in the sample file.
          call read_rt( sf_lun, src_num, 1, ra, dec, sf_start_sid, s )
          call read_rt( sf_lun, src_num, num_samp, ra, dec, sf_end_sid,
     *                                                               s )

C         Ensure the specified times are before the end of the sample file.
          if (start_sid.gt.sf_end_sid) start_sid= mod(start_sid,day2st)
          if (end_sid  .gt.sf_end_sid) end_sid  = mod(end_sid  ,day2st)

C         Ensure the start time is after the start of the sample file.
          if (start_sid .lt. sf_start_sid ) start_sid = start_sid+day2st
C         Ensure the stop time is after the start time.
          if (end_sid .lt. start_sid ) end_sid = end_sid+day2st

C         Maximise the overlap of the times with the sample file.
          if ((end_sid-day2st-max(start_sid-day2st,sf_start_sid)) .gt.
     *        (min(sf_end_sid, end_sid)-start_sid)               ) then
              start_sid = max( start_sid-day2st, sf_start_sid )
              end_sid   = min( end_sid  -day2st, sf_end_sid )
          else
              start_sid = max( start_sid, sf_start_sid )
              end_sid   = min( end_sid,   sf_end_sid   )
          end if

          call read_rt_sid( sf_lun, src_num, start_sid,
     *                      first_sample, ra, dec, sid, s )
          if ((s .ne. 0) .and. (s .ne. ILL_SAMPLE)) goto 9999
          s = 0

          call read_rt_sid( sf_lun, src_num, end_sid,
     *                      last_sample, ra, dec, sid, s )
          if ((s .ne. 0) .and. (s .ne. ILL_SAMPLE)) goto 9999
          s = 0

*          if (first_sample .gt. 1) then
*              call util_clrbfd( samp_list, 1, (first_sample-1) )
*          end if

          call util_setbfd( samp_list, first_sample, last_sample )

*          if (last_sample .lt. num_samp) then
*              call util_clrbfd( samp_list, (last_sample+1), num_samp )
*          end if

      else
  100     continue
              call io_getlst(    'Sample list : ',
     *                        default,
     *                        response,
     *                        list,
     *                        max_samp,
     *                        list_len,
     *                        s           )
              if ( s .ne. 0 ) goto 9999

              valid_samps = 0
              do 200, i = 1, list_len
                  if ((1.le.list(i)) .and. (list(i).le.num_samp)) then
                      valid_samps = valid_samps + 1
                  else
                      list(i) = 0
                  end if
  200         continue

              if ( valid_samps .eq. 0 ) then
                  write( 1,* ) '*** No valid samples in list'
              end if
          if ( valid_samps .eq. 0 ) goto 100

C         Set the new sample bit field.
*          call util_clrbfd( samp_list, 1, num_samp )
          do 300, i = 1, list_len
              if (list(i) .ne. 0 ) then
                  call util_setbit( samp_list, list(i) )
              end if
  300     continue
      end if

C     Finally reset current sample buffer since they are now invalid
      curr_samp  = 0
      curr_buff  = 0
      start_buff = 0

      if (s.ne.0) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          if ( s .ne. USR_BREAK ) then
              call lsf_wrerr( s, 'in subroutine LSF_add_samples' )
          end if
          return
      end
