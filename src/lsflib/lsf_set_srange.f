C
C
C+lsf_set_srange
C
      SUBROUTINE lsf_set_srange(   lsf_num,
     *                             string,
     *                             first_sample,
     *                             last_sample,
     *                             s                    )

C
C     Sets a sample range of the SF from the supplied string
C
C     Given:
C         Logical sample file number
              integer*4           lsf_num
C         String to decode
              character*(*)       string
C
C     Returned:
C         First and last samples in range
              integer*4           first_sample, last_sample
C
C     Updated:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     The sample range is decoded from the supplied string.
C     The range may be prefixed by an alphabetic code
C     with the following implications :
C
C     Code
C     'HA'    - Range is specified as hour angles
C     'ST'    - Range is specified as sidereal times
C     'PS'    - Range is specified as physical sample numbers
C     other   - Range is specified as physical sample numbers
C
C [PA, 13/8/91]
C
C-
C
C Function declarations
C
      include  '/mrao/include/chrlib_functions.inc'
C
C Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/include/constants.inc'
      include  '/mrao/post/include/lsflib_errors.inc'
      include  '/mrao/post/include/samplib_errors.inc'
      include  '/mrao/post/include/lsf_definition.inc'
      include  '/mrao/post/include/lsf_runtime.inc'

C
C Constants
C         Number of sidereal time units (10ths sec) in a sidereal day.
              integer             day2st
              parameter         ( day2st = 864000 )

C Local variables, equivalences and commons
C         General purpose integer - loop counter, string pointers
              integer             i, i1, i2, i3
C         Flags to indicate whether the range is specified as
C         hour angle, sidereal time, local time, sample or buffer number
              logical             ha, st, ut, ps, bn
C         General purpose radian R*8 time variable.
              real*8              time
C         Start and end sidereal time of LSF - as radians R*8
              real*8              start_time, stop_time
C         Offset to apply to sidereal time to get hour angle.
              real*8              ha_offset
C         Offset to apply to sidereal time to get UT (hours)
              real*8              st0_hrs
C         Start and end sidereal time of LSF - as 10ths of second.
              integer             lsf_start_sid, lsf_end_sid
C         Start and end sidereal time of range - as 10ths of second.
              integer             start_sid, end_sid
C         Parameters for read_rt_sid
              real*8              ra, dec
              integer             sid

C
C Check for non zero entry status
      if ( s .ne. 0 ) return

      if ( lsf_num .ne. curr_lsf_num ) then
          call get_lsf( lsf_num, s )
          if ( s .ne. 0 ) goto 9999
      end if

C Parse for any alphabetic prefix.
      i1 = chr_intlc(string)
      i2 = chr_lenw(string)
      i3 = chr_lenb(string)

      ha = chr_cmatch( string(i1:i2), 'HA' )
      st = chr_cmatch( string(i1:i2), 'ST' )
      ut = chr_cmatch( string(i1:i2), 'UT' )
      ps = chr_cmatch( string(i1:i2), 'PS' )
      bn = .not. ( ha .or. st .or. ut .or. ps )

      first_sample = 1
      last_sample = num_samp

C Find first and last sidereal times
      if ( ha .or. st .or. ut ) then
          call read_rt( sf_lun, src_num, first_sample,
     *                  ra, dec, lsf_start_sid, s         )
          start_time = dfloat(lsf_start_sid)/36000.0D+0
          call read_rt( sf_lun, src_num, last_sample,
     *                  ra, dec, lsf_end_sid, s           )
          stop_time  = dfloat(lsf_end_sid)/36000.0D+0
      end if
      if ( s .ne. 0 ) goto 9999

C find string indexes
      if (bn) then
        i1 = chr_intlc(string)
        i2 = chr_lenw(string)
        i3 = chr_lenb(string)
      else
        i1 = chr_lenw(string)+2
        i3 = chr_lenb(string)
        i2 = i1
        do i=1,3
          i1 = chr_lenw(string(i2:i3))+i2-1
          i2 = i1+2
        end do
        i1 = chr_lenw(string)+2
      end if

C decode string depending on prefix
      if ( ha ) then
          ha_offset = (baseln_skew - epoch_ra)/const_h2r
          time = start_time + ha_offset
          call chr_chstod(string(i1:i2),time,s)
          start_sid= int((time-ha_offset)*36000.0D+0)

          time = stop_time  + ha_offset
          call chr_chstod(string(i2:i3),time,s)
          end_sid  = int((time-ha_offset)*36000.0D+0)
      else if ( st ) then
          time = start_time
          call chr_chstod(string(i1:i2),time,s)
          start_sid = int(time*36000.0D+0)

          time = stop_time
          call chr_chstod(string(i2:i3),time,s)
          end_sid   = int(time*36000.0D+0)
      else if ( ut ) then
          call enq_mjd_st0( sf_lun, st0_hrs, s )
          st0_hrs  = (st0_hrs-int(st0_hrs))*24.0D+0
          time = dmod((st0_hrs+start_time/const_sut2sst), 24.0D+0 )
          call chr_chstod(string(i1:i2),time,s)
          start_sid = int((time+24.0-st0_hrs)*const_sut2sst*36000.0D+0)

          time = dmod((st0_hrs+stop_time/const_sut2sst), 24.0D+0 )
          call chr_chstod(string(i2:i3),time,s)
          end_sid   = int((time+24.0-st0_hrs)*const_sut2sst*36000.0D+0)
      else
          call chr_chitoc(string(i1:i2),first_sample,s)
          call chr_chitoc(string(i2:i3),last_sample,s)
      end if
      if ( s .ne. 0 ) goto 9999

C Convert times to sample numbers
      if ( ha .or. st .or. ut ) then
C .. Ensure the specified times are before the end of the LSF.
          if (start_sid.gt.lsf_end_sid) start_sid= mod(start_sid,day2st)
          if (end_sid  .gt.lsf_end_sid) end_sid  = mod(end_sid  ,day2st)

C .. Ensure the start time is after the start of the LSF
  100     if (start_sid .ge. lsf_start_sid ) goto 200
              start_sid = start_sid + day2st
          goto 100
  200     continue
C .. Ensure the stop time is after the start time
  300     if (end_sid .gt. start_sid ) goto 400
              end_sid = end_sid + day2st
          goto 300
  400     continue

C .. Maximise the overlap of the times with the LSF
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

      return

C Error Handling
 9999 continue
          if ( s .ne. USR_BREAK ) then
              call lsf_wrerr( s, 'in subroutine LSF_SET_SRANGE' )
          end if
          return
      end
