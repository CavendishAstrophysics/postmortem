

C+READ_RT_SID

      subroutine read_rt_sid ( lun,
     *                         src_num,
     *                         sid_key,
     *                         samp_num,
     *                         ra, dec,
     *                         sid_time,
     *                         s           )
C
C     Returns redtape for the next sample after a given sidereal time.
C
C     Given:
C         The logical unit number of the sample file.
              integer        lun
C         The number of the remove or calibration source to be read.
              integer        src_num
C         The indexing key for the sidereal time to be got.
              integer        sid_key
C
C     Returned:
C         The number of the sample whose redtape is returned.
              integer        samp_num
C         Coordinates of phase centre.
              real*8         ra, dec
C         Sidereal time for sample (1/10ths second)
              integer        sid_time
C         Status variable - must be zero on entry otherwise error.
              integer        s
C
C     Returns the sample number, phase centre and sidereal time
C     associated with the first sample greater than or equal to
C     a given sidereal time. If the sidereal time is before the
C     run start time or after it finished then a status of ILL_SAMPLE
C     is returned, the samp_num parameter is set to zero and an error
C     is logged. If the sidereal time is after the last sample, but
C     before the end of the run, then ILL_SAMPLE is also returned but
C     an error is not logged and the details of the last sample are
C     returned.
C
C     The file and source must be opened via OPEN_SF and OPEN_SOURCE
C     before this routine is called.
C
C         Other       - Unexpected io_system error
C
C     NPR,    July 1987
C-

C     Global includes -
C
      include '/mrao/post/include/samplib_errors.inc'
      include '/mrao/post/include/sf_pack.inc'

C     Local variable declarations -
C
C         High and low sample numbers for the weighted binary chop
              integer         high_samp, low_samp
C         High and low sidereal times
              real*8          high_sid,  low_sid

      if ( s .ne. 0 ) return

C     Check to see if the file information is in the file control block
C     - if not retrieve it.

      if ((lun .ne. sf_lun) .or. (src_num .ne. sf_src_num)) then
          call get_sf_pack( lun, src_num, s )
          if ( s .ne. 0 ) goto 999
      end if

      if (sid_key .le. start_time .or. stop_time .lt. sid_key) then
          samp_num = 0
          s = ILL_SAMPLE
          return
      else if (curr_samp .le. 0) then
C         Take a guess at what sample it will be.
          samp_num= 1 + real(num_samp-1) *
     *          real(sid_key-start_time)/(stop_time-start_time)
      else
          samp_num= curr_samp
      end if

C     Read the current redtape to find where we are.
      call read_rt( lun, src_num, samp_num, ra, dec, sid_time, s )
      if ( s .ne. 0 ) goto 999

C     We can now start a modified binary chop algorithm with the initial
C     bounds being the current sample and one of two artificial samples,
C     one being before the first sample in the file, the other being
C     after the last.
C
C     The loop invariant is :  low_samp < samp_num < high_samp
C         which must imply  :  low_sid  < sid_time < high_sid
C
C     The loop terminates if low_samp+1 = high_samp so invariant fails
C     because the inequality fails. The inequalities also ensure that
C     no attempt is made to access either of the artificial samples.

      if (sid_key .le. sid_time) then
          low_sid   = start_time
          low_samp  = 0
          high_sid  = sid_time
          high_samp = samp_num
      else
          low_sid   = sid_time
          low_samp  = curr_samp
          high_sid  = stop_time
          high_samp = num_samp+1
      end if

  10  if ( (s .ne. 0) .or. (high_samp.eq.(low_samp+1))) goto 20
          samp_num = high_samp -
     *               int(real(high_samp-low_samp)*
     *                   real(high_sid-sid_key)/(high_sid-low_sid))

C         Ensure that: low_samp < samp_num < high_samp.
          if (samp_num .eq. high_samp) samp_num = samp_num - 1

          call read_rt( lun, src_num, samp_num, ra, dec, sid_time,s)

          if ( sid_time .lt. sid_key ) then
              low_sid  = sid_time
              low_samp = samp_num
          else
              high_sid  = sid_time
              high_samp = samp_num
          end if
      goto 10
  20  continue
      if ( s .ne. 0 ) goto 999

      if (low_samp .eq. num_samp) then
          s = ILL_SAMPLE
      else if ( samp_num .ne. high_samp ) then
          samp_num = high_samp
          call read_rt( lun, src_num, samp_num, ra, dec, sid_time,s)
          if ( s .ne. 0 ) goto 999
      end if

      return

C     Error Handling -

 999  call smp_wrerr( s, 'in subroutine READ_RT_SID' )

      end
