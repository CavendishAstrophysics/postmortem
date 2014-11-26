C+READ_SAMPLE_SID

      subroutine read_sample_sid ( lun,
     *                             src_num,
     *                             sid_key,
     *                             samp_num,
     *                             num_vis,
     *                             vis_list,
     *                             vis_buff,
     *                             s          )
C
C     Returns interpolated visibility data at the given sidereal time.
C
C     Given:
C         The logical unit number of the sample file.
              integer         lun
C         The number of the source to be read.
              integer         src_num
C         The sidereal time sample for the sample to return.
              integer         sid_key
C         The number of visibilities to be returned.
              integer         num_vis
C         A list of index numbers for the required visibilities.
              integer         vis_list ( num_vis )
C
C     Returned:
C         Array of visibilities, (cos,sin) pairs.
              complex         vis_buff ( num_vis )
C         The sample number after the given sidereal time
              integer         samp_num
C         Status value - must be zero on entry.
              integer         s

C     The data is interpolated between samples to give the predicted
C     visibilities at the specified sidereal time. The interpolation
C     type is defined in the control tables. The processing flag
C     parameter that is used in read_sample is irrelevant since the
C     visibilities are always returned in external units.
C
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include '/mrao/post/include/samplib_errors.inc'
      include '/mrao/post/include/global_constants.inc'
      include '/mrao/post/include/sf_pack.inc'
      include '/mrao/include/constants.inc'

C     ****************************************************************
C
C     Local variable declarations
C         Loop counter
              integer         i
C         Another sample number.
              integer         samp_2
C         Two samples are read from the sample file to calculate
C         the actual flux to return. This requires another buffer ...
              complex         vis_buff2( max_vis )
C         ...weights for each buffer...
              real            buff_wt , buff2_wt
C         ...amplitudes and phases...
              real            ramp, rphi
              real            ramp_1, rphi_1, ramp_2, rphi_2
C         ... and two lots of redtape
              integer         sid_1, sid_2
              real*8          ra_1, dec_1, ra_2, dec_2

C     ****************************************************************
C
C     Subroutine initialisation
C
      if ( s .ne. 0 ) goto 999

      if ( num_vis .gt. max_vis ) then
          s = NO_SFBUFFER
          goto 999
      end if

C     ****************************************************************
C
C         Main Code
C         ---------

      call read_rt_sid(   lun, src_num, sid_key,
     *                    samp_num, ra_1, dec_1, sid_1, s )
C     If the sidereal time is out of range set the buffer to zero and
C     return.
      if ( s .eq. ILL_SAMPLE .and.
     *     (samp_num.eq.0 .or. interp_type.ne.2)) then
          do 10, i = 1, num_vis
              vis_buff(i) = (0.0,0.0)
  10      continue
          goto 999
c         return
      else
C         Can interpolate after the last sample to the end of the run
          s = 0
      end if

      call read_sample(   lun, src_num, samp_num, 1,
     *                    num_vis, vis_list, vis_buff, s)

      if ( interp_type.eq.2 .and. num_samp.ne.1 ) then
C         Get another sample and interpolate between the two.
          if ( samp_num .eq. 1 ) then
              samp_2 = 2
          else
              samp_2 = samp_num - 1
          end if

          call read_rt( lun, src_num, samp_2, ra_2, dec_2, sid_2, s )
          call read_sample( lun, src_num, samp_2, 1,
     *                      num_vis, vis_list, vis_buff2, s)
          if ( s .ne. 0 ) goto 999

C         Interpolate - at the moment linear over two samples.
          buff_wt  = real(sid_2-sid_key) / real(sid_2-sid_1)
          buff2_wt = real(sid_key-sid_1) / real(sid_2-sid_1)

C         Interpolate amplitude and phase for calibration files
          if (sf_type.eq.3) then
C	write(*,*)samp_num,samp_2,vis_buff(1),vis_buff2(1)
             do 20, i = 1, num_vis
               rphi_1 = 0.0
               rphi_2 = 0.0
               ramp_1 = cabs( vis_buff(i) )
               ramp_2 = cabs( vis_buff2(i) )
               if (ramp_1.ne.0.0) rphi_1 =
     *                  atan2( aimag(vis_buff(i)), real(vis_buff(i)) )
               if (ramp_2.ne.0.0) rphi_2 =
     *                  atan2( aimag(vis_buff2(i)), real(vis_buff2(i)) )
               if (abs(rphi_2-rphi_1).gt.const_pi)
     *            rphi_2 = rphi_2 - sign(const_2pi,rphi_2)
               ramp = ramp_1*buff_wt + ramp_2*buff2_wt
               rphi = rphi_1*buff_wt + rphi_2*buff2_wt
               vis_buff(i) = ramp * cmplx( cos(rphi), sin(rphi) )
  20         continue
C         Interpolate cos and sine for sample/remove files
          else
             do 21, i = 1, num_vis
               vis_buff(i) = vis_buff(i)*buff_wt + vis_buff2(i)*buff2_wt
  21         continue
          end if
      end if

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 999  call smp_wrerr( s, 'in subroutine READ_SAMPLE_SID' )

      end


