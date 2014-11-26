
C     *****************************************************************
C
C+remove_source
C
      SUBROUTINE remove_source(  rem_lun, rem_num, rem_type,
     *                           cal_lun, cal_num,
     *                           psf_lun, ion_num, psf_samp,
     *                           num_vis, vis,
     *                           ra, dec, sid,
     *                           sp_list,
     *                           base, skew,
     *                           s             )

C     Removes the given source from the given visibility buffer.

C     Given:
C         Remove file logical unit number.
              integer         rem_lun
C         Number of remove source in remove file to remove.
              integer         rem_num
C         Remove type
              integer         rem_type
C         Calibration unit number and calibration used in remove.
              integer         cal_lun, cal_num
C         Physical sample file unit and ion number and psf sample number
              integer         psf_lun, ion_num, psf_samp
C         Number of visibilities in buffer
              integer         num_vis
C         Visibility buffer
              complex         vis(num_vis)
C         Redtape of buffer - ra, dec and sidereal time.
              real*8          ra, dec
              integer         sid
C         Spacing list corresponding to vis
              integer         sp_list(num_vis)
C         Coordinates of baseline spacings
              real*8          base(3, num_vis)
C         Skew angle of baseline.
              real*8          skew

C     Returned:
C         Status value - must be zero on entry.
              integer         s

C     Removes sources using a :REM file, which must be opened
C     on unit number rem_lun. Assumes that the source visibilities are
C     in an uncalibrated form since it removes the remove calibration
C     before applying the remove.
C
C     The phase centre of the buffer remains unchanged.
C
C     The code has been modified to handle the current definition
C     of non-zero rem_type.                                    PJW 10/91
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include '/mrao/post/include/lsflib_errors.inc'
      include '/mrao/post/include/samplib_errors.inc'
      include '/mrao/post/include/global_constants.inc'

C     ****************************************************************
C
C     Local variable declarations
C         Loop counter
              integer         i
C         Current sample number in remove file
              integer         samp_num
C         Sample redtape
              integer         samp_sid
              real*8          samp_ra, samp_dec
C         Remove visibilities.
              complex         rem_vis( max_vis )
C         Ionospheric correction buffer to apply at remove phase centre.
              real            ion( 4 )

C     ****************************************************************
C
C     Subroutine initialisation
C
      if ( s .ne. 0 ) goto 9999

      if ( num_vis .gt. max_vis ) then
          s = ILL_BUFFSIZE
          goto 9999
      end if

C     Get ionospheric correction.
      if (ion_num .ne. 0) then
          call read_ion_corr( psf_lun, ion_num, psf_samp, ion, s )
      else
          ion( 1 ) = 0.0
          ion( 3 ) = 0.0
      end if

C     ****************************************************************
C
C         Main Code
C         ---------

      call read_sample_sid(   rem_lun, rem_num, sid, samp_num,
     *                        num_vis, sp_list, rem_vis, s )

      if (s .eq. ILL_SAMPLE) then
C         Sidereal time is outside scope of remove.
          s = 0
      else
          call read_rt(   rem_lun, rem_num, samp_num,
     *                    samp_ra, samp_dec, samp_sid, s )

          call phase_rot( num_vis,
     *                    rem_vis,
     *                    base,
     *                    samp_ra,  samp_dec,
     *                    ra,  dec,
     *                    sid, skew,
     *                    -ion(1),
     *                    s                 )

C         Remove calibration
          if (cal_lun.ne.0 .and. cal_num.ne.0) then
              call calibrate( cal_lun, cal_num,
     *                        num_vis, rem_vis, sp_list,
     *                        samp_sid, .false., s )
          end if

C         Remove from source visibilities
C          if (rem_type .eq. 0) then
C              do 100, i = 1, num_vis
C                  if ( vis(i) .ne. (0.0,0.0) ) then
C                      vis(i) = vis(i) - rem_vis(i)
C                  end if
C  100         continue
C          else
C              do 200, i = 1, num_vis
C                  if ( vis(i) .ne. (0.0,0.0) ) then
C                      vis(i) = vis(i) - rem_vis(i)*cmplx(ion(3),0.0)
C                  end if
C  200         continue
C          end if
C      end if
C
C     The above old code chooses rem_type zero or non-zero and
C         weights non-zero by the ionosphere correction amplitude.
C     This was probably used by Nick for 38mhz removes of CASS/CYG,
C     but is incompatible with the current definitions of removes.
C     So PJW has suppressed it. Upwards compatibility of a sort could
C     be preserved if ion weighting was used if telescope = 38mhz.    14/10/91


          do 100, i = 1, num_vis
          if ( vis(i) .ne. (0.0,0.0) ) vis(i) = vis(i) - rem_vis(i)
100       continue
      endif

      if (s .ne. 0) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine REMOVE_SOURCE' )
          return
      end
