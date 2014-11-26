
C     *****************************************************************
C
C+lsf_rot_vis
C
      SUBROUTINE lsf_rot_vis( lsf_num,
     *                        num_vis,
     *                        vis,
     *                        ra, dec,
     *                        s                      )

C
C     Phase rotates the current vis. buffer to a new position.
C
C     Given:
C         Logical sample file number.
              integer             lsf_num
C         Size of the visibility buffer.
              integer             num_vis
C         Visibility buffer
              complex             vis( num_vis )
C         New RA and Dec.
              real*8              ra, dec

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Phase rotates the current visibility buffer, supplied by the
C     calling routine to a new RA and Dec. Assumes the buffer has
C     essentially not been changed since the last call to lsf_get_vis
C     or a previous call to this routine. Thus the geometry, number
C     of spacings, phase centre and sidereal time are still valid.
C
C     NPR     16 October 1987.
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/post/include/lsflib_errors.inc'
      include  '/mrao/post/include/lsf_runtime.inc'

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

      if ( sp_list_len .ne. num_vis ) then
          s = ILL_BUFFSIZE
          goto 9999
      end if

C     ****************************************************************
C
C         Main Code
C         ---------
C
      call phase_rot( sp_list_len,
     *                vis,
     *                base,
     *                raw_samp_ra,  raw_samp_dec,
     *                ra, dec,
     *                samp_sid, baseln_skew,
     *                0.0,
     *                s                 )
      raw_samp_ra  = ra
      raw_samp_dec = dec

      if (s .ne. 0) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine LSF_ROT_VIS' )
          return
      end
