C
C+lsf_get_radii
C
      SUBROUTINE lsf_get_radii( lsf_num,
     *                         max_r,
     *                         r_list,
     *                         num_r,
     *                         s                      )

C
C     Gets a buffer of radius coordinates.
C
C     Given:
C         Logical sample file number.
              integer             lsf_num
C         Size of radial buffer
              integer             max_r

C     Returned:
C         Buffer for radial coordinates.
              real                r_list( max_r )
C         Number of coordinates returned.
              integer             num_r
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Returns the radial coordinates of the current visibilities.
C     At the moment I am not sure what the best form of radial
C     coordinates is but I have calculated the radius of the
C     baseline vector projected onto the uv plane, for starters.
C
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/constants.inc'
      include  '/mrao/post/include/lsflib_errors.inc'
      include  '/mrao/post/include/lsf_definition.inc'
      include  '/mrao/post/include/lsf_runtime.inc'

C     ****************************************************************
C
C     Variables, equivalences and commons
C         Loop counter
              integer*4       i
C         Current hour angle of source
              real*8          ha
C         Sin hour angle times cotangent dec of source
              real*8          sha_cotd

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

      if ( sp_list_len .gt. max_r ) then
          s = ILL_BUFFSIZE
          goto 9999
      else
          num_r = sp_list_len
      end if

C     ****************************************************************
C
C         Main Code
C         ---------
C

      ha = (dfloat(samp_sid)/10.0D+0)*const_st2r + baseln_skew-epoch_ra
      sha_cotd = dsin( ha ) * dcos( epoch_dec )/dsin( epoch_dec )

      do 100, i = 1, sp_list_len
          r_list(i) = base( 1, i ) - sha_cotd * base( 3, i )
  100 continue

      if (s .ne. 0) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine LSF_GET_RADII' )
          return
      end
