
C     *****************************************************************
C
C+lsf_get_uv
C
      SUBROUTINE lsf_get_uv( lsf_num,
     *                       max_uv,
     *                       skew,
     *                       uv_list,
     *                       num_uv,
     *                       s                      )

C
C     Gets a buffer of uv positions.
C
C     Given:
C         Logical sample file number.
              integer             lsf_num
C         Size of uv buffer
              integer             max_uv
C         UV plane skew angle - measured in the same sense as the map
C         redtape skew angle.
              real*8              skew

C     Returned:
C         Buffer for uv coordinates.
              real                uv_list( 2, max_uv )
C         Number of coordinates returned.
              integer             num_uv
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Returns the uv coordinates of the current visibilities.
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
C         Sin and cos hour angle.
              real*8          sh, ch
C         Cotangent declination.
              real*8          cotd
C         Cotangent declination times sine and cosine skew-angle
              real*8          cotdss, cotdcs

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

      if ( sp_list_len .gt. max_uv ) then
          s = ILL_BUFFSIZE
          goto 9999
      else
          num_uv = sp_list_len
      end if

C     ****************************************************************
C
C         Main Code
C         ---------
C

      ha     = (dfloat(samp_sid)/10.0D+0)*const_st2r
     *         + baseln_skew - skew - epoch_ra
      sh     = dsin( ha )
      ch     = dcos( ha )
      cotd   = dcos(epoch_dec) / dsin(epoch_dec)
      cotdss = cotd * dsin( skew )
      cotdcs = cotd * dcos( skew )

      do 100, i = 1, sp_list_len
          uv_list(1,i) = base(2,i)*sh - base(1,i)*ch + base(3,i)*cotdss
          uv_list(2,i) = base(2,i)*ch + base(1,i)*sh - base(3,i)*cotdcs
  100 continue

      if (s .ne. 0) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine LSF_GET_UV' )
          return
      end
