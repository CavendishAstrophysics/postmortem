C+lsf_enq_max_rad
C
      SUBROUTINE lsf_enq_max_rad( lsf_num,
     *                            max_rad,
     *                            s                    )

C
C     Returns the maximum baseline radius of the logical sample file.
C
C     Given:
C         Logical sample file number
              integer             lsf_num
C
C     Returned:
C         Maximum baseline radius in wavelengths.
              real                max_rad
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     The radius returned is the maximum projected radius in the
C     uv plane at epoch.
C
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/post/include/lsf_definition.inc'
      include  '/mrao/post/include/lsf_runtime.inc'

C     ****************************************************************
C
C     Local variables, equivalences and commons
C         Loop counter
              integer         i
C         Cotangent of phase centre declination
              real            cotd
C         Maximum radius of current baseline
              real            radius

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
      cotd    = cos( epoch_dec ) / sin ( epoch_dec )
      max_rad = 0.0

      do 100, i = 1, sp_list_len
          radius = sqrt( base(1,i)*base(1,i)+base(2,i)*base(2,i)) +
     *             abs( cotd*base(3,i) )
          max_rad = amax0( max_rad, radius )
  100 continue

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine LSF_ENQ_MAX_RAD' )
          return
      end
