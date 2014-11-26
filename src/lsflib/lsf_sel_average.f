C*
C Routines for interactive editing of a logical sample file
C ---------------------------------------------------------
C
C+lsf_sel_average
C
      SUBROUTINE lsf_sel_average ( lsf_num, s )

C     Asks the user to select the lsf averaging type.
C
C     Given:
C         Logical sample file number
              integer*4           lsf_num
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Sets the averaging type for the LSF smoothing - either:
C     1.  straight boxcar averaging
C     2.  averaging scaled by the inverse of the ionispheric
C         correction amplitude.
C     3.  same as 2, except the average is weighted by the square
C         of the correction amplitude.
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
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/post/include/lsf_definition.inc'
      include  '/mrao/post/include/lsf_runtime.inc'

C     ****************************************************************
C
C     Local variables, equivalences and commons
C         Loop counter
              integer             i
C         Valid averaging types and users selection
              character*(16)      average_types(3), reply
              data average_types/ 'simple-average',
     *                            'scaled-average',
     *                            'weighted-average'    /

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

      if (smooth_type .eq. 0 .or. lsf_ion_key.eq.0) then
          call io_wrout( 'Averaging cannot be altered in current LSF' )
          return
      end if

C     ****************************************************************
C
C         Main Code
C         ---------
C

      call io_getopt( 'Averaging type : ', average_types(smooth_type),
     *              average_types, 3,
     *              reply, s                   )
      if ( s .ne. 0 ) goto 9999

      lsf_key  = 1
      lsf_name = ' '
      do 100 i = 1, 3
          if (chr_cmatch( reply, average_types(i) )) smooth_type = i
  100 continue

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          if ( s .ne. usr_break ) then
              call lsf_wrerr( s, 'in subroutine LSF_SEL_AVERAGE' )
          end if
          return
      end
