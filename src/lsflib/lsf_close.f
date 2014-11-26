C
C+lsf_close
C
      SUBROUTINE lsf_close( lsf_num,
     *                      s                      )

C
C     Closes a logical sample file.
C
C     Given:
C         Logical sample file number
              integer         lsf_num

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer         s
C
C     Closes all files associated with the given logical sample file
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
      call close_sf( sf_lun, s )

      if ( rf_lun .ne. 0 ) call close_sf( rf_lun, s )
      if ( cf_lun .ne. 0 ) call close_sf( cf_lun, s )

      sf_lun = 0
      rf_lun = 0
      cf_lun = 0

      if ( s .ne. 0 ) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine LSF_CLOSE' )
          return
      end
