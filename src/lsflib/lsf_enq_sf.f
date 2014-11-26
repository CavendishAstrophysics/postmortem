
C     *****************************************************************
C
C+lsf_enq_sf
C
      SUBROUTINE lsf_enq_sf( lsf_num,
     *                       phys_sf_lun,
     *                       lsf_src_num,
     *                       s                    )

C
C     Returns the sample file unit number and source of a lsf.
C
C     Given:
C         Logical sample file number
              integer             lsf_num
C
C     Returned:
C         Fortran unit number of physical sample file.
              integer             phys_sf_lun
C         Source number of logical sample file.
              integer             lsf_src_num
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     This routine is primarily so that programs outside the LSFLIB
C     routines can access the physical sample file control tables
C     via the SAMPLIB enquiry routines and/or write new data
C     ionospheric corrections) to the sample file.
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
      phys_sf_lun = sf_lun
      lsf_src_num = src_num

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine LSF_ENQ_SF' )
          return
      end
