
C     *****************************************************************
C
C+lsf_enq_ae_vis
C
      SUBROUTINE lsf_enq_ae_vis( lsf_num,
     *                          index_number,
     *                          iae1, iae2,
     *                          s                     )

C
C     Returns the aerials for an element in the lsf spacing list
C
C     Given:
C         Logical sample file number
              integer             lsf_num
C         Position in the lsf spacing list of the spacing number.
              integer             index_number
C
C     Returned:
C         Aerial designations (East, West)
              integer             iae1, iae2
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     PA      1 March 1989
C
C-

C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
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
      if (index_number .gt. sp_list_len .or. index_number .lt. 1 ) then
          iae1 = 0
          iae2 = 0
      else
          call enq_ae_vis(sf_lun,sp_list(index_number),iae1,iae2,s)
      end if

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine LSF_ENQ_DESIG' )
          return
      end
