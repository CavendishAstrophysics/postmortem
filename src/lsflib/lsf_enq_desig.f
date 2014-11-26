
C     *****************************************************************
C
C+lsf_enq_desig
C
      SUBROUTINE lsf_enq_desig( lsf_num,
     *                          index_number,
     *                          isp, iba, ich,
     *                          s                     )

C
C     Returns the spacing number, sub-band and channel numbers for
C     an element in the lsf spacing list.
C
C     Given:
C         Logical sample file number
              integer             lsf_num
C         Position in the lsf spacing list of the spacing number.
              integer             index_number
C
C     Returned:
C         Spacing number
              integer             isp
C         Sub-band number
              integer             iba
C         Channel number
              integer             ich
C
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
          isp = 0
          iba = 0
          ich = 0
      else
          call enq_vis_desig(sf_lun,sp_list(index_number),
     *                       isp,iba,ich,s)
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
