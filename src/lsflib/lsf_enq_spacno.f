
C     *****************************************************************
C
C+lsf_enq_spacno
C
      SUBROUTINE lsf_enq_spacno( lsf_num,
     *                           index_number,
     *                           spac_number,
     *                           s                    )

C
C     Returns the spacing number of an element in the lsf spacing list.
C
C     Given:
C         Logical sample file number
              integer             lsf_num
C         Position in the lsf spacing list of the spacing number.
              integer             index_number
C
C     Returned:
C         Spacing number
              real*4              spac_number
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     This routine is needed to circumvent the problem that arises
C     because the user specifies spacing lists in terms of spacing
C     numbers but the lsf routines normally only return the current
C     positions of the visibilities in the uv plane. Thus this routine
C     returns the spacing number (which doesn't vary with time), given
C     the index in the visibility buffer that the user is interested
C     in.
C
C     NPR     6 October 1987.
C     PA      23 November 1988.
C
C-
C     ****************************************************************
C
C     Internal variables

      integer         ispac_no, iband_no, ichan_no

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
          spac_number = 0.0
      else
          call enq_vis_desig(sf_lun,sp_list(index_number),
     *                       ispac_no,iband_no,ichan_no,s)
          if (s.eq.0) then
              spac_number = ispac_no
          else
              spac_number = 0.0
          end if
      end if

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine LSF_ENQ_SPACNO' )
          return
      end
