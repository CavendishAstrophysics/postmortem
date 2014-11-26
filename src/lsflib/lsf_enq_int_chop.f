
C     *****************************************************************
C
C+lsf_enq_int_chop
C
      SUBROUTINE lsf_enq_int_chop (   lsf_num,
     *                                int_chop_type,
     *                                int_chop_params,
     *                                s                      )

C
C     Enquiry routine for pre and post interference chops in the LSF.
C
C     Given:
C         Logical sample file number
              integer         lsf_num
C         Interference chop type
              integer         int_chop_type
C         Interference chop parameters
              integer         int_chop_params(10)
C         Flag set for returning pre-chop, otherwise returns post-chop.
              logical         pre_chop_flg

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer         s
C
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/post/include/lsf_definition.inc'
      include  '/mrao/post/include/lsf_runtime.inc'
      include  '/mrao/post/include/int_chop_record.inc'

C     ****************************************************************
C
C     Variables, equivalences and commons
C         Loop counter.
              integer         i

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

C     ******************************************************************
C
C         Main Code
C         ---------
C

      if ( pre_chop_flg ) then
          int_chop_type = pre_int_chop_type
          do 100, i = 1, 10
              int_chop_params(i) = pre_int_chop_params(i)
  100     continue
      else
          int_chop_type = post_int_chop_type
          do 200, i = 1, 10
              int_chop_params(i) = post_int_chop_params(i)
  200     continue
       end if

      if (s .ne. 0) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine LSF_ENQ_INT_CHOP' )
          return
      end
