
C     *****************************************************************
C
C+lsf_set_pbeam
C
      SUBROUTINE lsf_set_pbeam( lsf_num, pbeam_flg, s )

C     Defines whether to include the primary beam in the LSF model.
C
C     Given:
C         Logical sample file number
              integer*4           lsf_num
C         Logical set .true. to set the primary beam on (default value)
              logical             pbeam_flg
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     NPR     6 April 1988.
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/post/include/lsflib_errors.inc'
      include  '/mrao/post/include/lsf_runtime.inc'
      include  '/mrao/post/include/lsf_model.inc'

C     ****************************************************************
C
C     Local variables
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

      if (sf_type .ne. 1) then
C         Not a physical sample file.
          call io_wrout('sample file of unexpected type.')
c          return
      end if

C     ****************************************************************
C
C         Main Code
C         ---------
C
C     Set up runtime common blocks.
      mod_pb_flg = pbeam_flg

      if (s .ne. 0) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine LSF_SET_PBEAM' )
          return
      end
