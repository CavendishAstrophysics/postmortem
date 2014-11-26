C
C+map_delete_lsf
C
      SUBROUTINE map_delete_lsf ( s )

C     Deletes a logical sample file from the current map definition.
C
C     Given:
C         None.
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C
C-
C     ****************************************************************
C
C     Function definitions
C
      include        '/mrao/include/iolib_functions.inc'

C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include    '/mrao/include/constants.inc'
      include    '/mrao/include/iolib_errors.inc'
      include    '/mrao/include/maplib_redtape.inc'
      include    '/mrao/post/include/mapsys_save.inc'

C     ****************************************************************
C
C     Local variables and arrays
C         Logical sample file number
              integer         lsf_num
C         Current logical sample file in map redtape.
              integer         curr_lsf
C         Physical sample file name, and logical sample file key
              character*80    sf_name
              integer         lsf_key

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return
      call dpredt( mapsys_save, s )

C     ****************************************************************
C
C         Main Code
C         ---------
C

      if ( numlsf .ne. 1 ) then
          curr_lsf = 1

  100     continue
              call enmlsf( curr_lsf, sf_name, lsf_key, s )
              call lsf_open( sf_name, lsf_key, 'READ', lsf_num, s )
              call lsf_display( lsf_num, s )
              call lsf_close( lsf_num, s )
              if ( s .ne. 0 ) goto 9999

              if ( io_yesno( 'Delete this LSF ? ', 'No', s )) then
                  call stmlsf( curr_lsf, ' ', 0, s )
              else
                  curr_lsf = curr_lsf + 1
              end if

          if ( numlsf .ne. 1 .and. curr_lsf .le. numlsf ) goto 100
      end if

      if ( numlsf .eq. 1 ) write( 1,'(/,X,A,/)' )
     * 'You cannot delete any more LSF''s since there is only one left.'

      if (s.ne.0) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          if (s .ne. USR_BREAK) then
              call map_wrerr( s, 'in subroutine MAP_DELETE_LSF' )
          end if
          call ldredt( mapsys_save, 0 )
          return
      end
