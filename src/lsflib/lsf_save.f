C
C+lsf_save
C
      SUBROUTINE lsf_save ( lsf_num,
     *                      lsf_file_key,
     *                      s                      )

C
C     Saves a logical sample file.
C
C     Given:
C         Logical sample file number
              integer             lsf_num
C
C     Returned:
C         The file key given to the lsf.
              integer             lsf_file_key
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Saves the logical sample file if the sample file is a physical
C     sample file. Otherwise the logical sample file cannot be saved
C     because of the possibility of name ambiguities.
C
C     If the sample file is not a physical sample file then the
C     logical sample file is not saved, but a zero status is returned
C     after a message has been written to the console.
C
C     If successful the LSF file is put on the tape save list.
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
C         Sample file type
              integer         test_sf_type

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

C     Check to ensure that the sample file is a physical sample file
C     directory - otherwise the logical sample file cannot be saved.

      call enq_sftype( sf_lun, test_sf_type, s )
      if ( test_sf_type.ne.1 ) then
          print *, 'Sample file of expected type '
C          return
      end if

C     ****************************************************************
C
C         Main Code
C         ---------
C

      lsf_name = ' '
      call io_getstr( 'Enter description of lsf : ', '', lsf_name, s )
      if ( s .ne. 0 ) goto 9999

  200 if (lsf_name .ne. ' ') goto 300
        print *,'*** description must not be null'
        call io_getstr( 'Enter description of lsf : ', '', lsf_name, s )
        if ( s .ne. 0 ) goto 9999
      goto 200
  300 continue

      lsf_file_key = 0
      call lsf_write_def( sf_lun, lsf_file_key, log_samp_file, s )
      if (s .ne. 0) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          if ( s .ne. USR_BREAK ) then
              call lsf_wrerr( s, 'in subroutine LSF_SAVE' )
          end if
          return
      end
