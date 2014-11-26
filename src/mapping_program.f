      program mapping_program

C     Program to make maps, invoked from POSTMORTEM.
C
C     NPR     17 November 1987.
C     DJT     11 August   1992: Unix implementation.
C
C     ******************************************************************

      character*6     process_name
      character*4     file_type
      parameter (process_name = 'MAPPER', file_type = 'map' )

      include '/mrao/include/iolib_errors.inc'
      include '/mrao/include/chrlib_functions.inc'
      include '/mrao/post/include/post_common.inc'

      logical         finished, term_input
      character*80    file_name, error_message
      character*33    map_dir
      character*16    user
      integer         s, mode, termno, ls

C     ****************************************************************
C
C     Program initialisation
C
      s = 0
      call io_initio
      call io_setesc( .true. )
      call io_setexc( .true., s )
      call io_setlog( '/mrao/post/post_error.elog', s )

      error_message = ' in MAPPING_PROGRAM'

      call io_enqexe( user, mode, termno )
      call io_enqcli( file_name, ls )
      term_input = mode.eq.0 .or. ls.gt.0

      call getenv( 'SAMPDIR', def_dir )

      if ( s .ne. 0 ) goto 9999

C
C Main Code
C ---------
C
      if ( term_input ) then
          call enmdir( map_dir, s )
          call io_makfil( map_dir, ' ', file_type, file_name, ls )
          call io_getfil( 'Map file name : ', '*', file_name, s )

          finished = (s .eq. USR_BREAK)

      end if


  100 if ( finished .or. s .ne. 0 ) goto 200
          error_message = process_name//' program with '//file_name

  150     continue
              s = 0
              call io_wrout( file_name )
              call mapper( file_name, s )

              if ( s .ne. 0 ) call io_wrerr( s, error_message )
          if (s .eq. 110) goto 150
          s = 0

  200 continue

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 if ( s .ne. 0 ) then
          call io_wrerr( s, error_message )
      end if

      call io_setesc( .false. )

      end

