C+find_sources

      SUBROUTINE find_sources( s )

C     Interactive routine to allow the user to interrogate source lists.
C
C     Given:
C         None.

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Scans the available source lists for sources that match the
C     user selected criterion.
C
C     Possible return statuses are:
C         0           -   Success
C         other       -   Unexpected system error
C
C     NPR     5 October 1987.
C-
C     ****************************************************************
C
C     Function declarations
C
      include        '/mrao/include/chrlib_functions.inc'
      real*8          separation

C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/iolib_errors.inc'

C     ****************************************************************
C
C     Local constant and variable declarations
C

C     Variables, equivalences and commons
C         Catalogue identifier
              integer         cd
C         General purpose string
              character*80    string
C         Logical unit number of source list file.
              integer         lun
C         Logical unit numbers of output file and previous output file.
              integer         out, old_out
C         File name of source file
              character*80    file_name
C         Ra, dec and radius of search.
              real*8          ra_cent, dec_cent, radius
C         Maximum and minimum flux of search
              real*8          max_flux, min_flux
C         Name of source read from the file
              character*(20)  source
C         RA hours, min and sec, Dec deg, min and sec.
              integer         ra_hrs, ra_min, dec_deg, dec_min
              real            ra_sec, dec_sec
C         RA, dec (radians) and flux of source
              real*8          ra, dec, flux
C         Flag for first source found
              logical         first

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return
      call io_enqout( old_out )

C     ****************************************************************
C
C         Main Code
C         ---------
C
      file_name = 'SOURCE-LIST:SYMB'
      call io_getfil( 'Source list : ', '*', file_name, s )
      if ( s .ne. 0 ) goto 999

      call io_getra(  '1950.0 RA  of search centre : ', '12 0 0',
     *             ra_cent, s )
      call io_getdec( '1950.0 DEC of search centre : ', '90 0 0',
     *             dec_cent, s )
      call io_getdec( 'Search radius (DMS) : ', '5 0 0', radius, s )
      call io_getd( 'Minimum flux (Jy) : ', '0', min_flux, s )
      call io_getd( 'Maximum flux (Jy) : ', '100000', max_flux, s )
      call io_opeout( out, s )
      if ( s .ne. 0 ) then
          close (lun)
          goto 999
      end if

C open the catalogue
      call io_opefil( cd, file_name, 'READ', 0, s )

      first = .true.
      do while (s.eq.0)

C .. read next record
        read( cd, 1000, iostat = s ) source,
     *                               ra_hrs, ra_min, ra_sec,
     *                               dec_deg, dec_min, dec_sec,
     *                               flux

        if (s.eq.0) then
          call torads( 'HMS', ra_hrs, ra_min, ra_sec, ra )
          call torads( 'DMS', dec_deg, dec_min, dec_sec, dec )

          if ((separation(ra_cent,dec_cent,ra,dec).lt.radius) .and.
     *        (flux .ge. min_flux) .and. (flux .le. max_flux) ) then
             write(string,1001)  source,
     *                           ra_hrs,  ra_min,  ra_sec,
     *                           dec_deg, dec_min, dec_sec,
     *                           flux
             if (first) write(out,*)
             write(out,*) string
             first = .false.
          end if

        end if
      end do

C close the catalogue
      close (cd)
      if ( s .eq. END_FILE) s = 0

      if (out .ne. old_out) then
          call io_close( out, s )
          call io_setout( old_out )
      end if
      call io_wrout(' ')

      return

C     Format statement
 1000 format( 1X, A16, 2( 2I3, F7.3, 1X ), F10.4 )
 1001 format( A15, 2( 2X, 2I3, F7.3 ), 2(3X,F10.4) )

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 999  continue
          if (s .ne. USR_BREAK) then
              call smp_wrerr( s, 'in subroutine FIND_SOURCES' )
          end if
          return
      end
