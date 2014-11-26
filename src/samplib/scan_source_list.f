C+scan_source_list

      subroutine scan_source_list  (  source_name,
     *                                source_list,
     *                                full_source_name,
     *                                ra, dec,
     *                                s                      )

C
C     Scans the available source list(s) for a given source.
C
C     Given:
C         Source name
              character*(*)       source_name
C         Source list to scan
              character*(*)       source_list
C
C     Returned:
C         Full source name
              character*(*)       full_source_name
C         Ra and dec of the source.
              real*8              ra, dec
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Scans the given source list for a source matching the given source
C     name. If it is found then the full source and the 1950.0 ra and
C     dec returned.  Source_name and full_source_name can be the
C     same variable.
C
C     Possible return statuses are:
C         0           -   Source found
C         NO_SOURCE   -   No source found
C         other       -   Unexpected system error
C
C     NPR     10 June 1987.
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include '/mrao/post/include/samplib_errors.inc'
      include '/mrao/include/chrlib_functions.inc'
      include '/mrao/include/iolib_functions.inc'
      include '/mrao/include/iolib_errors.inc'

C     ****************************************************************
C
C     Local variables, equivalences and commons
C         Flag indicating whether source has been found or not
              logical         found
C         Name of source
              character*80    source
C         String for prompting
              character*80    string
              integer         len_string
C         Catalogue descriptor
              integer         cd
C         RA hours, min and sec, Dec deg, min and sec.
              integer         ra_hrs, ra_min, dec_deg, dec_min
              real            ra_sec, dec_sec

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return


C     ****************************************************************
C
C         Main Code
C         ---------

C open the catalogue
      call io_opefil ( cd, source_list, 'READ', 0, s )
      found = .false.

      do while (s.eq.0 .and. .not.found)

C .. read next record
        read( cd, 1000, iostat = s ) source,
     :                               ra_hrs, ra_min, ra_sec,
     :                               dec_deg, dec_min, dec_sec

        if (s.eq.0) then

            if ( chr_cmatch( source_name, source ) ) then
              string = 'select '//source(1:chr_lenb(source))//' ? '
              len_string = chr_lenb(string) + 1
              if (io_yesno(string(1:len_string),'yes',s)) then
                found = .true.
                full_source_name = source
                call torads ( 'HMS', ra_hrs, ra_min, ra_sec, ra, s )
                call torads ( 'DMS', dec_deg, dec_min, dec_sec, dec, s )
              end if
            end if

        end if
      end do

C close the catalogue
      close ( cd )
      if ( s .eq. -1) s = END_FILE
      if ( s .eq. END_FILE) s = 0
      if ( s .ne. 0 ) goto 9999

      if ( .not. found ) then
          s = NO_SOURCE
          full_source_name = ' '
          ra  = 0.0D+0
          dec = 0.0D+0
      end if

 1000 format( 1X, A16, 2( 2I3, F7.3, 1X ), F10.4 )

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call smp_wrerr( s, 'in subroutine SCAN_SOURCE_LIST' )
          return
      end
