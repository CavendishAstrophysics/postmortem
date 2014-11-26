C
C+map_sel_type
C
      SUBROUTINE map_sel_type( map_name, s )

C     Asks the user to select if the map is a beam or a map.
C
C     Given:
C         None.
C
C     Returned:
C         Current default map name - type .beam, .map, .bset as app.
              character*(*)       map_name
C         Status variable - must be zero on entry - otherwise error
              integer             s
C length of char variables increased: 20 Jan 99 GP
C-
C     ****************************************************************
C
C     Function declarations

      include    '/mrao/include/chrlib_functions.inc'

C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include    '/mrao/include/iolib_errors.inc'
      include    '/mrao/include/maplib_redtape.inc'
      include    '/mrao/post/include/mapsys_save.inc'

C     ****************************************************************
C
C     Local variables and arrays
C
C     Constants
C         Number of map types
              integer             num_types
              parameter         ( num_types = 3 )

C     Local variables, equivalences and commons
C         Separate user, name and type for map_name
              character*64        user
              character*64        file_name
              character*4         type
C         Map name length
              integer             ls
C         Valid map types and users selection
              character*(4)       map_types(num_types), reply
              data     map_types/ 'map',
     *                            'beam',
     *                            'bset'    /

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
      call io_brkfil( map_name, user, file_name, type )

      call io_getopt( 'Map type (map or beam) : ', map_types(maptyp),
     *              map_types, num_types,
     *              reply, s                   )
      if ( s .ne. 0 ) goto 9999

      if ( chr_chsame( reply, 'map' ) ) then
          maptyp = 1
          call io_makfil( user, file_name, 'map', map_name, ls )
          bsetid = ' '
      else if ( chr_chsame( reply, 'beam' ) ) then
          maptyp = 2
          call io_makfil( user, file_name, 'beam', map_name, ls )
          bsetid = ' '
      else if ( chr_chsame( reply, 'bset' ) ) then
          call map_sel_bset( s )
          if (s.eq.0) then
              call io_makfil( user, file_name, 'bset', map_name, ls )
          end if
      end if

      if (s.ne.0) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          if (s.ne.USR_BREAK) then
              call map_wrerr( s, 'in subroutine MAP_SEL_TYPE' )
          end if
          call ldredt( mapsys_save, 0 )
          return
      end
