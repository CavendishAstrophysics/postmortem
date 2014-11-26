C
C+map_sel_size
C
      SUBROUTINE map_sel_size ( s )

C     Asks the user to select the map size.
C
C     Given:
C         None.
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Sets the map size to a power of two in u and v.
C
C     mods to allow 1024 maps                            PJW 21/6/90
C
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include        '/mrao/include/iolib_errors.inc'
      include        '/mrao/post/include/mapsys_save.inc'

C     ****************************************************************
C
C     Local variables and arrays
C         Width and height of the map
              integer     num_wide, num_high
C         Maximum size for rows or columns
              integer     max_size
              parameter   ( max_size = 1024 )
C         Temporary calculation variable
              integer     temp
C         Normal uv array defining map size.
              integer     uv(4)

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
      call enredt( uv, temp, s )
      num_wide = uv(2)-uv(1)+1
      num_high = uv(3)-uv(4)+1

      call io_geti( 'Width of map  : ', '*', num_wide, s )
      call io_geti( 'Height of map : ', '*', num_high, s )
      if ( s .ne. 0 ) goto 9999

      temp = min0( max_size, max0( 2, num_high ) )
c     temp = nint( 2.0**real(nint( log2( real( temp ) ) ) ) )
      temp = nint( 2.0**real(nint( log(real(temp))/log(2.0) ) ) )
      if ( temp .ne. num_high ) then
          write( *, '(A,I5)' ) ' Height adjusted to ', temp
          num_high = temp
      end if

      temp = min0( max_size, max0( 2, num_wide ) )
c     temp = nint( 2.0**real(nint( log2( real( temp ) ) ) ) )
      temp = nint( 2.0**real(nint( log(real(temp))/log(2.0) ) ) )
      if ( temp .ne. num_wide ) then
          write( *, '(A,I5)' ) ' Width adjusted to ', temp
          num_wide = temp
      end if

      uv(1) = (uv(2)+uv(1)+1-num_wide)/2
      uv(2) = uv(1) + num_wide - 1
      uv(3) = (uv(3)+uv(4)-1+num_high)/2
      uv(4) = uv(3) - num_high + 1

      call stredt( uv, 3, s )
      if ( s .ne. 0 ) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          if (s.ne.USR_BREAK) then
              call map_wrerr( s, 'in subroutine MAP_SEL_SIZE' )
          end if
          call ldredt( mapsys_save, 0 )
          return
      end
