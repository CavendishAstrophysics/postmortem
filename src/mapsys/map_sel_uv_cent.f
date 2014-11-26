
C     *****************************************************************
C
C+map_sel_uv_cent
C
      SUBROUTINE map_sel_uv_cent ( s )

C     Asks the user to select the u,v coordinate of the map centre.
C
C     Given:
C         None.
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     NPR.    22 September 1987.
C
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include    '/mrao/include/constants.inc'
      include    '/mrao/include/iolib_errors.inc'
      include    '/mrao/include/maplib_errors.inc'
      include    '/mrao/include/maplib_redtape.inc'
      include    '/mrao/post/include/mapsys_save.inc'

C     ****************************************************************
C
C     Local variables and arrays
C         U and V coordinates of map centre.
              integer         u, v
C         Standard map window uv array
              integer         uv(4)
C         Ra and dec of map phase centre at epoch.
              real*8          ra, dec
C         String and string lengths
              character*80    string
              integer         ls, ls1
C         Default output device
              integer         out
C         Internal map data type
              integer         data_type

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return
      call dpredt( mapsys_save, s )

      call io_enqout( out )

C     ****************************************************************
C
C         Main Code
C         ---------
C
      call enredt( uv, data_type, s )
      u = (uv(2)+uv(1)+1)/2
      v = (uv(3)+uv(4)-1)/2
      call io_geti( 'U-coordinate of map centre : ', '*', u, s )
      call io_geti( 'V-coordinate of map centre : ', '*', v, s )
      if ( s .ne. 0 ) goto 9999

      call stproj( iproj,1,usamp,skew,ramap,decmap,refdat,epoch,s)
      call uvtord ( dfloat(u), dfloat(v), ra, dec, s)

      if ( s .eq. 0) then
C       convert u and v to a coordinate shift.
        u = u - (uv(2)+uv(1)+1)/2
        v = v - (uv(3)+uv(4)-1)/2

        if (maptyp .eq.3) then
C           Beamset - adjust beamset origin
            u0set = u0set + u
            v0set = v0set + v
        end if

        uv(1) = uv(1) + u
        uv(2) = uv(2) + u
        uv(3) = uv(3) + v
        uv(4) = uv(4) + v
        call stredt( uv, 3, s )

        call precrd( epoch, ra, dec, refdat, rampc, decmpc )
        string = ' Map phase centre is '
        ls = 22
        call chr_chdtos( rampc/const_h2r,  0, string(ls:), ls1 )
        ls = ls+ls1+3
        call chr_chdtos( decmpc/const_d2r, 0, string(ls:), ls1 )
        write( out, '(2A,F6.1,A)' ) string(1:ls+ls1+1), '(',refdat,')'
      else if (s .eq. UV_UNREAL) then
        call io_wrout('That UV point has no corresponding ra and dec.' )
      end if

      if ( s .ne. 0 ) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          if ( s .ne. usr_break ) then
              call map_wrerr( s, 'in subroutine MAP_SEL_UV_CENT' )
          end if
          call ldredt( mapsys_save, 0 )
          return
      end
