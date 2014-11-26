
C     *****************************************************************
C
C+map_sel_weight
C
      SUBROUTINE map_sel_weight ( s )

C     Asks the user to select the map visibility weighting function.
C
C     Given:
C         None.
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Sets the map weighting function to one of the possible
C     alternatives defined in (postmortem)weighting-types:incl
C
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include        '/mrao/include/chrlib_functions.inc'
      include        '/mrao/include/iolib_functions.inc'
      include        '/mrao/include/iolib_errors.inc'
      include        '/mrao/include/maplib_redtape.inc'
      include        '/mrao/include/maplib_errors.inc'
      include        '/mrao/post/include/mapsys_save.inc'
      include        '/mrao/post/include/weighting_types.inc'

C     ****************************************************************
C
C     Local variables, equivilances and commons
C         Loop counter
              integer             i
C         Users selection of weighting type, and general pupose string
              character*20        reply
              character*2         string
C         Weighting function parameters.
              real                st_dev, cutoff

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
      call io_getopt( 'Weighting function type (?=list) : ',
     *              weight_types(mod(wghttp,10)),
     *              weight_types(0), num_weight+1,
     *              reply, s                      )
      if ( s .ne. 0 ) goto 9999

      if ( chr_chsame( reply, weight_types(no_weighting) )) then
          st_dev = 0.0
      else if ( chr_chsame( reply, weight_types(super_smooth)) .or.
     *          chr_chsame( reply, weight_types(noise_wt    ))) then
          if ((mod(wghttp,10) .eq. super_smooth) .or.
     *        (mod(wghttp,10) .eq. noise_wt    )) then
              st_dev = wghtpa(1)
          else
              st_dev = 0.0
          end if
          write(string,'(I2)') nint(st_dev*2+1)
          call io_getr('Smooth box width : ', string, st_dev, s )
          st_dev = max(0.0, min(3.0, int( abs( (st_dev-1)/2 ) )))
      else if ( chr_chsame( reply, weight_types(radial_wt) )) then
          st_dev = 0.0
      else if ( chr_chsame(reply,weight_types(gaussian_wt))     .or.
     *          chr_chsame(reply,weight_types(radial_gauss_wt))) then

          if (mod(wghttp,10).ne.gaussian_wt  .and.
     *        mod(wghttp,10).ne.radial_gauss_wt ) then
              st_dev = 1.0/(1.5*sqrt(2.0*log(1.0/0.30)))
          else
              st_dev = wghtpa(1)
          end if

          call io_getr(
     *        'S.D. of gaussian (ratio of the aperture halfwidth) : ',
     *        '*', st_dev, s)
      else
          s = ILL_WEIGHT
          goto 9999
      end if

      if (io_yesno( 'Apply a visibility cutoff ? ', 'No', s )) then
          cutoff = wghtpa(2)
          call io_getr(
     *        'Radius of cutoff (ratio of the aperture halfwidth) : ',
     *        '*', cutoff, s )
      else
          cutoff = 0.0
      end if
      if ( s .ne. 0 ) goto 9999

C     Set up redtape parameters
      do 100, i = 0, num_weight
          if (chr_chsame( reply, weight_types(i) ) ) wghttp = i
  100 continue
      wghtpa(1) = st_dev
      wghtpa(2) = abs(cutoff)
      if (wghtpa(2) .ne. 0.0) wghttp = wghttp+10

      if (s.ne.0) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          if (s .ne. usr_break) then
              call map_wrerr( s, 'in subroutine MAP_SEL_WEIGHT' )
          end if
          call ldredt( mapsys_save, 0 )
          return
      end
