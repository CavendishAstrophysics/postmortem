
C     *****************************************************************
C
C+map_sel_conv
C
      SUBROUTINE map_sel_conv ( s )

C     Asks the user to select the map convolution function.
C
C     Given:
C         None.
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Sets the current map convolution function to one of the possible
C     alternatives defined in (library)maplib-tabfns:incl.
C
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include        '/mrao/include/chrlib_functions.inc'
      include        '/mrao/include/iolib_errors.inc'
      include        '/mrao/include/maplib_redtape.inc'
      include        '/mrao/include/maplib_errors.inc'
      include        '/mrao/include/maplib_tabfns.inc'
      include        '/mrao/post/include/mapsys_save.inc'

C     ****************************************************************
C
C         Loop counter.
              integer             i
C         Users selection of convolution and correction function.
              character*(30)      reply

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

      call io_geti(   'Convolution function oversampling : ',
     *           '*', convos, s )
      call io_geti(   'Convolution function half-width   : ',
     *           '*', convhw, s )

      call io_getopt( 'Convolution function type (?=list): ',
     *              fn_types(convtp),
     *              fn_types, num_conv,
     *              reply, s                                )
      if ( s .ne. 0 ) goto 9999
      convos = max( 2, ( int( iabs(convos)/2 ) * 2 ) )

      if ( chr_chsame( reply, fn_types(prol_spher) ) ) then
          if ( convtp .ne. prol_spher ) then
              convtp    = prol_spher
              convpa(1) = 1.0
              convpa(2) = 0.0
          end if
          call io_getr( 'Alpha (0, 1, or 2) : ', '*', convpa(1), s )
          convpa(1) = amax0( 0.0,( amin0( 2.0, real(nint(convpa(1))) )))
C         Restrict halfwidth to permissible range
          convhw = amax0( 2, ( amin0( 3, convhw )))
      else if ( chr_chsame( reply, fn_types(gauss_sinc) ) ) then
          if ( convtp .ne. gauss_sinc ) then
              convtp    = gauss_sinc
              convpa(1) = 110.0/90.0
              convpa(2) = real(convhw)/sqrt(2.0*log(1.0/0.35))
          end if
          call io_getr( 'First zero of sinc (in uv) : ',
     *               '*', convpa(1), s )
          call io_getr( 'Standard deviation of gaussian (in uv) : ',
     *               '*', convpa(2),s)
      else if ( chr_chsame( reply, fn_types(l2_optimal) ) ) then
        if (convtp .ne. l2_optimal) then
            convtp = l2_optimal
            convpa(1) = 90.0
            convpa(2) = 0.0
        else
C           Convert current value from fraction to percentage
            convpa(1) = real( nint( convpa(1)*200.0 ) )
        end if

        call io_getr( 'Map width to optimise (%) : ', '*', convpa(1),s)
C       Convert back to fraction.
        convpa(1) = amax0( 50.0, amin0( 90.0, convpa(1) ) )
        convpa(1) = real( nint( convpa(1)/10.0 )) / 20.0
C       Restrict halfwidth to permissible range
        convhw = amax0( 2, ( amin0( 3, convhw )))
      else
          s = ILL_MAPFN
      end if

      call io_getopt( 'Correction fn. type (?=list) : ',
     *              corr_types(corrtp),
     *              corr_types(0), num_corr,
     *              reply, s                                )

      do 100, i = 0, num_corr-1
          if ( chr_chsame( reply, corr_types(i) ) ) corrtp = i
  100 continue

      if ( s .ne. 0 ) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          if (s.ne.usr_break) then
              call map_wrerr( s, 'in subroutine MAP_SEL_CONV' )
          end if
          call ldredt( mapsys_save, 0 )
          return
      end
