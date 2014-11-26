
C     *****************************************************************
C
C+map_sel_grading
C
      SUBROUTINE map_sel_grading ( s )

C     Asks the user to select the map grading function.
C
C     Given:
C         None.
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Sets the current map grading function to one of the possible
C     alternatives defined by the subroutine grading_fn.
C
C     If a 'round beam' is required 100 is added to the grading-type
C                                                          PJW  25/6/90
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include        '/mrao/include/iolib_errors.inc'
      include        '/mrao/include/iolib_functions.inc'
      include        '/mrao/include/chrlib_functions.inc'
      include        '/mrao/include/maplib_redtape.inc'
      include        '/mrao/post/include/mapsys_save.inc'
      include        '/mrao/post/include/grading_types.inc'

C     ****************************************************************

C     Local variables, equivilances and commons
C         Loop counter
              integer             i
C         Users selection of grading type.
              character*(20)      reply
C         Previous grading type.
              character*(20)      prev_type

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

      prev_type = grad_types(gradtp)
      call io_getopt( 'Grading fn. type (?=list) : ', prev_type,
     *              grad_types(0), num_grad+1,
     *              reply, s      )
      if ( s .ne. 0 ) goto 9999
      call chr_chucas( prev_type )
      call chr_chucas( reply )

C     Convert gradpa(1) to a percentage
      gradpa(1) = 100.0*gradpa(1)

      if ( index( reply, 'GAUSSIAN' ) .ne. 0 ) then
        if ( index( prev_type, 'GAUSSIAN' ) .eq. 0 ) then
            gradpa(1) = 100.0/(1.5*sqrt(2.0*log(1.0/0.30)))
        end if
        call io_getr( 'S.D. of gaussian (% of aperture radius) : ',
     *               '*', gradpa(1), s)
      else if ( chr_chsame(reply,grad_types(psw2_gr)).or.
     *        chr_chsame(reply,grad_types(psw3_gr))     ) then
        if (gradtp.ne.psw2_gr.and.gradtp.ne.psw3_gr) then
            gradpa(1) = 65.0
            gradpa(2) = 1.0
        end if

        call io_getr( 'Alpha (0, 1, or 2) : ', '*', gradpa(2), s )
        call io_getr('Cutoff radius (% of aperture) : ',
     *              '*', gradpa(1),s)
      else if ( chr_chsame(reply,grad_types(l2w2_gr)).or.
     *        chr_chsame(reply,grad_types(l2w3_gr))     ) then
        if (gradtp.ne.l2w2_gr.and.gradtp.ne.l2w3_gr) then
            gradpa(1) = 65.0
            gradpa(2) = 0.45
        end if

C       Convert to fractional increase in size
        gradpa(2)= 1.0/gradpa(2)
        call io_getr( 'Relative point source size : ',
     *               '*', gradpa(2),s)

C       Convert back to fraction.
        gradpa(2) = amax1( 1.0/.45, amin1( 4.0, gradpa(2) ) )
        gradpa(2) = real(nint(20.0/gradpa(2)))/20.0
        call io_getr('Cutoff radius (% of aperture) : ',
     *              '*', gradpa(1),s)
      else if ( chr_chsame(reply,grad_types(optimal_gr)) ) then
        if (gradtp.ne.optimal_gr) then
            gradpa(1) = 65.0
            gradpa(2) = 5
        end if

        call io_getr( 'Relative point source size : ', '*', gradpa(2),s)
        gradpa(2) = max(abs(gradpa(2)), 0.5)

C       Convert back to fraction.
        call io_getr('Cutoff radius (% of aperture) : ',
     *              '*', gradpa(1),s)
      else
          gradpa(1) = 0.0
          gradpa(2) = 0.0
      end if
      gradpa(1) = gradpa(1)/100.0

C     Set the grading type.
      do 100, i = 0, num_grad
          if (chr_chsame( grad_types(i), reply )) gradtp = i
  100 continue

C     Round beams
      if( io_yesno( 'Round beam? ', 'NO', s ) )  then
          gradtp = gradtp + 100
      endif

      if (s.ne.0) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          if (s .ne. USR_BREAK) then
              call map_wrerr( s, 'in subroutine MAP_SEL_GRADING' )
          end if
          call ldredt( mapsys_save, 0 )
          return
      end
