C
C+map_sel_proj
C
      SUBROUTINE map_sel_proj ( s )

C     Asks the user to select the map projection.
C
C     Given:
C         None.
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Sets the map projection parameters in the map redtape.
C
C     Mods to allow silly sampling - for round-beam tests    PJW 21/6/90
C
C-
C     ****************************************************************
C
C     Function declarations.

      include    '/mrao/include/chrlib_functions.inc'
      include    '/mrao/include/iolib_functions.inc'

C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include    '/mrao/include/constants.inc'
      include    '/mrao/include/iolib_errors.inc'
      include    '/mrao/include/maplib_redtape.inc'
      include    '/mrao/post/include/mapsys_save.inc'

C     ****************************************************************
C
C     Local variables and arrays
C         Loop counter
              integer         i
C         Default output file
              integer         out
C         Full physical sample file name.
              character*80    sf_name
C         Logical sample file key and number
              integer         lsf_key, lsf_num
C         Maximum baseline of each lsf and the maximum overall radius
              real            radius, max_rad
C         Maximum allowable sampling
              real*8          max_samp
C         Projection parameters.
              integer         pr_code
              real*8          pr_samp, pr_skew, pr_epoch,pr_angle,epoch2
C         Integer date version of epoch
              integer         iepoch(3)
C         The two different types of projection allowed and users choice
              character*10    pr_str(2), reply
              data   pr_str / 'equatorial', 'sky' /

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return
      call dpredt( mapsys_save, s )

      call io_enqout(out)

      call enmapj(pr_code, pr_samp, pr_skew, pr_epoch, pr_angle, s )

      max_rad = 0.0
      do 100, i = 1, numlsf
          call enmlsf( i, sf_name, lsf_key, s )
          call lsf_open( sf_name, lsf_key, 'READ', lsf_num, s )
          call lsf_enq_max_rad( lsf_num, radius, s )
          max_rad = max( radius, max_rad )
          call lsf_close( lsf_num, s )
          if (s .ne. 0) goto 9999
  100 continue

      max_samp = 1.0D+0 / (const_sa2r * 2.0D+0 * dfloat(max_rad) )
C     Ensure that convolution function does not go outside aperture.
      max_samp = max_samp/(1.0-2.0*dfloat(convhw+1)/amin0(iymax,ixmax))

C     ****************************************************************
C
C         Main Code
C         ---------
C
      call frddat( pr_epoch, iepoch )
      call io_getdat( 'Epoch of projection : ', '*', iepoch, s )
      if ( s .ne. 0 ) goto 9999
      if (iepoch(1).eq.1.and.iepoch(2).eq.1.and.iepoch(3).eq.1950) then
          pr_epoch = 1950.0D+0
      else
          call toddat( iepoch, epoch2 )
          if (abs(pr_epoch-epoch2) .gt. 1.0D+0/366.0D+0) pr_epoch=epoch2
      end if

  200 continue
          call io_getd( 'Sampling in U ("/gp) : ', '*', pr_samp, s )
          if (s .ne. 0) goto 9999

          if ( pr_samp .gt. max_samp ) then
            write( out, '(X,A,F6.1,A)')
     *              'Sampling should be less than ', max_samp, ' "/gp.'
            if(io_yesno('Are you absolutely sure you want this value? ',
     *                 'no', s ) ) goto 201
          end if
      if ( pr_samp .gt. max_samp ) goto 200

201   pr_code = max( 1, min( pr_code, 2 ) )
      call io_getopt( 'Projection (?=list) : ',
     *             pr_str(pr_code), pr_str, 2, reply, s)
      if ( chr_chsame( reply, pr_str(1) ) ) then
          pr_code = 1
      else
          pr_code = 2
      end if

      call io_getra( 'Map skew angle (in HMS) : ', '*', pr_skew, s )

      call stmapj( pr_code, pr_samp, pr_skew, pr_epoch, pr_angle, s )

      if ( s .ne. 0 ) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          if (s .ne. USR_BREAK) then
              call io_wrerr( s, 'in subroutine MAP_SEL_PROJ' )
          end if
          call ldredt( mapsys_save, 0 )
          return
      end
