C
C+cal_display
C
      SUBROUTINE cal_display ( psf_name, s )

C
C     Prints the current calibration.
C
C     Given:
C         The name of the physical sample file - must be closed.
              character*80        psf_name

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C-
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include        '/mrao/include/iolib_errors.inc'
      include        '/mrao/post/include/cal_common.inc'
      include        '/mrao/post/include/merge_types.inc'
      include        '/mrao/include/constants.inc'
      include        '/mrao/include/chrlib_functions.inc'

C
C     Local variables
C         Full name of sample file used for the calibration.
              character*64        cal_sf_name
C         Logical sample file number
              integer             lsf_num
C         Previous and current output devices
              integer             out, old_out
C         loop count
              integer             i
C         string for model position and flux output
              character*80        string
              integer             ls,ls1

C     ==================================================================
C
C     Subroutine initialisation
C     -------------------------
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

C
C     Main Code
C     ---------
C
      call io_enqout( old_out )
      call io_opeout( out, s )


      cal_sf_name = cal_sf
      call lsf_open( cal_sf_name, cal_lsf, 'READ', lsf_num, s )
      call lsf_set_pc(lsf_num, cal_refdat,
     *                 cal_ra(1), cal_dec(1), cal_source, s)
      call lsf_display( lsf_num, s )
C      call io_wrout( 'Calibration-LSF: '//lsf_name )
      call lsf_close( lsf_num, s )
      if ( s .ne. 0 ) goto 9999

      if (cal_src_type .eq. 1) then
              write(out,'(A,I6,A)') ' Cal. model     :', cal_num_point,
     *                         ' point sources'
              do i = 1,cal_num_point
                  string = ' position + flux: '
                  ls = chr_lenb( string ) + 3
                  string(ls:ls) = '-'
                  call chr_chdtos( cal_ra(i)/const_h2r,
     *                          3, string(ls:),       ls1 )
                  call chr_chdtos( cal_dec(i)/const_d2r,
     *                          3, string(ls+ls1+3:), ls1 )
                  ls = chr_lenb(string) + 3
                  write( out, '( A,F8.3)' )
     *                       string(1:ls), cal_src_flux(i)
              enddo
      else
          write(out,'(A,A,/,17X,A,F6.3)')
     *    ' Source-model   : Aperture file = ',
     *    cal_model_file(1:-1),
     *    ' Multiplying factor = ', cal_src_flux(1)
          if ( cal_ap_interp .eq. 1 ) then
                write(out,*)'                 linear interpolation'
          else
                write(out,*)'                 non-linear interpolation'
          endif
      end if

      if( .not. cal_no_band ) write(out,'(A,F10.3,A)')
     *    ' Bandwidth      : ', cal_bandwidth, ' MHz'
      if( .not. cal_no_integt ) write(out,'(A,F10.2,A)')
     *    ' Integration-time ', cal_integt, ' secs'

      if( cal_mod_mean .ne. 0. .or. cal_mod_sigma .ne. 0.) then
          write(out,*)'Noise added    :',cal_mod_mean , cal_mod_sigma
      endif

      write(out,'(A,I9)')
     *    ' Reference Ant. : ', cal_refant

      if (cal_no_pbeam) then
        call io_wrout('Primary-beam   : Correction suppressed')
      else
        call io_wrout('Primary-beam   : Correction enabled')
      end if
      if (cal_no_amp) then
        call io_wrout('Amplitude      : Correction suppressed')
      else
        call io_wrout('Amplitude      : Correction enabled')
      end if
      if (cal_no_phi) then
        call io_wrout('Phase          : Correction suppressed')
      else
        call io_wrout('Phase          : Correction enabled')
      end if

      if (cal_type.eq.3) then
        call io_wrout( 'Gains for each : visibility' )
      else
        if (cal_merge_type .eq. no_merge) then
          call io_wrout( 'Gains for each : aerial/channel' )
        else if (cal_merge_type .eq. channel_merge) then
          call io_wrout( 'Gains for each : aerial/sub-band' )
        else if (cal_merge_type .eq. subband_merge) then
          call io_wrout( 'Gains for each : aerial' )
        else if (cal_merge_type .eq. hut_merge) then
          call io_wrout( 'Gains for each : hut' )
        else if (cal_merge_type .eq. aerial_merge) then
          call io_wrout( 'Gains for each : merged-aerial' )
        end if
      end if

      call io_wrout( ' ' )

      if (out .ne. old_out) then
          call io_close( out, s )
          call io_setout( old_out )
      end if

      return

C
C     Error Handling
C     --------------
C
 9999 continue
          if ( s .ne. USR_BREAK ) then
              call cal_wrerr( s, 'in subroutine cal_display ' )
          end if
          return
      end
