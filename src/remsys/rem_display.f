C
C+rem_display
C
      SUBROUTINE rem_display ( psf_name, s )

C
C     Prints the current remove.
C
C     Given:
C         The name of the physical sample file - must be closed.
              character*80        psf_name

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include        '/mrao/include/iolib_errors.inc'
      include        '/mrao/post/include/remove_common.inc'
      include        '/mrao/include/constants.inc'
      include        '/mrao/include/chrlib_functions.inc'

C     ****************************************************************
C
C     Local variables
C         Logical sample file number
              integer             lsf_num
C         Previous and current output devices
              integer             out, old_out
C         loop count
              integer             i
C         working char string and length
              character*80        string
              integer             ls,ls1



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
C
      call io_enqout( old_out )
      call io_opeout( out, s )

      call lsf_open( psf_name, rem_lsf, 'READ', lsf_num, s )
      call lsf_set_pc(lsf_num, rem_refdat,
     *                 rem_ra(1),rem_dec(1), rem_source, s)
      call lsf_display( lsf_num, s )
      if( rem_type .eq. 1 ) then
          call io_wrout( 'Normal remove' )
      elseif( rem_type .eq. 2 ) then
          if( rem_src_type .eq. 1 ) then
              write(out,'(A,I6,A)') ' Remove model   :', rem_num_point,
     *                         ' point sources'
              do i = 1,rem_num_point
                  string = ' position + flux: '
                  ls = chr_lenb( string ) + 3
                  string(ls:ls) = '-'
                  call chr_chdtos( rem_ra(i)/const_h2r,
     *                          3, string(ls:),       ls1 )
                  call chr_chdtos( rem_dec(i)/const_d2r,
     *                          3, string(ls+ls1+3:), ls1 )
                  ls = chr_lenb(string) + 3
                  write( out, '( A,F8.3)' )
     *                       string(1:ls), rem_src_flux(i)
              enddo
          elseif( rem_src_type .eq. 2 ) then
              write(out,'(2A)')' Remove model aperture from: ',
     *                     rem_model_file(1:-1)
              write(out,'(a,f10.2)')
     *         '                   which will be multiplied by',
     *                    rem_src_flux(1)
              if( rem_ap_interp .eq. 1 ) then
                    write(out,*)'                  linear interpolation'
              else
                    write(out,*)'                  non-linear interp.'
              endif
          endif
          If(rem_no_pbeam) then
                    write(out,*)'Primary-beam   : Correction suppressed'
          else
                    write(out,*)'Primary-beam   : correction enabled'
          endif
          if(rem_no_band) then
              call io_wrout(
     *        '                  with bandwidth correction suppressed' )
          else
            write(out,'(a,f10.3,a)')
     *        '                  with bandwidth corrected for',
     *                    rem_bandwidth, ' MHz'
          endif
          if(rem_no_integt) then
              call io_wrout(
     *        '                  with integation-time corrn suppressed')
          else
              write(out, '(a,f8.1,a)' )
     *        '                  with integration-time corrected for',
     *                    rem_integt,' secs'
          endif

          if( rem_mod_mean .ne. 0.  .or.  rem_mod_sigma .ne. 0. ) then
              write(out,*)'Noise added    :',rem_mod_mean,rem_mod_sigma
          endif
      else
          write(out,*)'  UNKNOWN remove type',rem_type
      endif
      call lsf_close( lsf_num, s )
      if ( s .ne. 0 ) goto 9999

      if (out .ne. old_out) then
          call io_close( out, s )
          call io_setout( old_out )
      end if

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          if ( s .ne. USR_BREAK ) then
              call rem_wrerr( s, 'in subroutine REM_DISPLAY' )
          end if
          return
      end
