
C
C+ion_disp_corr
C
      SUBROUTINE ion_disp_corr( sf_name,
     *                          plot_device,
     *                          s                      )

C
C     Plots a given ionospheric correction on the plot device.
C
C     Given:
C         Logical sample file number
              character*(*)       sf_name
C         PGPLOT plot device
              character*(*)       plot_device
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C
C-
C     ****************************************************************
C
C     Function declarations
C
      include    '/mrao/include/chrlib_functions.inc'

C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/constants.inc'
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/post/include/idesys_errors.inc'
      include  '/mrao/post/include/clfst_constants.inc'
      include  '/mrao/post/include/ion_runtime.inc'

C     ****************************************************************
C
C     Variables, equivalences and commons
C         General purpose loop counter and string length
              integer         i, ls
C         The sample file unit number
              integer         sf_lun
C         Number of samples in the sample file
              integer         num_samp
C         Current ionospheric correction name.
              character*80    corr_name
C         Sample data returned from read_rt and read_ion_corr
              real*8          ra, dec
              integer         sid
              real*4          ion_arr(4)
C         Observing frequency
              real*8          freq
C         Conversion factor radians/wavelength to degrees/kilometer
              real            rpwl2dpkm
C         Arrays of the correction, the detected amplitude and time
              real            ion( max_samp )
              real            amp( max_samp )
              real            time( max_samp )
C         Sample file user name and type
              character       user*33, name*16, type*4
C         Plot parameters
              integer         num_plot, first_samp, last_samp, samp_rate
C         Header text for the plot.
              character*(80)  header

C         Postmortem common
              common / post / ion, amp, time

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

      if (ion_number.le.0 .or. (ion_type.ne.0.and.ion_key.eq.0)) then
          call io_wrout( 'Current correction has not yet been made.' )
          return
      end if

C     ****************************************************************
C
C         Main Code
C         ---------
C
      call open_sf( sf_lun, sf_name, 'READ', 0, s )
      call open_source( sf_lun, 1, 0, s )
      call enq_freq( sf_lun, freq, s )
      call enq_numsamp( sf_lun, 1, num_samp, s )
      rpwl2dpkm = (freq*1000.0D+0)/(const_c*const_d2r)
      call io_brkfil( sf_name, user, name, type )
      ls = chr_lenb( name )

      call enq_ion_name( sf_lun, ion_number, corr_name, s )
      write(header, '(2A, I2, 2A)' )
     *        name(1:ls),' - Ionospheric Correction no. ', ion_number,
     *        ' on ',corr_name

      first_samp = ion_first
      call io_geti( 'First sample ?', '*', first_samp, s )
      first_samp = max( 1, min( first_samp, num_samp ) )
      last_samp  = ion_last
      call io_geti( 'Last sample ?', '*', last_samp, s )
      last_samp  = max( first_samp, min( last_samp, num_samp ) )
      samp_rate = (last_samp-first_samp)/800 + 1
      call io_geti( 'Sampling rate ?', '*', samp_rate, s )
      samp_rate = max( 1, samp_rate )
      if ( s .ne. 0 ) goto 9999

      num_plot = 0
      do 100, i = first_samp, last_samp, samp_rate
          call read_rt( sf_lun, 1, i, ra, dec, sid, s )
          if ( s .ne. 0 ) goto 9999

          call read_ion_corr( sf_lun, ion_number, i, ion_arr, s )
          if ( s .eq. 0 ) then
              num_plot = num_plot+1
              time(num_plot) = real(sid)/36000.0
              ion(num_plot) = ion_arr(1)*rpwl2dpkm
              amp(num_plot) = ion_arr(3)
          else
              s = 0
          end if
  100 continue
      call close_source( sf_lun, 1, s )
      call close_sf( sf_lun, s )

C     Plot graphs
      call pgbegin( 0, plot_device, 1, 1 )
      call pgbbuf
      call pmadvance( s )
      call pgvport( 0.1, 0.9, 0.55, 0.9 )
      call plot_data( num_plot, time, amp, 1.0, 1.0, s )
      call pglabel( 'Sidereal time (dec. hrs)', 'Amplitude(Jy)',
     *              header                                       )
      call pgvport( 0.1, 0.9, 0.1, 0.45 )
      call plot_data( num_plot, time, ion, 1.0, 1.0, s )
      call pglabel('Sidereal time (dec. hrs)','Correction (deg/km)',' ')
      call pgebuf
      call pgend

      if (s .eq. 0) return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          if ( s .ne. USR_BREAK ) then
              call ion_wrerr( s, ' in subroutine ion_disp_corr ' )
          else
              s = 0
              call close_source( sf_lun, 1, s )
              call close_sf( sf_lun, s )
          end if
          return
      end
