C
C+ion_edit_corr
C
      SUBROUTINE ion_edit_corr( sf_name,
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
      rpwl2dpkm = (freq*1000.0D+0)/(const_c*const_d2r)
      call io_brkfil( sf_name, user, name, type )
      ls = chr_lenb( name )

      call enq_ion_name( sf_lun, ion_number, corr_name, s )
      write(header, '(2A, I2, 2A)' )
     *        name(1:ls),' - Ionospheric Correction no. ', ion_number,
     *        ' on ',corr_name

      if ( s .ne. 0 ) goto 9999

      do 100, i = ion_first, ion_last
          call read_rt( sf_lun, 1, i, ra, dec, sid, s )
          if ( s .ne. 0 ) goto 9999
          time(i) = real(sid)/36000.0

          call read_ion_corr( sf_lun, ion_number, i, ion_arr, s )
          if ( s .eq. 0 ) then
              ion(i) = ion_arr(1)*rpwl2dpkm
              amp(i) = ion_arr(3)
          else
              s = 0
              ion(i) = 0.0
              amp(i) = 0.0
          end if
  100 continue
      call close_source( sf_lun, 1, s )
      call close_sf( sf_lun, s )

c     call ide_system( int(ion_last-ion_first+1),
c    *                 time(ion_first), amp(ion_first), ion(ion_first),
c    *                 'Sidereal time (dec. hrs)',
c    *                 'Amplitude (Jy)', 'Correction (deg/km)',
c    *                 header,
c    *                 plot_device, s                          )

      if (s .eq. ide_write) then
          s = 0
          call open_sf( sf_lun, sf_name, 'WRITE', 0, s )
          call open_source( sf_lun, 1, 0, s )
          do 200, i = ion_first, ion_last
              call read_ion_corr( sf_lun, ion_number, i, ion_arr, s )
              ion_arr(1) = ion_arr(1)*rpwl2dpkm

C             Only update if necessary
              if ( s.ne.0               .or.
     *             ion_arr(1).ne.ion(i) .or.
     *             ion_arr(3).ne.amp(i)       ) then
                  s = 0
                  ion_arr(1) = ion(i)/rpwl2dpkm
                  ion_arr(3) = amp(i)
                  call write_ion_corr( sf_lun, ion_number, i, ion_arr,s)
                  if (s .ne. 0) goto 9999
              end if
  200     continue
          call close_source( sf_lun, 1, s )
          call close_sf( sf_lun, s )
      end if

      if (s .eq. 0) return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          if ( s .ne. USR_BREAK ) then
              call ion_wrerr( s, ' in subroutine ion_edit_corr ' )
          else
              s = 0
              call close_source( sf_lun, 1, s )
              call close_sf( sf_lun, s )
          end if
          return
      end
