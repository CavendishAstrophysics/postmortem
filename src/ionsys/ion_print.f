C
C+ion_print
C
      SUBROUTINE ion_print ( psf_name, s )

C
C     Prints the current ionospheric correction.
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
      include        '/mrao/include/constants.inc'
      include        '/mrao/include/iolib_errors.inc'
      include        '/mrao/post/include/ion_runtime.inc'

C     ****************************************************************
C
C     Local variables
C         Loop counter
              integer             i
C         Logical sample file number
              integer             lsf_num
C         Previous and current output devices
              integer             out, old_out
C         String and its length
              character*80        string
              integer             ls, ls1

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

      if (ion_type .ne. 0) then
          write( out, '(/,X,A)' ) 'Correction logical sample file :'
          call lsf_open( psf_name, ion_lsf, 'READ', lsf_num, s )
          call lsf_display( lsf_num, s )
          call lsf_close( lsf_num, s )
          if ( s .ne. 0 ) goto 9999
      end if

      write( out, '(X,A)' ) 'Correction Sources :'
      do 100, i = 1, ion_numsrcs
          string = ' '//ion_source( i )
          ls     = 24
          call chr_chdtos( ion_ra(i)/const_h2r, 1, string(ls:), ls1 )
          ls = ls+ls1
          call chr_chdtos( ion_dec(i)/const_d2r, 1, string(ls:), ls1 )
          write(out,'(2A,F6.1,A)') string(1:ls+ls1+2),'(',ion_epoch,')'
  100 continue

      if (ion_type .eq. 0) then
          write(out,'(X,A)')
     *        'Correction derived by Peter Warners old Mapper program.'
      else if (ion_type .eq. 1) then
          write(out,'(X,A,I4,A)') 'Map size is ',map_size,' gridpoints.'
          write(out,'(X,A,F7.3,A)') 'Sampling is ', arcsec_per_gp,
     *                                                    ' arcsec/gp.'
          write(out,'(X,A,A)')
     *        'Convolution: binning to nearest gridpoint ',
     *        'weighted by no. of vis/gp'

          write(out,'(X,A,/,X,A,F6.1,A)')
     *    'Correction finds the maximum of a parabola through max. and',
     *    'adjacent points - search radius is ',search_rad,' arcsec.'
          if( radial_factor .ne. 0. ) write(out,'(X,A,F8.4)')
     *    'Additional radial weighting of radius *',radial_factor
      else if (ion_type .eq. 2) then
          write(out,'(X,A,I4,A)') 'Map size is ',map_size,' gridpoints.'
          write(out,'(X,A,F7.3,A)') 'Sampling is ', arcsec_per_gp,
     *                                                    ' arcsec/gp.'
          write(out,'(X,A,A)')
     *        'Convolution: binning to nearest gridpoint ',
     *        'weighted by no. of vis/gpt.'

          write(out,'(X,A,X,A,F6.1,A,/,X,A,F4.2,A)')
     *   'Correction finds the centroid of',
     *   'flux within',search_rad,' arcsec of the map centre and above',
     *   'a level of ', min_amp, ' of the map maximum in this region.'
      else if (ion_type .eq. 3) then
          write(out,'(X,A,I4,A)') 'Map size is ',map_size,' gridpoints.'
          write(out,'(X,A,F7.3,A)') 'Sampling is ', arcsec_per_gp,
     *                                                    ' arcsec/gp.'
          write(out,'(X,A,A)')
     *        'Convolution: binning to nearest gridpoint.',
     *        'weighted by no. of vis/gp.'

          write(out,'(X,A,/,X,A,F6.1,A,/,X,A,/,X,A,/,X,A,F4.2,A,F6.1,
     *                /,X,A,/,X,A,I2,A,/   )')
     *   'Algorithm is an iterative procedure restricted to a region ',
     *   'within ',search_rad,' arcsec of the map centre.',
     *   'The seed is the position of the map maximum in the region: ',
     *   'each subsequent guess is the centriod of all flux greater',
     *   'than ',min_amp,' of the maximum and within a radius of ',
     *   source_size, ' arcsec of the seed.',
     *   'The process is iterated ', num_iters,' times.'
      end if

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
              call ion_wrerr( s, ' in subroutine ion_print ' )
          end if
          return
      end
