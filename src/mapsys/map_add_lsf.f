C
C+map_add_lsf
C
      SUBROUTINE map_add_lsf ( s )

C     Adds a logical sample file to the current map definition.
C
C     Given:
C         None.
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include    '/mrao/include/constants.inc'
      include    '/mrao/include/iolib_errors.inc'
      include    '/mrao/include/maplib_redtape.inc'
      include    '/mrao/post/include/lsflib_errors.inc'
      include    '/mrao/post/include/mapsys_save.inc'

C     ****************************************************************
C
C     Local variables and arrays
C         Logical sample file number
              integer         lsf_num
C         Physical sample file name, and logical sample file key
              character*80    sf_name
              integer         lsf_key
C         Maximum baseline of the lsf.
              real            radius
C         Maximum allowable sampling
              real            max_samp

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return
      call dpredt( mapsys_save, s )

      if (numlsf .ge. maxlsf) then
          call io_wrout( 'No more LSF''s can be added to this map.' )
          return
      end if

      call enmlsf( 1, sf_name, lsf_key, s )

      if (lsf_key .eq. 1) then
C         Logical sample file must be saved
          call io_wrout( 'Current logical sample file must be saved.' )
          call lsf_open( sf_name, lsf_key, 'READ', lsf_num, s )
          call lsf_save( lsf_num, lsf_key, s )
          call lsf_close( lsf_num, 0 )
          if ( s .ne. 0 ) goto 9999
          call stmlsf( 1, sf_name, lsf_key, s )
      end if

C     ****************************************************************
C
C         Main Code
C         ---------
C

      sf_name = ' '
      lsf_key = 0
      call lsf_open( sf_name, lsf_key, 'READ', lsf_num, s )
      if (s.ne.0) goto 9999
      call lsf_enq_max_rad( lsf_num, radius, s )
      call lsf_close( lsf_num, s )

      max_samp = 1.0 / (const_sa2r * 2.0 * radius )
C     Ensure that convolution function does not go outside aperture.
      max_samp = max_samp / ( 1 - 2*(convhw+1)/min0(iymax, ixmax) )

      if ( usamp .gt. max_samp ) then
          write( 1, * ) 'Map sampling too small for this sample file.'
      else
          numlsf = numlsf + 1
          call stmlsf( numlsf, sf_name, lsf_key, s )
      end if

      if ( s .ne. 0 ) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
         if (s.eq.NO_LSFSAVED) then
           call io_wrout('No logical sample files saved for this file.')
         else if (s.ne.USR_BREAK) then
           call map_wrerr( s, 'in subroutine MAP_ADD_LSF' )
         end if
         call ldredt( mapsys_save, 0 )
         return
      end
