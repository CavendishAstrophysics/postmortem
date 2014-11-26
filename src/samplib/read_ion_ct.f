C+READ_ION_CT

      subroutine read_ion_ct ( lun, s )
C
C     Reads ionospheric correction redtape for a given sample file.
C
C     Given:
C         Logical unit number of physical sample file.
              INTEGER     LUN
C
C     Returned:
C         Status - must be zero on entry.
              INTEGER     S
C
C     Sets up the ionospheric correction redtape for a given, open
C     physical sample file by reading the redtape from the corresponding
C     'ION' file, or, if this does not exist, by initialising the redtape
C     common blocks.
C
C     This routine is called initially from OPEN_SOURCE when the source
C     in a physical sample file is opened.
C
C     S = 0 for successful return, otherwise error code.
C
C-
C
      include '/mrao/include/iolib_errors.inc'
      include '/mrao/post/include/samplib_errors.inc'
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v0.inc'
      include '/mrao/post/include/sf_pack.inc'
      include '/mrao/post/include/ion_definition.inc'

      character*64    file_name*64
      integer         i, ion

      if ( s.ne.0 ) return

C     Check whether correction redtape is already available
      if (ion_lun.eq.lun) return

C     Get correction file name
      call enq_namfil( lun, 'ION', file_name, s )

      if ( s.eq.0 ) then
C         Open the file
          call io_nxtlun( ion, s )
          if ( s.ne.0 ) goto 999
          open( ion, file=file_name, access='DIRECT', status='OLD',
     *               form='unformatted', recl=ion_length*4, iostat=s )
          if ( s.ne.0 ) goto 999

C         Open successful, read redtape into common blocks
          read( ion, rec=1, iostat=s ) ion_redtape
          close( ion )

      else if ( s.eq.NO_FILE ) then
C         Initialise the common blocks
          s = 0

C         Read sample file control tables.
          call read_ct( lun, s )
          if ( s.ne.0 ) goto 999

C         Read sample file packing information.
          if (sf_lun .ne. lun) then
              call get_sf_pack( lun, 1, s )
          end if
          if ( sf_type .ne. 1 ) s = NOT_PHYSF
          if ( s.ne.0 ) goto 999

          do i = 1, ion_length
              ion_redtape(i) = 0
          enddo

          do i = 1, max_ionsrcs
              ion_source(i) = ' '
          enddo

          max_index    = (samp_len-start_ion)/4

      endif

      if ( s.ne.0 ) goto 999

      ion_lun = lun

      return

 999  call smp_wrerr( s, 'in subroutine READ_ION_CT' )

      end
