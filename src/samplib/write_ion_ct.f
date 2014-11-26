
C+WRITE_ION_CT

      subroutine write_ion_ct ( lun, s )
C
C     Writes ionospheric correction redtape for a given sample file.
C
C     Given:
C         Logical unit number of physical sample file.
              INTEGER     LUN
C
C     Returned:
C         Status - must be zero on entry.
              INTEGER     S
C
C     Writes the ionospheric correction redtape for a given, open
C     physical sample file to the corresponding 'ION' file on disc.
C
C     This routine is called from SET_ION_CORR whenever the correction
C     redtape is updated.
C
C     S = 0 for successful return, otherwise error code.
C last mod 12 April 2000 GP [io_setacc]
C-
C
      include '/mrao/include/iolib_errors.inc'
      include '/mrao/post/include/samplib_errors.inc'
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/ion_definition.inc'

      character  file_name*64
      integer    ion

      if ( s.ne.0 ) return

C     Read and check control tables

      call read_ion_ct( lun, s )
      if ( s.ne.0 ) goto 999

C     Open the correction file and write redtape
      call io_nxtlun( ion, s )
      if ( s.ne.0 ) goto 999

C     Get correction file name
      call enq_namfil( lun, 'ION', file_name, s )
      if (s.eq.0) then
          open( ion, file=file_name, access='direct', status='OLD',
     *               form='unformatted', recl=ion_length*4, iostat=s )
      else if (s .eq. NO_FILE) then
          open( ion, file=file_name, access='DIRECT', status='NEW',
     *               form='unformatted', recl=ion_length*4, iostat=s )
          call io_setacc(file_name, 'r', 'rw', 'rw', s)
      end if

      if ( s.eq.0 ) then
          write( ion, rec=1, iostat=s ) ion_redtape
          close( ion )
      end if

      if ( s.ne.0 ) goto 999

      return

 999  call smp_wrerr( s, 'in subroutine WRITE_ION_CT' )

      end
