C
C+ion_list_corrs
C
      SUBROUTINE ion_list_corrs ( psf_name, s )

C
C     Lists the current corrections in the sample file.
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
C     Function declarations
C
      include        '/mrao/include/chrlib_functions.inc'

C     ****************************************************************
C
C     Global common declarations
C
      include        '/mrao/post/include/ion_runtime.inc'

C     ****************************************************************
C
C     Local variables
C         Loop counter
              integer             i
C         Sample file unit number
              integer             sf_lun
C         Current output devices
              integer             out
C         The number of old, & the total number of corrections on file
              integer             num_corr, num_old
C         Number of new slots in the file and the number full.
              integer             num_slots, num_full
C         Correction name and its length
              character*80        name
              integer             ls

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
      call io_enqout( out )
      call open_sf( sf_lun, psf_name, 'READ', 0, s )
      call open_source( sf_lun, 1, s )

      call enq_numcorr( sf_lun, num_corr, s )

      write( out, '(/,X,A,/,X)' ) 'Corrections in sample file are :'
      do 100, i = 1, num_corr
          call enq_ion_name( sf_lun, i, name, s )
          ls = chr_lenb(name)
          if (i.eq.ion_number) then
              write(out, '(A,I2,3A)') ' ==> ',i, '. on ', name(1:ls),'.'
          else
              write(out, '(A,I2,3A)') '     ',i, '. on ', name(1:ls),'.'
          end if
  100 continue

c     call enq_ion_slots( sf_lun, num_old, num_slots, num_full, s )
c     write(out,'(/,X,I2,A,I2,A,/,X,I2,A,I2,A,/)')
c    *    num_old,' out of ',num_corr,
c    *                        ' corrections are old style corrections.',
c    *    num_full, ' new slots used out of ', num_slots, ' available.'

      call close_source( sf_lun, 1, s )
      call close_sf( sf_lun, s )
      if ( s .ne. 0 ) goto 9999


      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call ion_wrerr( s, ' in subroutine ion_list_corrs ' )
          return
      end
