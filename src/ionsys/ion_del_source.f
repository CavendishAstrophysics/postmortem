C     Enforce explicit type checking

C
C+ion_del_source
C
      SUBROUTINE ion_del_source( s )

C
C     Deletes a source from the ionospheric correction definition.
C
C     Given:
C         None.

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
      include        '/mrao/include/chrlib_functions.inc'
      include        '/mrao/include/iolib_errors.inc'
      include        '/mrao/post/include/ion_runtime.inc'

C     ****************************************************************
C
C     Local variables
C         Loop counter
              integer             i
C         Flag set if source has already been used
              logical             found
C         Source name to delete
              character*(20)      source_name

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

      if (ion_numsrcs .eq. 1) then
          call io_wrout( 'You cannot delete the last source.' )
          return
      end if

C     ****************************************************************
C
C         Main Code
C         ---------
C
      call io_getopt( 'Source to delete ?',
     *              '', ion_source, ion_numsrcs, source_name, s )
      if ( s .ne. 0 ) goto 9999

      found = .false.
      do 100, i = 1, ion_numsrcs
          if ( .not. found ) then
              found = chr_cmatch(source_name,ion_source(i))
          else
              ion_source(i-1) = ion_source(i)
              ion_ra(i-1)     = ion_ra(i)
              ion_dec(i-1)    = ion_dec(i)
          end if
  100 continue
      ion_numsrcs = ion_numsrcs - 1

      if ( s .ne. 0 ) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          if ( s .ne. USR_BREAK ) then
              call ion_wrerr( s, ' in subroutine ion_del_source ' )
          end if
          return
      end
