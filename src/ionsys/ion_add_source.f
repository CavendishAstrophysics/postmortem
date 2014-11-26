C
C+ion_add_source
C
      SUBROUTINE ion_add_source( s )

C
C     Adds a source to the current ionospheric correction definition.
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
      include        '/mrao/include/iolib_errors.inc'
      include        '/mrao/post/include/ion_runtime.inc'

C     ****************************************************************
C
C     Local variables
C         Loop counter
              integer             i
C         Flags set if source has already been used or is a planet.
              logical             found, planet
C         New source name, ra and dec
              character*(20)      source_name, source_chr_ucase
              real*8              ref_date, ra, dec

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
      if (ion_numsrcs .lt. max_ionsrcs) then
          call get_source( 'Source name ?', ' ', ion_epoch,
     *                     ref_date, ra, dec, source_name, s       )
          if ( s .ne. 0 ) goto 9999

          found = .false.
          do 100, i = 1, ion_numsrcs
              if (ion_ra(i).eq.ra .or. ion_dec(i).eq.dec) then
                  found = .true.
              end if
  100     continue

          source_chr_ucase= source_name
          call chr_chucas( source_chr_ucase )
          call planet_index( source_chr_ucase, i, s )
          planet = ( s .eq. 0 )
          s = 0

          if (.not. found .and. .not. planet) then
             ion_numsrcs = ion_numsrcs+1
             call precrd2( 1,
     *                      ref_date, ra, dec,
     *                      ion_epoch,
     *                      ion_ra(ion_numsrcs), ion_dec(ion_numsrcs) )
             ion_source( ion_numsrcs ) = source_name
          else if (found) then
             call io_wrout( 'The correction already uses that source.' )
          else
             call io_wrout( 'A high proper motion source must be the '//
     *                    'first source in a correction.'             )
          end if
      else
          call io_wrout( 'There are no more source slots available.' )
      end if

      if ( s .ne. 0 ) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          if ( s .ne. USR_BREAK ) then
              call ion_wrerr( s, ' in subroutine ion_add_source ' )
          end if
          return
      end
