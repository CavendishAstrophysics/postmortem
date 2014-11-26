C
C+get_source
C
      SUBROUTINE get_source(  prompt, default, epoch,
     *                        ref_date, ra, dec, name, s )

C
C     Reads source details from the command line.
C
C     Given:
C         User prompt.
              character*(*)       prompt
C         Default source name.
              character*(*)       default
C         Epoch that source position should be valid for.
              real*8              epoch
C
C     Returned:
C         Source reference date, RA and Dec.
              real*8              ref_date, ra, dec
C         Source name
              character*(*)       name
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C
C     Prompts for a source and:
C     1.  Checks to see whether it is a special case with high proper
C         motion (Sun, planets or the poles).
C     2.  The response PHASE-CENTRE requests the default 1950.0 phase
C         centre which is not known by this routine.  RA and DEC are
C         set to -1.0 to key this option.
C     3.  if not, scans the source list for a match and if no chr_match is found
C         then prompts for a 1950.0 RA and Dec.
C
C     The source positions and reference dates returned correspond to
C     the maplib definitions (ie. all positions are 1950.0 except for the
C     North and South poles - even for sources with high proper motions
C     such as the Sun)
C
C     NPR   30 October 1987.
C
C-
C     ****************************************************************
C
C     Function declarations
C
      include        '/mrao/include/chrlib_functions.inc'

C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include        '/mrao/post/include/post_sys_pars.inc'
      include        '/mrao/post/include/samplib_errors.inc'
      include        '/mrao/include/iolib_errors.inc'
      include        '/mrao/include/constants.inc'

C     ****************************************************************
C
C     Local variables, equivalences and commons
C         Loop counter
              integer         i
C         Source list name
              character*80    source_list
C         Abbreviated source name entered by user.
              character*15    abbr_name
C         RA and dec of the source at epoch.
              real*8          epoch_ra, epoch_dec
C         Integer date form of epoch and mjd form of epoch
              integer         iepoch(3)
              real*8          mjd
C         Position in string form and two string lengths
              character*40    position
              integer         ls1, ls
C         Default output device
              integer         out
C         Names of the planets and flag set if the source is a planet.
              character*8     planets(10)
              logical         planet_flg
              data  planets / 'Sun', 'Moon',
     *                        'Mercury', 'Venus', 'Mars', 'Jupiter',
     *                        'Saturn', 'Uranus', 'Neptune', 'Pluto' /

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

      call io_enqout(out)

      source_list = def_source_list

C     ****************************************************************
C
C         Main Code
C         ---------
C
  100 continue
          abbr_name   = name
          call io_getstr( prompt, default, abbr_name, s )

          if (abbr_name .eq. '?') then
              call io_getfil( 'Source list : ', '*', source_list, s)
          end if
      if (s.eq.0 .and. abbr_name .eq. '?') goto 100
      if ( s .ne. 0 ) goto 9999

C     Find if the source is a planet.
      planet_flg = .false.
      do 200, i = 1, 10
          if ( chr_cmatch(abbr_name, planets(i))
     *                            .and. .not.planet_flg ) then
              planet_flg = .true.
              abbr_name  = planets(i)
          end if
  200 continue

C     Find Ra and dec, treating the poles and planets as special cases.
      if (planet_flg) then
C         Adjust the epoch to a Julian date.
          call frddat( epoch, iepoch )
          call sla_cldj( iepoch(3), iepoch(2), iepoch(1), mjd, s )
          mjd = mjd + 0.5D+0

C         Find RA,Dec at date for current Julian date and 1950.0D+0
          ref_date = 1950.0D+0
          name     = abbr_name
          call chr_chucas(abbr_name)
          call planet_geo( abbr_name, mjd, ra, dec, s )
          call precrd2( 1, epoch, ra, dec, ref_date, ra, dec )
      else if ( chr_cmatch( abbr_name, 'NORTH-POLE' ) ) then
          ra       = const_pi
          dec      = const_piby2
          ref_date = epoch
          name     = 'North-Pole'
      else if ( chr_cmatch( abbr_name, 'SOUTH-POLE' ) ) then
          ra       = const_pi
          dec      = -const_piby2
          ref_date = epoch
          name     = 'South-Pole'
      else if ( chr_cmatch( abbr_name, 'PHASE-CENTRE' ) ) then
          ra       = -1.0
          dec      = -1.0
          ref_date = 1950.0D+0
          name     = ' '
      else
          call scan_source_list(  abbr_name, source_list,
     *                            name, ra, dec, s )

          if ( s .eq. NO_SOURCE ) then
              s = 0
              name = abbr_name
              call io_getra(  '1950.0 RA  : ', '*', ra,  s )
              call io_getdec( '1950.0 dec : ', '*', dec, s )
              if ( s .ne. 0 ) goto 9999
          else if ( s .ne. 0 ) then
              goto 9999
          end if

          ref_date = 1950.0D+0
      end if

C     Write out result.

      write(out,*)

      if (ra.gt.0.0 .and. dec.gt.0.0) then
        call chr_chdtos( ra/const_h2r,  2, position, ls1)
        call chr_chdtos( dec/const_d2r, 2, position(ls1+1:), ls)
        ls = ls1+ls+2
        write(out,'(2X,3A)') name(1:12),position(1:ls),'(1950)'
        call precrd2( 1, ref_date, ra, dec,
     *                epoch, epoch_ra, epoch_dec )
        call chr_chdtos( epoch_ra/const_h2r,  2, position, ls1)
        call chr_chdtos( epoch_dec/const_d2r, 2, position(ls1+1:), ls)
        ls = ls1+ls+2
        write(out,'(2X,3A,F7.2,A)')
     *                   name(1:12),position(1:ls),'(',epoch,')'

       else

        write(out,'(X,A)') name

      end if

      write(out,*)

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          if ( s .ne. usr_break ) then
              call smp_wrerr( s, 'in subroutine GET_SOURCE' )
          end if
          return
      end
