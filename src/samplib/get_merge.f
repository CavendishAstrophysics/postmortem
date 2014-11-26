
C+GET_MERGE

       subroutine get_merge ( lun, prompt, default, merge_type, s )
C
C     Reads a valid spacing merge type from the command line.
C
C     Given:
C         Sample file logical unit number
              integer             lun
C         Prompt
              character*(*)       prompt
C         Default - must be valid merge descriptor string from the
C         file (postmortem)merge_types.inc'
              character*(*)       default

C     Returned:
C         Integer coded merge type - from (postmortem)merge_types.inc'
              integer             merge_type
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C
C-
C     ****************************************************************
C
C     Function declarations

      include '/mrao/include/chrlib_functions.inc'

C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include '/mrao/include/iolib_errors.inc'
      include '/mrao/post/include/merge_types.inc'
      include '/mrao/post/include/phys_tscopes.inc'
      include '/mrao/post/include/samplib_errors.inc'

C     ****************************************************************
C
C     Local variable declarations
C         Users reply to io_getopt
              character*80        reply
C         loop counter
              integer             i
C         Telescope type
              integer             itscope

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) goto 999

C     ****************************************************************
C
C         Main Code
C         ---------
C

      call enq_phys_tscope( lun, itscope, s )
      if (s.ne.0) goto 999

      if (itscope.eq.clfst .or. itscope.eq.ryle) then
          if ( chr_chsame( default, '*' ) ) then
              i = index( merge_types( merge_type ), ' ' )
              reply = merge_types(merge_type)(1:i)
          end if

    1    call io_getopt(prompt,default, merge_types, num_merge, reply,s)
          if ( s .ne. 0 ) goto 999

          do 100, i = 1, num_merge
              if (chr_cmatch( reply, merge_types(i) )) then
                  merge_type = i
                  if (itscope.eq.clfst .and.
     :               (merge_type.eq.subband_merge .or.
     :                merge_type.eq.channel_merge .or.
     :                merge_type.eq.fr_aerial_merge)) then
                       s = ILL_MERGE
                  else if (itscope.eq.ryle .and.
     :                (merge_type.eq.hut_merge .or.
     :                 merge_type.eq.hut_sw_merge)) then
                       s = ILL_MERGE
                  end if
              end if
  100     continue

          if ( s .eq. ILL_MERGE ) then
             call smp_wrerr(s, ' ')
             s = 0
             goto 1
          end if

      else
          s = ILL_TSCOPE
      end if

      if (s.ne.0) goto 999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 999  if ( s .ne. usr_break ) then
          call smp_wrerr( s, 'in subroutine GET_MERGE' )
      end if

      end
