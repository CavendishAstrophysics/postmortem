C
C+pmadvance
C
      subroutine pmadvance ( s )

C     Duplicates PGADVANCE for postmortem, returning a status.
C
C     Given:
C         None.

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     This routine is essentially the same as PGADVANCE, but if the
C     user types <esc>, Q or E instead of <RETURN> then a status of
C     USR_BREAK is returned. Also no prompting is done and the PGPLOT
C     ask flag is set to the default for the current plot device - on
C     if interactive, otherwise off.
C
C     NPR     13 October 1987.
C-
C     ==================================================================
C
C     Function declarations

      include        '/mrao/include/chrlib_functions.inc'

C
C     Global constant declarations

      include        '/mrao/include/iolib_errors.inc'

C
C     Local variable declarations
C         The "advance" state of PGPLOT.
              integer             adv_state
C         The "prompt" state of PGPLOT.
              logical             prompt
C         Users keyboard response
              character*1         response

C
C Main Code
C ---------
C
      if ( s .ne. 0 ) return

      call pgqask( prompt )
      call pgqadv( adv_state )

      if ((adv_state.eq.1) .and. prompt ) then
          call io_setech( .false. )
          call io_getkey( ' ',' ','Qq',response,s)
          call io_setech( .true. )
          if ( chr_cmatch( response, 'Q' ) ) s = usr_break
      end if

      call pgask( .false. )
      if ( s .eq. 0 ) then
          call pgadvance
      end if

      call pgask( prompt )

      return

C
C Error Handling
C --------------
C
 9999 continue
          call plot_wrerr( s, 'in subroutine PMADVANCE' )
          return
      end
