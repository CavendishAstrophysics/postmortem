C General NAG called routine used during least-square minimization
C
C
C+lsfun1
C
      SUBROUTINE lsfun1(m, n, xc, fvecc)
C
C Subroutine to evaluate residuals for E04FDF call in
C
C Given:
C   number of residuals
       integer        m
C   number of variables
       integer        n
C   current variable values
       real*8         xc(n)
C
C Returns:
C   residuals
       real*8         fvecc(m)
C
C The exact action of this routine depends on the setting of IMODE
C
C IMODE=1 used in ion_disp_wave, keyed by ION-DISPLAY-WAVE
C     Used to fit:    a + bx + cxx
C PJW 28/04/89
C
C IMODE=2 used in fit_spacings, keyed by FIT-SPACINGS
C     Used to fit:    Acos(HA) + Dsin(HA) + C to spacing data
C PA 10/10/90
C-
C
      include '/mrao/post/include/global_constants.inc'
      include '/mrao/post/include/lsfun1.inc'

C Local variables
      integer       i

C check mode
      if (imode.eq.1) then
        do i = 1,m
          fvecc(i) = xc(1) + xc(2)*pos(i) + xc(3)*pos(i)*pos(i)
     *               - phase(i)
        end do

      else if (imode.eq.2) then
        do i=1,m
          fvecc(i) = xc(1)*cos(vis_HA(i)) + xc(2)*sin(vis_HA(i)) +
     *               xc(3) - vis_phase(i)
        end do
      end if

      end
C
C
*+ set_E04FDF_mode

      subroutine set_E04FDF_mode( mode, s )
C     -------------------------------------
C
C Set the mode for use in the E04FDF minimization
C
C Given:
C   mode
      character*(*)         mode
C Returned:
C   status word
      integer               s
C
C The mode for the minimization is set using this routine.  Values
C currently defined are those specified in the specification to the
C LSFUN1 routine.
C
C-
       include '/mrao/post/include/global_constants.inc'
       include '/mrao/post/include/lsfun1.inc'

       if (s.ne.0) return

       imode = 0
       if (mode(1:1).eq.'I' .or. mode(1:1).eq.'i') then
         imode = 1
       else if (mode(1:1).eq.'F' .or. mode(1:1).eq.'f') then
         imode = 2
       end if
       if (imode.eq.0) then
         call io_wrout('***(set_E04FDF_mode) Warning: Unknown mode')
       end if

       if (s.ne.0) call io_wrerr(s,' in set_E04FDF_mode')

       end
