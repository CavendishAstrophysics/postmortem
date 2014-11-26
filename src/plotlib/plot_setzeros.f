
C+plot_setzeros
C
      subroutine plot_setzeros ( option, s )
C
C Determines the optional plotting of zero data values
C
C Input:
C    option to turn on plots of zero data values (YES/NO)
       character*(*)      option
C Returned:
C    Status
       integer            s
C-

       include '/mrao/post/include/plot_control.inc'
       include '/mrao/post/include/plot_errors.inc'
       include '/mrao/include/chrlib_functions.inc'

       if (s.ne.0) return

       plot_zeros = chr_cmatch(option(1:chr_lenb(option)),'YES')

       if (s.ne.0) call plot_wrerr(s,'in subroutine PLOT_SETZEROS' )

       end
