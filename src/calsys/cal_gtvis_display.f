C
C
C+cal_gtvis_display

       subroutine cal_gtvis_display( data, plot_device, s )
C      ----------------------------------------------------
C
C Display the contents of the supplied visibility gains table
C
       include '/mrao/post/include/global_constants.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/constants.inc'
C
C      Given:
C        complex data defining the gains table
                  complex    data( max_vis )
C        default plot device
                  character  plot_device*(*)
C      Returned:
C        error status
                  integer   s
C
C
C The visibility-based gains table is displayed.  The display is
C a plot of the amplitude and phase corrections for each visibility.
C
C PA 21/11/91
C-
C Local variables
C   counters
       integer      i,  n
C   output string and title for plot
       character    string*60, local_title*80
C   length of string
       integer      ls, len_title
C   arrays for data plotting
       real*4       vis_amplitude
       real*4       vis_phase
       real*4       max_vis_amplitude
C   range of data for amplitude plot
       real*4       x1, y1, y2
C   PGPLOT functions
       real*4       pgrnd


C check status on entry
       if (s.ne.0) return

C prompt for title for plot and table
       call io_getstr('Plot title : ','Calibration Factors',
     *             local_title, s )
       len_title = chr_lenb(local_title)
       max_vis_amplitude = 0.0
       do i=1,max_vis
         max_vis_amplitude = max(max_vis_amplitude,abs(data(i)))
       end do

C plot amplitude/phase correction factors: plot is of a "histogram" type
       call plot_begin( plot_device, s )
       call pgbbuf
       call pgvport(0.1,0.8,0.1,0.4)
       call pgwindow(0.0,1.0,0.0,1.0)
       call pgmove(0.0,0.0)
       call pgdraw(0.0,1.0)
       call pgmove(0.0,0.0)
       call pgdraw(1.0,0.0)
       call pgwindow(0.0,float(max_vis),-180.0,180.0)
       call pgmove(0.0,0.0)
       call pgdraw(float(max_vis),0.0)
       call pgmove(0.0,0.0)
       do n=1,max_vis
         vis_phase = atan2(imag(data(n)),real(data(n)))/const_d2r
         call pgdraw(float(n-1),vis_phase)
         call pgdraw(float(n),vis_phase)
       end do
       call pgwindow(0.0,1.0,0.0,1.0)
       do n=0,max_vis,100
         write(string(1:4),'(I4)') n
         x1=float(n)/float(max_vis)
         y1=0.0
         y2=0.05
         call pgmtext('B',1.25,x1,0.5,string(1:4))
         call pgmove(x1,y1)
         call pgdraw(x1,y2)
       end do
       call pgmtext('L',0.25,0.5,0.5,'0')
       call pgmove(0.0,0.0)
       call pgdraw(0.05,0.0)
       call pgmtext('L',0.25,0.0,0.5,'-180')
       call pgmove(0.0,1.0)
       call pgdraw(0.02,1.0)
       call pgmtext('L',0.25,1.0,0.5,'180')
       call pglabel('Visibility','Degrees',' ')

       call pgvport(0.1,0.8,0.55,0.85)
       call pgwindow(0.0,1.0,0.0,1.0)
       call pgmove(0.0,0.0)
       call pgdraw(0.0,1.0)
       call pgmove(0.0,0.0)
       call pgdraw(1.0,0.0)
       call pgrnge(0.0,max_vis_amplitude,y1,y2)
       max_vis_amplitude = pgrnd(y2,2)
       call pgwindow(0.5,float(max_vis)+0.5,y1,max_vis_amplitude)
       call pgmove(0.0,0.0)
       do n=1,max_vis
         vis_amplitude = abs(data(n))
         call pgdraw(float(n-1),vis_amplitude)
         call pgdraw(float(n),vis_amplitude)
       end do
       call pgwindow(0.0,1.0,0.0,1.0)
       do n=0,max_vis,100
         write(string(1:4),'(I4)') n
         x1=float(n)/float(max_vis)
         y1=0.0
         y2=0.05
         call pgmtext('B',1.25,x1,0.5,string(1:4))
         call pgmove(x1,y1)
         call pgdraw(x1,y2)
       end do
       call pgmtext('L',0.25,0.0,0.5,'0.0')
       string = ' '
       call pgmove(0.0,1.0)
       call pgdraw(0.02,1.0)
       call chr_chrtoc(max_vis_amplitude,string,ls)
       call pgmtext('L',0.25,1.0,0.5,string(1:ls))
       call pglabel(' ',' ',local_title(1:len_title))

       call pgebuf
       call pgend

C report any errors
       if (s.ne.0) call cal_wrerr( s, 'in subroutine cal_gtvis_display')

       end
