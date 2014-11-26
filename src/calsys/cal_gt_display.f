C
C
C+cal_gt_display

       subroutine cal_gt_display( data, plot_device, s )
C      -------------------------------------------------
C
C Display the contents of the supplied gains table
C
       include '/mrao/post/include/global_constants.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/constants.inc'
C
C      Given:
C        complex data defining the gains table
                  complex    data( max_channel, max_subb, max_RT_aes )
C        default plot device
                  character  plot_device*(*)
C      Returned:
C        error status
                  integer   s
C
C
C The gains table is displayed.  The display is both to the alpha screen
C as a table of those aerial/sub-band combinations without calibration,
C and also a plot of the amplitude and phase corrections for each aerial/
C sub-band/channel combination.
C
C PA 21/05/90
C-
C Local variables
C   output unit number
       integer      iout
C   unit numbers for terminal I/O
       integer      termi, termo
C   counters
       integer      iae, isb, ich, i, ii, n
C   output string and title for plot
       character    string*60, local_title*80
C   length of string
       integer      ls, len_title
C   arrays for data plotting
       real*4       ae_amplitude( max_RT_aes*max_subb*max_channel )
       real*4       ae_phase( max_RT_aes*max_subb*max_channel )
       real*4       max_ae_amplitude
C   range of data for amplitude plot
       real*4       x1, y1, y2
C   PGPLOT functions
       real*4       pgrnd
       integer      nsub


C check status on entry
       if (s.ne.0) return

       call io_enqtio(termi, termo)

C prompt for title for plot and table
       call io_getstr('Plot/table title : ','Calibration Factors',
     *             local_title, s )
       len_title = chr_lenb(local_title)
C open unit for output
       call io_opeout( iout, s )

C output title and table of aerial/channel combinations without calibration
       write (iout,100) local_title(1:len_title)
100    format( 1x/1x,A/
     *            1x,'----------------------------------------',
     *               '----------------------------------------'/1x/
     *            1x,'Aerial : Subband / Channel'/
     *            1x,' No.   : A 12345678 ',
     *                       ' B 12345678 ',
     *                       ' C 12345678 ',
     *                       ' D 12345678 ',
     *                       ' E 12345678 '/
     *            1X,'----------------------------------------',
     *               '----------------------------------------')
       ii = 0
       max_ae_amplitude = 0.0
       do iae = 1,max_RT_aes
         string = ' '
         do isb = 1,max_subb
           do ich=1,max_channel
             ii = ii + 1
             ae_amplitude(ii) = abs(data(ich,isb,iae))
             max_ae_amplitude = max(max_ae_amplitude,ae_amplitude(ii))
             ae_phase(ii) = atan2(imag(data(ich,isb,iae)),
     *                            real(data(ich,isb,iae)))/const_d2r
             if (data(ich,isb,iae).eq.(1.0,0.0)) then
               i = (isb-1)*12 + ich + 3
               string(i:i) = 'X'
             end if
           end do
         end do
         write(iout,'(1X,I4,''   :'',A60)') iae, string
       end do
       write (iout,*) ' '

C plot amplitude/phase correction factors: plot is of a "histogram" type
       call plot_begin( plot_device, s )
       call pgbbuf
       call pgvport(0.1,0.8,0.1,0.4)
       call pgwindow(0.0,1.0,0.0,1.0)
       call pgmove(0.0,0.0)
       call pgdraw(0.0,1.0)
       call pgmove(0.0,0.0)
       call pgdraw(1.0,0.0)
       call pgwindow(0.0,320.0,-180.0,180.0)
       call pgmove(0.0,0.0)
       call pgdraw(320.0,0.0)
       call pgmove(0.0,0.0)
       do n=1,320
         call pgdraw(float(n-1),ae_phase(n))
         call pgdraw(float(n),ae_phase(n))
       end do
       ii = 0
       call pgwindow(0.0,1.0,0.0,1.0)
       do n=0,319,40
         ii = ii + 1
         write(string(1:1),'(I1)') ii
         x1=float(n)/320.0
         y1=0.0
         y2=0.05
         call pgmtext('B',1.25,x1,0.5,string(1:1))
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
       call pglabel('Aerial (Sub-band/Channel)','Degrees',' ')

       call pgvport(0.1,0.8,0.55,0.85)
       call pgwindow(0.0,1.0,0.0,1.0)
       call pgmove(0.0,0.0)
       call pgdraw(0.0,1.0)
       call pgmove(0.0,0.0)
       call pgdraw(1.0,0.0)
       call pgrnge(0.0,max_ae_amplitude,y1,y2)
       max_ae_amplitude = pgrnd(y2,nsub)
       call pgwindow(0.5,320.5,y1,max_ae_amplitude)
       call pgmove(0.0,0.0)
       do n=1,320
         call pgdraw(float(n-1),ae_amplitude(n))
         call pgdraw(float(n),ae_amplitude(n))
       end do
       ii = 0
       call pgwindow(0.0,1.0,0.0,1.0)
       do n=0,319,40
         ii = ii + 1
         write(string(1:1),'(I1)') ii
         x1=float(n)/320.0
         y1=0.0
         y2=0.05
         call pgmtext('B',1.25,x1,0.5,string(1:1))
         call pgmove(x1,y1)
         call pgdraw(x1,y2)
       end do
       call pgmtext('L',0.25,0.0,0.5,'0.0')
       string = ' '
       call pgmove(0.0,1.0)
       call pgdraw(0.02,1.0)
       call chr_chrtoc(max_ae_amplitude,string,ls)
       call pgmtext('L',0.25,1.0,0.5,string(1:ls))
       call pglabel(' ',' ',local_title(1:len_title))

       call pgebuf
       call pgend

C report any errors
       if (s.ne.0) call cal_wrerr( s, 'in subroutine cal_gt_display')
       call io_close( iout, s )
       call io_setout( termo )

       end
