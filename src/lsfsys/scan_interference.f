C
C+scan_interference
C
      SUBROUTINE scan_interference( lsf_num,
     *                              plot_device,
     *                              s                 )

C
C     Checks the level of interference clipping in the LSF.
C
C     Given:
C         Logical sample file number
              integer*4           lsf_num
C         PGPLOT device
              character*(*)       plot_device
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Runs through the current logical sample file displaying data
C     sample by sample.  Sample noise is plotted if required.
C
C     PA
C     PJW, 24/09/90: plotting of sample noise added
C-
C=======================================================================
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/chrlib_functions.inc'
      include  '/mrao/include/iolib_functions.inc'
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/post/include/global_constants.inc'

C
C     Variables, equivalences and commons
C         General purpose loop counters
              integer         i
C         Sample file logical unit number
              integer         sf_lun
C         Plot title
              character*(80)  title(4)
C         Spacing list and string description.
              integer         sp_list( max_vis )
              character*(80)  list
C         List of visibilities returned from get_vis_buffer
              complex         vis_list( max_vis )
              real            vis( 2, max_vis )
              equivalence   ( vis(1), vis_list(1) )
C         Number of spacings returned.
              integer         num_spac
C         Current and last LSF buffer to include in statistics
              integer         buff_num, last_buff
C         Sampling rate in LSF buffers.
              integer         samp_rate
C         Totals of rejected visibilities and total visibilities
              integer         null_vis(max_vis), tot_vis(max_vis)
C         Telescope description parameters
              integer         isp, isb, ich, iae1, iae2
C         New and initial output device lun.
              integer         out, old_out
C         Sub bands
              character*1     sub_bands(5)
C         Control switch for plot of sample noise
              logical         noise_plot
C         Temporary store and summation variables
              integer         n, ns
              real            cos_sqr, sin_sqr, max_noise
              real*8          sum_cos, sum_sin, ssq_cos, ssq_sin
              real*8          var_cos, var_sin
              real            noise( max_samp ), samp_no( max_samp )

      common /post/sp_list, vis_list, null_vis, tot_vis, noise, samp_no

C     Define sub bands
              data    sub_bands / 'A', 'B', 'C', 'D', 'E' /

C
C Subroutine initialisation
C -------------------------
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return
      call lsf_enq_sf( lsf_num, sf_lun, i, s )

C Main Code
C ---------
C
      call get_spacings( sf_lun, 'Spacing list : ', 'All',
     *                   list, sp_list, max_vis, num_spac, s )
      call lsf_set_spacings( lsf_num, num_spac, sp_list, 2, s )
50    call lsf_get_range( lsf_num, buff_num, last_buff, s )
      call io_geti( 'Sampling rate : ', '1', samp_rate, s )
      if ( io_yesno( 'Plot of sample noise ? ', 'yes', s ) ) then
          noise_plot = .true.
          max_noise = 1.
          ns = 0
      else
          noise_plot = .false.
      endif
      if (s.ne.0) goto 9999

      if( ( (last_buff - buff_num)/samp_rate) + 1 .gt. max_samp ) then
          call io_wrout(' too many samples - > 2000 ')
          goto 50
      endif

      call io_enqout( old_out )
      call io_opeout( out, s )
      if (s.ne.0) goto 9999

      call lsf_title( lsf_num, list, buff_num, last_buff, title, s )

      do i=1,num_spac
        null_vis(i) = 0
        tot_vis(i)  = 0
      end do

  100 if ((s .ne. 0) .or. (buff_num .gt. last_buff)) goto 1000
          call lsf_set_buffer(lsf_num, buff_num, s )
          call lsf_get_vis( lsf_num, max_vis, vis_list, num_spac, s )

          do i = 1, num_spac
            if (vis_list(i).eq.(0.0,0.0)) then
              null_vis(i) = null_vis(i) + 1
            end if
            tot_vis(i) = tot_vis(i) + 1
          end do
          buff_num = buff_num+samp_rate

          if ( noise_plot ) then
                ns = ns + 1
C ... initialise statistics variables
                sum_cos = 0.0D+0
                sum_sin = 0.0D+0
                ssq_cos = 0.0D+0
                ssq_sin = 0.0D+0
                n       = 0
                do i = 1, num_spac
                    cos_sqr = vis(1,i)*vis(1,i)
                    sin_sqr = vis(2,i)*vis(2,i)
                    if ( (cos_sqr+sin_sqr) .gt. 0.0 ) then
                        sum_cos = sum_cos + vis(1,i)
                        sum_sin = sum_sin + vis(2,i)
                        ssq_cos = ssq_cos + cos_sqr
                        ssq_sin = ssq_sin + sin_sqr
                        n       = n + 1
                    end if
                end do
                samp_no(ns) = buff_num
                if ( n .gt. 0 ) then
                    var_cos = (ssq_cos - sum_cos*sum_cos/n)/n
                    var_sin = (ssq_sin - sum_sin*sum_sin/n)/n
                    noise(ns) = sqrt(var_cos + var_sin)
                    max_noise  = amax1( max_noise, noise(ns) )
                else
                    noise(ns) = 0.
                endif
          endif

          goto 100

1000  continue

      if ( noise_plot ) then

          call plot_begin( plot_device, s )
          call pmadvance( s )
          if ( s .eq. 0 ) then
            call pgvport( 0.1, 0.9, 0.4, 0.8 )
            call plot_setscale( .false., 0.0, max_noise, s )
            call plot_data( ns, samp_no, noise, 1.0, 1.0, s )
            call plot_setscale( .true., 0.0, 0.0, s )
            call pglabel( 'Sample number', 'Sample noise', ' ' )

            call pgvport( 0.1, 0.9, 0.85, 1.0 )
            call pgwindow( 0.0, 100.0, 4.2, -0.2 )
            do i = 1, 4
              call pgtext( 0.0, real(i), title(i) )
            enddo
          endif

C   Restore status if USR_BREAK is detected
          if ( s .eq. USR_BREAK ) s = 0
          call pgend

      endif


      if (s.eq.0) then
          call io_wrout( ' ' )
          do i = 1, 4
              call io_wrout(title(i))
          end do
          write (out,10)
          do i = 1,num_spac
            call enq_vis_desig( sf_lun, sp_list(i), isp, isb, ich, s )
            call enq_ae_vis( sf_lun, sp_list(i), iae1, iae2, s )
            if (null_vis(i).gt.0) then
              write (out,20) isp, sub_bands(isb), ich, iae1, iae2,
     *                       100.0*float(null_vis(i))/float(tot_vis(i)),
     *                       null_vis(i), tot_vis(i)
            end if
            if ( io_attn(s) ) goto 2000
          end do
 10       format(1X/
     *           1X,'Spacing  Aerials ',
     *              '  % visib. ',' No. visib. ','  Total No. '/
     *           1X,'                 ',
     *              '  rejected ','  rejected  ','    visib.  '/
     *           1X,'-----------------',
     *              '-----------','------------','------------')

 20       format(1X,i4,':',a1,':',i1,2i4,2x,f7.3,5x,i5,7x,i5)
          write (out,30) num_spac
 30       format(1X/1X,'Checked ',I4,' visibilities for interference'/
     *              1X)
      end if

2000  if ( out .ne. old_out ) then
          call io_close ( out, s )
          call io_setout( old_out )
      end if

      if ( s .ne. 0 ) goto 9999
      return

C Error Handling
C --------------
C
 9999 continue
          if ( s .ne. usr_break ) then
              call lsf_wrerr( s, 'in subroutine SCAN_INTERFERENCE' )
          end if
          return
      end
C
C
C
C
