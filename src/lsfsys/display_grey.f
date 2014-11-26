C
C+display_grey
C
      SUBROUTINE display_grey (lsf_num, true_data, s)

C
C     Display spacings on (/xwindow) grey-scale device
C
C     Given:
C         Logical sample file number
              integer*4           lsf_num
C         Flag set .true. if real (not model) data is to be plotted
              logical             true_data
C
C     Returned:
C         Status variable - must be zero on entry
              integer             s
C
C The logical sample file is displayed as a grey-scale image ordered
C with the Y-axis as buffer number (time) and the  X-axis as
C spacing index number.
C
C If the request is for a long buffer (> 512), the display is split up
C Phases now in degrees
C mod GP  28 Mar 2003
C-

C=======================================================================
C
C Global includes
C
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/include/constants.inc'
      include  '/mrao/post/include/global_constants.inc'
      include  '/mrao/include/iolib_functions.inc'

C
C     Local constant and variable declarations
C
C     Constants
C         The maximum number of spacings that can be displayed
              integer         max_sp_plot
              parameter      (max_sp_plot = 512)
C         The maximum number of sample buffers
              integer         max_sa_plot
              parameter      (max_sa_plot = 512)

C     Variables, equivalences and commons
C         General counter
              integer         i
C         Sample file logical unit number
              integer         sf_lun
C         Spacing list and string description.
              integer         sp_list (max_vis)
              character*(80)  list
C         Sidereal times for each sample
              integer         sid
              real            time (max_samp)
              integer         buffer_index (max_samp)
              real            rhour
              integer         hh, mm
C         Amplitude
              real            amplitude
C         type of display (amp,phase,cos,sin)
              character       image_opt(4)*9,image_type*9
C         Cursor defined flag and cursor position and input
              logical         cursor_defined
              real            xpos, ypos
              integer         istat, length
              integer         pgcurs
              character*4     response
              character*1     char

              character*40    plot_device

C         List of visibilities returned from get_vis_buffer
              complex         vis_list (max_vis)
C         Number of LSF buffers to plot
              integer         num_buff

* request
              integer   start_buff, end_buff
* First, current and last LSF buffers to display this time
              integer         first_buff, buffer, last_buff
* split into no_plot screens-full
              integer   no_plots
C         The number of spacings in the current LSF
              integer         total_sp
C         Current spacing number
              integer         spacing
C         Plot type for plot_complex
              integer         plot_type
C         Spacing designation
              integer         isp, iba, ich, iae1, iae2
              character*1     sub_bands(5)
C         Image array
              real            image_data (max_sp_plot*max_sa_plot)
              real            image_max, image_min
C         Transformation matrix and range of data to display
              real            trans(6), black, white

C    Place data in work space
              include '/mrao/post/include/post_work_array.inc'
              integer      i_point_1
              parameter   (i_point_1 = 1)
              equivalence (post_work(i_point_1), sp_list)
              integer      i_point_2
              parameter   (i_point_2 = i_point_1 + max_vis)
              equivalence (post_work(i_point_2), vis_list)
              integer      i_point_3
              parameter   (i_point_3 = i_point_2 + 2*max_vis)
              equivalence (post_work(i_point_3), time)
              integer      i_point_4
              parameter   (i_point_4 = i_point_3 + max_samp)
              equivalence (post_work(i_point_4), buffer_index)
              integer      i_point_5
              parameter   (i_point_5 = i_point_4 + max_samp)
              equivalence (post_work(i_point_5), image_data(1))


              data   sub_bands  / 'A', 'B', 'C', 'D', 'E' /
              data   image_opt  /'Amplitude','Phase','Cosine','Sine'/

C Subroutine initialisation
C -------------------------
C
      if  (s .ne. 0)  return

      plot_type = 0
      call lsf_enq_sf (lsf_num, sf_lun, i, s)
      plot_device = '/xwindow'


C Main Code

 100      s = 0

          call io_getopt('Image type : ','Amplitude',image_opt,4,
     *                 image_type,s)

  110     call get_spacings (sf_lun, 'Spacing list : ', 'All',
     *                       list, sp_list, max_vis, total_sp, s)
          call lsf_set_spacings (lsf_num, total_sp, sp_list, 2, s)
          if (s .ne. 0) goto 9999
          if (total_sp .gt. max_sp_plot) then
              write (*, '(X,A,I4)')
     *           'Number of spacings must be less than ',max_sp_plot
              goto 110
          end if

      call lsf_get_range (lsf_num, first_buff, last_buff, s)

        no_plots = 1 + (last_buff - first_buff)/max_sa_plot
        if (no_plots .gt. 1) then

          write (*, '(x,a,i2,a)') 'Split into ',no_plots,' sections'

        endif

        start_buff = first_buff

      if  (s .ne. 0)  goto 9999

C loop and read visibility data


 200  num_buff  = 0
      buffer    = start_buff
      end_buff  = min(start_buff+max_sa_plot-1, last_buff)
      write (*, '(x,a,i5,i5)')
     *             'sample buffer range ', start_buff, end_buff
      image_max = -1.0E+30
      image_min =  1.0E+30

      do while (buffer .le. end_buff)
          num_buff = num_buff + 1
          call lsf_set_buffer (lsf_num, buffer, s)

          if (true_data) then
              call lsf_get_vis  (lsf_num,max_vis,vis_list(1),total_sp,s)
          else
              call lsf_get_model(lsf_num,max_vis,vis_list(1),total_sp,s)
          end if

          do spacing = 1,total_sp

            if (image_type.eq.'Amplitude') then
              image_data(spacing+(num_buff-1)*total_sp) =
     *            abs(vis_list(spacing))
            elseif (image_type.eq.'Phase') then
              if (real(vis_list(spacing)).ne.0.) then
                image_data(spacing+(num_buff-1)*total_sp) =
     *            atan2(imag(vis_list(spacing)),
     *                  real(vis_list(spacing)))/const_d2r
              else
                 image_data(spacing+(num_buff-1)*total_sp) = 0.0
              endif
            elseif (image_type.eq.'Cosine') then
              image_data(spacing+(num_buff-1)*total_sp) =
     *            real(vis_list(spacing))
            elseif (image_type.eq.'Sine') then
              image_data(spacing+(num_buff-1)*total_sp) =
     *                 imag(vis_list(spacing))
            endif
C
            image_max = max(image_max,
     *                      image_data(spacing+(num_buff-1)*total_sp))
            image_min = min(image_min,
     *                      image_data(spacing+(num_buff-1)*total_sp))
          end do

          call lsf_get_sid (lsf_num, sid, s)
          if  (s .ne. 0)  goto 9999
          time(num_buff) = float (sid) /36000.0
          buffer_index(num_buff) = buffer
          buffer = buffer + 1

      end do

      black = image_min
      white = image_max
      call io_getr('Black-level : ','*',black,s)
      call io_getr('White-level : ','*',white,s)
      if (s.ne.0) goto 9999


C transformation matrix
      trans(1) = 0.0
      trans(2) = 1.0
      trans(3) = 0.0
      trans(4) = 0.0
      trans(5) = 0.0
      trans(6) = 1.0

C open device and initialise
      call plot_begin (plot_device, s)
      call pmadvance (s)
      call pgbbuf
      call pgvport(0.05,0.95,0.05,0.95)
      call pgwindow(1.0,real(total_sp),1.0,real(num_buff))

      call io_setexc (.false., s)
      call pggray (image_data, total_sp, num_buff,
     *                         1, total_sp, 1, num_buff,
     *                         white, black, trans     )
c    *                         black, white, trans     )
      call io_setexc (.true., s)
      call pgebuf
      call pgmtext('B',1.25,0.5,0.5,'Spacing, s-b, ch index')
      call pgmtext('L',0.25,0.5,0.5,'ST ->')

C if device accepts cursor I/O then enable use of the cursor
      if (s.ne.0) goto 9999
      call pgqinf('CURSOR',response,length)
      cursor_defined = response.eq.'YES'
      if (cursor_defined) then
        char = 'Q'
        xpos = total_sp/2
        ypos = num_buff/2
        write (*,*)
     *   'Q to quit, else position cursor and type any other char'
        istat = pgcurs(xpos,ypos,char)
        call chr_chucas(char)
        do while (char.ne.'Q'.and.char.ne.'q')
          call lsf_enq_desig (lsf_num, nint(xpos), isp, iba, ich, s)
          call lsf_enq_ae_vis (lsf_num, nint(xpos), iae1, iae2, s)
          amplitude = image_data(nint(xpos)+(nint(ypos)-1)*total_sp)
          rhour = time(nint(ypos))
          hh = int(rhour)
          mm = int(60*(rhour - hh))

          write (*, 1000) isp, sub_bands(iba), ich, iae1, iae2,
     *                    image_type, amplitude,
     *                    hh, mm, buffer_index(nint(ypos))

 1000 format (1x, 'spacing', i4, A1, ':', i1, '  ae ', i2, '/', i2,
     *        2x, a, ':', 1pe12.3,
     *        2x, 'ST ',i2.2, 1x, i2.2, 2x, 'samp buff ', i4)
          istat = pgcurs(xpos,ypos,char)
          call chr_chucas (char)
        end do
      end if

      call pgend
      if  (s .ne. 0)  goto 9999
      if (end_buff .lt. last_buff) then
        start_buff = end_buff + 1
        if (io_yesno('continue ... ?', 'yes', s)) goto 200
        return
      endif

      return


C Error Handling
C --------------

 9999 continue
      if  (s .ne. usr_break)  then
          call lsf_wrerr (s, 'in subroutine DISPLAY_GREY')
      end if
      return
      end
C
C
C
