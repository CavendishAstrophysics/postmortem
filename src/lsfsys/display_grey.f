C
C+display_grey
C
      SUBROUTINE display_grey( lsf_num,
     *                         true_data,
     *                         s                      )

C
C     Display spacings on the lexidata or other grey-scale device
C
C     Given:
C         Logical sample file number
              integer*4           lsf_num
C         Flag set .true. if real (not model) data is to be plotted
              logical             true_data
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C The logical sample file is displayed as a grey-scale image ordered
C with the Y-axis as buffer number (time) and the  X-axis as
C spacing index number.
C
C-
C=======================================================================
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/post/include/global_constants.inc'

C
C     Local constant and variable declarations
C
C     Constants
C         The maximum number of spacings that can be displayed
              integer         max_sp_plot
              parameter     ( max_sp_plot = 512 )
C         The maximum number of sample buffers
              integer         max_sa_plot
              parameter     ( max_sa_plot = 512 )

C     Variables, equivalences and commons
C         General counter
              integer         i
C         Sample file logical unit number
              integer         sf_lun
C         Spacing list and string description.
              integer         sp_list( max_vis )
              character*(80)  list
C         Sidereal times for each sample
              integer         sid
              real            time( max_samp )
              integer         buffer_index( max_samp )
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
C         Plot device
              character*40    plot_device
C         List of visibilities returned from get_vis_buffer
              complex         vis_list( max_vis )
C         Number of LSF buffers to plot
              integer         num_buff
C         First, current and last LSF buffer number to display.
              integer         first_buff, buff_num, last_buff
C         The sampling rate to use.
              integer         samp_rate
C         The number of spacings in the current LSF
              integer         num_spac
C         Current spacing number
              integer         spac_num
C         Plot type for plot_complex
              integer         plot_type
C         Spacing designation
              integer         isp, iba, ich, iae1, iae2
              character*1     sub_bands(5)
C         Image array
              real            image_data( max_sp_plot*max_sa_plot )
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
              equivalence (post_work(i_point_5), image_data(1) )

C    Data
              data   sub_bands  / 'A', 'B', 'C', 'D', 'E' /
              data   image_opt  /'Amplitude','Phase','Cosine','Sine'/

C Subroutine initialisation
C -------------------------
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

      plot_type = 0
      call lsf_enq_sf( lsf_num, sf_lun, i, s )

C
C Main Code
C ---------
C
  100 continue
          s = 0
C
          call io_getopt('Image type : ','Amplitude',image_opt,4,
     *                 image_type,s)
C
  110     call get_spacings( sf_lun, 'Spacing list : ', 'All',
     *                       list, sp_list, max_vis, num_spac, s )
          call lsf_set_spacings( lsf_num, num_spac, sp_list, 2, s )
          if (s .ne. 0) goto 9999
          if (num_spac .gt. max_sp_plot) then
              write( *, '(X,A,I4)')
     *           'Number of spacings must be less than ',max_sp_plot
              goto 110
          end if

      write (*,'(X,A,i5)')
     *      'Grey-scale maximum number of samples is ',max_sa_plot
      call lsf_get_range( lsf_num, first_buff, last_buff, s )
      samp_rate = (last_buff-first_buff)/max_sa_plot + 1
      samp_rate = max( 1, samp_rate )
      if (samp_rate.gt.1) then
        write (*,*) 'Sampling data in time '
      end if
      if ( s .ne. 0 ) goto 9999

C loop and read visibility data
      num_buff  = 0
      buff_num  = first_buff
      image_max = -1.0E+30
      image_min =  1.0E+30
      do while (buff_num .le. last_buff)
          num_buff = num_buff + 1
          call lsf_set_buffer( lsf_num, buff_num, s )
          if (true_data) then
              call lsf_get_vis(   lsf_num,
     *                            max_vis,
     *                            vis_list( 1 ),
     *                            num_spac,
     *                            s           )

          else
              call lsf_get_model( lsf_num,
     *                            max_vis,
     *                            vis_list( 1 ),
     *                            num_spac,
     *                            s           )

          end if

          do spac_num = 1,num_spac
C
          if (image_type.eq.'Amplitude') then
            image_data(spac_num+(num_buff-1)*num_spac) =
     *            abs(vis_list(spac_num))
          elseif (image_type.eq.'Phase') then
            if (real(vis_list(spac_num)).ne.0.) then
               image_data(spac_num+(num_buff-1)*num_spac) =
     *            atan2(imag(vis_list(spac_num)),
     *                  real(vis_list(spac_num)))
            else
               image_data(spac_num+(num_buff-1)*num_spac) = 0.0
            endif
          elseif (image_type.eq.'Cosine') then
            image_data(spac_num+(num_buff-1)*num_spac) =
     *            real(vis_list(spac_num))
          elseif (image_type.eq.'Sine') then
            image_data(spac_num+(num_buff-1)*num_spac) =
     *                 imag(vis_list(spac_num))
          endif
C
            image_max = max(image_max,
     *                      image_data(spac_num+(num_buff-1)*num_spac))
            image_min = min(image_min,
     *                      image_data(spac_num+(num_buff-1)*num_spac))
          end do

          call lsf_get_sid(   lsf_num, sid, s )
          if ( s .ne. 0 ) goto 9999
          time(num_buff) = float( sid )/36000.0
          buffer_index(num_buff) = buff_num
          buff_num = buff_num + samp_rate

      end do

      black = image_min
      white = image_max
      call io_getr('Black-level : ','*',black,s)
      call io_getr('White-level : ','*',white,s)
      if (s.ne.0) goto 9999

C prompt for PGPLOT device type
c     call io_getplt('Graphics device : ','/xwindow',plot_device,s)
      plot_device = '/xwindow'

C setup transformation matrix
      trans(1) = 0.0
      trans(2) = 1.0
      trans(3) = 0.0
      trans(4) = 0.0
      trans(5) = 0.0
      trans(6) = 1.0

C open device and initialise
      call plot_begin( plot_device, s )
      call pmadvance( s )
      call pgbbuf
      call pgvport(0.1,0.9,0.1,0.9)
      call pgwindow(1.0,real(num_spac),1.0,real(num_buff))

      call io_setexc( .false., s )
      call pggray( image_data, num_spac, num_buff,
     *                         1, num_spac, 1, num_buff,
     *                         white, black, trans      )
c    *                         black, white, trans      )
      call io_setexc( .true., s )
      call pgebuf
      call pgmtext('B',1.25,0.5,0.5,'Spacing (/sub-band,channel) Index')
      call pgmtext('L',0.25,0.5,0.5,'ST (hrs)')

C if device accepts cursor I/O then enable use of the cursor
      if (s.ne.0) goto 9999
      call pgqinf('CURSOR',response,length)
      cursor_defined = response.eq.'YES'
      if (cursor_defined) then
        char = 'Q'
        xpos = num_spac/2
        ypos = num_buff/2
        write (*,*)'Position cursor and type any character Q=quit'
        istat = pgcurs(xpos,ypos,char)
        call chr_chucas(char)
        do while (char.ne.'Q'.and.char.ne.'q')
          call lsf_enq_desig( lsf_num, nint(xpos), isp, iba, ich, s )
          call lsf_enq_ae_vis( lsf_num, nint(xpos), iae1, iae2, s )
          amplitude = image_data(nint(xpos)+(nint(ypos)-1)*num_spac)
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
          call chr_chucas( char )
        end do
      end if

      call pgend
      if ( s .ne. 0 ) goto 9999

      return


C Error Handling
C --------------

 9999 continue
      if ( s .ne. usr_break ) then
          call lsf_wrerr( s, 'in subroutine DISPLAY_GREY' )
      end if
      return
      end
C
C
C
