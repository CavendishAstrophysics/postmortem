C+READ_SAMPLE

      subroutine read_sample ( lun,
     *                         src_num,
     *                         samp_num,
     *                         proc_flag,
     *                         num_vis,
     *                         vis_list,
     *                         vis_buff,
     *                         s            )
C
C     Returns visibilities for a single sample.
C
C     Given:
C         The logical unit number of the sample file.
              integer             lun
C         The number of the remove or calibration source to be read.
              integer             src_num
C         The number of the sample to be read.
              integer             samp_num
C         Processing flag. Valid values : 1 - return buffer as Janskys
C                                         2 - return buffer weighted
              integer             proc_flag
C         The number of visibilities to be returned.
              integer             num_vis
C         A list of index numbers for the required visibilities.
              integer             vis_list ( num_vis )
C
C     Returned:
C         Array of visibilities, (cos,sin) pairs.
              complex             vis_buff ( num_vis )
C         Status variable - must be zero on entry otherwise error.
              integer             s
C
C     A low level routine for accessing a given sample from a file. The
C     file has to be set up via OPEN_SF before it is actually accessed
C     - if not, a status of SF_NOTSAV is returned.
C
C     DJT, NPR,   May 1987.
C-
C     ****************************************************************

C     Global includes -
C
      include  '/mrao/post/include/samp_rt.inc'
      include  '/mrao/post/include/sf_pack.inc'
      include  '/mrao/post/include/sf_buffer.inc'
      include  '/mrao/post/include/samplib_errors.inc'

C     Local variable declarations -

C         Loop counter
              integer     i
C         Pointers to positions in file buffer
              integer     offset, vis_offset, rt_offset, ptr
C         Weight and normalisation factor for the visibilities
              real        fact

              intrinsic   btest

C     ****************************************************************
C
C     Subroutine initialisation
C
      if ( s .ne. 0 ) return

      call read_buffer ( lun, src_num, samp_num, s )
      if ( s .ne. 0 ) goto 999

      offset = buffer_ptr + data_offset +
     *         (curr_samp-first_samp)*samp_len - 1

      rt_offset = offset + start_rt - 1
      do 10 i = 1, length_rt
          samp_rt( i )= buffer( rt_offset+i )
   10 continue

      vis_offset = offset + start_vis - 1

      if (proc_flag .eq. 1) then
          fact = 1.0/amp_factor
      else if (proc_flag .eq. 2) then
          fact = float( samp_wt ) / amp_factor
      else
          s = ILL_PROCFLG
          goto 999
      end if

      if ( data_type .eq. 1 ) then
          do 20 i = 1, num_vis
              ptr = 2*vis_offset + 2*vis_list(i)-1
              vis_buff(i) = fact*cmplx( ibuff(ptr), ibuff(ptr+1) )
   20     continue
      else if ( data_type .eq. 2 ) then
          do 30 i = 1, num_vis
              ptr = vis_offset + 2*vis_list(i)-1
              vis_buff(i) = fact*cmplx( buffer(ptr), buffer(ptr+1) )
   30     continue
      else if ( data_type .eq. 3 ) then
          do 40 i = 1, num_vis
              ptr = vis_offset + 2*vis_list(i)-1
              vis_buff(i) = fact*cmplx( rbuff(ptr), rbuff(ptr+1) )
   40     continue
      else
          s = ILL_DATATYPE
          goto 999
      end if

C     Set status if bad sample or sample weight zero

      if (btest(samp_status,2) .or. samp_wt.eq.0 ) then
         s = BAD_SAMPLE
      endif

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C

 999  call smp_wrerr( s, 'in subroutine READ_SAMPLE' )

      end
