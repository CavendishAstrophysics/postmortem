

C+WRITE_SAMPLE

      subroutine write_sample ( lun,
     *                          src_num,
     *                          samp_num,
     *                          proc_flag,
     *                          num_vis,
     *                          vis_list,
     *                          vis_buff,
     *                          s          )
C
C     Writes visibilities for a single sample.
C
C     Given:
C         The logical unit number of the sample file.
              integer             lun
C         The number of the remove or calibration source to be written.
              integer             src_num
C         The number of the sample to be written.
              integer             samp_num
C         Processing flag. Valid values : 1 - buffer given as Janskys
C                                         2 - buffer given weighted
              integer             proc_flag
C         The number of visibilities to be written.
              integer             num_vis
C         A list of index numbers for the given visibilities.
              integer             vis_list ( num_vis )
C         Array of visibilities, (cos,sin) pairs.
              complex             vis_buff ( num_vis )
C
C     Returned:
C         Status variable - must be zero on entry otherwise error.
              integer             s
C
C     A low level routine for writing a given set of visibilities as a
C     sample to a sample file.  The file has to be set up via open_sf or
C     open_rf before it is accessed - if not a status of SF_NOTSAV is
C     returned.
C
C     If a visibility is not provided (ie it is ommitted from the
C     spacing list) then it is given the value (1.0,0.0) if the file
C     is a calibration file or (0.0,0.0) for other types of sample files
C
C     DJT,    October 1987.
C-
C     ****************************************************************

C     Global includes -
C
      include  '/mrao/post/include/samp_rt.inc'
      include  '/mrao/post/include/sf_pack.inc'
      include  '/mrao/post/include/sf_buffer.inc'
      include  '/mrao/post/include/samplib_errors.inc'
      include  '/mrao/include/iolib_errors.inc'

C     Local variable declarations -

C         Loop counter
              integer     i, j
C         Pointers to positions in file buffer
              integer     offset, vis_offset, ptr
C         The value of the current visibility
              complex     value
C         Weight and normalisation factor for the visibilities
              real        fact
C         Default value to write if spacing not in spacing list
              complex     def_value

C     ****************************************************************
C
C     Subroutine initialisation
C
      if ( s .ne. 0 ) return

C     Check to see if the required sample is in the file buffer.
C     - if not retrieve it.

      call read_buffer( lun, src_num, samp_num, s )
      if ( s .eq. END_FILE ) s = 0
      if ( s .ne. 0 ) goto 999

      if ( sf_type .eq. 3 ) then
C         Calibration file
          def_value = (1.0, 0.0)
      else
          def_value = (0.0, 0.0)
      end if

      offset = buffer_ptr + data_offset +
     *         (curr_samp-first_samp)*samp_len - 1

C     Write the visibilities according to data type and processing flag

      vis_offset = offset + start_vis - 1

      if (proc_flag .eq. 1) then
          fact = 1.0/amp_factor
      else if (proc_flag .eq. 2) then
          fact = float( samp_wt ) / amp_factor
      else
          s = ILL_PROCFLG
          goto 999
      end if

      j = 1
      if ( data_type .eq. 1 ) then
          do 10 i = 1, num_vis_corr
              ptr = 2*vis_offset + 2*i - 1
              if (j .le. num_vis .and. vis_list(j).eq.i) then
                  value = vis_buff(j)
                  j = j+1
              else
                  value = def_value
              end if
              ibuff(ptr)   =
     *            nint( min(32767.0,max(-32768.0,real( value )/fact)))
              ibuff(ptr+1) =
     *            nint( min(32767.0,max(-32768.0,imag( value )/fact)))
   10     continue
      else if ( data_type .eq. 2 ) then
          do 20 i = 1, num_vis_corr
              ptr = vis_offset + 2*i - 1
              if (j .le. num_vis .and. vis_list(j).eq.i) then
                  value = vis_buff(j)
                  j = j+1
              else
                  value = def_value
              end if
              buffer(ptr)   =
     *           nint(min(2.147483E9,max(-2.147483E9,real(value)/fact)))
              buffer(ptr+1) =
     *           nint(min(2.147483E9,max(-2.147483E9,imag(value)/fact)))
   20     continue
      else if ( data_type .eq. 3 ) then
          do 30 i = 1, num_vis_corr
              ptr = vis_offset + 2*i - 1
              if (j .le. num_vis .and. vis_list(j).eq.i) then
                  value = vis_buff(j)
                  j = j+1
              else
                  value = def_value
              end if
              rbuff(ptr)   = real( value )/fact
              rbuff(ptr+1) = aimag( value )/fact
   30     continue
      else
          s = ILL_DATATYPE
      end if

      if (s.ne.0) goto 999
      update_flag  = update_flag + 1

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C

 999  call smp_wrerr( s, 'in subroutine WRITE_SAMPLE' )

      end
