C+OPEN_SOURCE

      subroutine open_source ( lun, src_num, buff_size, s )
C
C     Opens a source for a given, open, sample file.
C
C     Given:
C         Logical unit number of sample file.
              integer     lun
C         Source number in sample file.
              integer     src_num
C         Buffer size in pages (2048 bytes)
              integer     buff_size
C
C     Returned:
C         Status - must be zero on entry
              integer     s
C
C     Sets up the sample file packing parameters for a particular source
C     number in the given sample file. Does this by reading the control
C     tables from the file opened on unit LUN and setting up the
C     details of sample packing in common block SF_PACK, which is then
C     saved in the save buffer.
C
C     If buff_size is less than one, a default of 32 pages is taken.
C
C     If the file is a physical sample file the ionospheric correction
C     control tables are read.
C
C     S = 0 for successful return, otherwise errcode.
C
C     NPR,    October 1987.
C-
C
      include '/mrao/post/include/samplib_errors.inc'
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/src_pack.inc'
      include '/mrao/post/include/sf_buffer.inc'
      include '/mrao/post/include/sf_pack.inc'
      include '/mrao/post/include/sf_save.inc'

      integer     i

      if (s.ne.0) return

      call read_ct( lun, s )
      call enq_sftype( lun, sf_type, s )
      call enq_src_pack( lun, src_num, src_pack, s )
      if (s .ne. 0) goto 999

      sf_lun     = lun
      sf_src_num = src_num

C    Copy from src_pack to sf_pack
      sf_first_samp_ptr = src_samp_ptr
      samp_len   = src_samp_len
      data_type  = src_data_type
      start_rt   = src_start_rt
      length_rt  = src_length_rt
      start_vis  = src_start_vis
      sf_num_vis = src_max_vis
      amp_factor = src_amp_factor
      start_time = src_start_time
      stop_time  = src_stop_time
      interp_type= src_interp_type
      num_samp   = src_num_samp
      start_mon  = src_start_mon
      length_mon = src_length_mon

      if ( (ct_vers.eq.0 .or. ct_vers.eq.2) .and. sf_type.eq.1 ) then
          start_ion         = start_mon + length_mon
          length_ion        = 1 + int((samp_len-start_ion)/4)*4
          length_ion        = min( length_ion, 32*4+1 )

C         Set up ionospheric correction redtape.
          call read_ion_ct ( lun, s )
      else
          start_ion         = 0
          length_ion        = 0
      end if

C     Now set up the file buffer
      buffer_ptr  = 1
      i           = 1
      do while (sf_save(lun_ptr,i).gt.0 .and. i.le.max_sf)
          buffer_ptr = buffer_ptr + sf_save(length_ptr,i)
          i = i + 1
      enddo

      if (buff_size .ge. 1 ) then
          buffer_len = page * buff_size
      else
          buffer_len = page * 32
      end if

      if ((buffer_ptr-1+buffer_len) .gt. read_buff_len ) then
          s = no_sfbuffer
          goto 999
      end if

      data_offset = 0
      update_flag = 0
      first_samp  = 0
      curr_samp   = 0
      last_samp   = 0

      block_size  = page

      call put_sf_pack( s )

      if (s.ne.0) goto 999
      return

 999  call smp_wrerr( s, 'in subroutine OPEN_SOURCE' )

      end
