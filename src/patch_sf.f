C
       integer status
       status=0
       call io_initio
       call io_initlf(.true.)
       call io_setesc(.true.)
       call patch_sf(status)
       call io_setesc(.false.)
       write(*,*)
       end



*+
       subroutine patch_sf (status)
C
C  Patch a sample file
C
*-
       integer    status
c
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/post/include/global_constants.inc'
       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/ctab_pack.inc'
       include '/mrao/post/include/samplib_errors.inc'
c
       character  list*80, file*64
       integer    vis_list(max_vis)
       integer    i, ii, is, isamp, isamp1, isamp2, num_samp
       integer    n, nae, ns, nvis, bad_samp, i1, i2
       integer    ifile, iout, iae1, iae2, sid_time, sid_time1
       complex    vis(max_vis)
       real*8     ra, dec
c
       if (status.ne.0) return
c
       call io_enqout(iout)
c
c  Get sample file name
c
       file = '/data/ryle/'
       call io_getfil('sample file : ', ' ', file, status)
c
c  Open the sample file
c
       call open_sf(ifile, file, 'WRITE', 1, status)
       call open_source(ifile, 1, 0, status)
       call enq_numsamp(ifile, 1, num_samp, status)
c
c  Patch sidereal time of first sample
c
       if (status.eq.0) then
c
         write(iout,*)
         call read_rt(ifile,1,1,ra,dec,sid_time1,status)
         write(iout,'(I5,2F8.4,I8)') 1,ra,dec,sid_time1
         call read_rt(ifile,1,2,ra,dec,sid_time,status)
         write(iout,'(I5,2F8.4,I8)') 2,ra,dec,sid_time
         sid_time1 = sid_time - 320
         call write_rt(ifile,1,1,ra,dec,sid_time1,status)
         write(iout,'(I5,2F8.4,I8)') 1,ra,dec,sid_time1
c
       endif
c
       if (status.eq.USR_BREAK) then
         status=0
       else if (status.ne.0) then
         call io_wrerr(status,'in routine patch_sf')
       endif
c
       call close_source(ifile,1,status)
       call close_sf(ifile,status)
c
       end
C+WRITE_RT

      subroutine write_rt ( lun,
     *                      src_num,
     *                      samp_num,
     *                      ra, dec,
     *                      sid_time,
     *                      s           )
C
C     Writes redtape information for a single sample.
C
C     Given:
C         The logical unit number of the sample file.
              integer        lun
C         The number of the remove or calibration source to be read.
              integer        src_num
C         The number of the sample to be read.
              integer        samp_num
C
C     Returned:
C         Coordinates of phase centre.
              real*8         ra, dec
C         Sidereal time for sample (1/10ths second)
              integer        sid_time
C         Status variable - must be zero on entry otherwise error.
              integer        s
C
C     Writes the phase centre and sidereal time associated with a
C     given sample.
C
C     The details of the sample redtape can be found on the file
C     SAMP-RT:INCL.  The file must be opened via OPEN_SF, OPEN_RF or
C     OPEN_CF before this routine is called.
C
C     DJT,    October 1987
C-

C     Global includes -
C
       include '/mrao/post/include/samp_rt.inc'
       include '/mrao/post/include/sf_pack.inc'
       include '/mrao/post/include/sf_buffer.inc'

C     Local variable declarations -
C
C         Loop counter
              integer       i
C         Pointers to positions in file buffer
              integer       offset, rt_offset

      if ( s .ne. 0 ) return

      call read_buffer ( lun, src_num, samp_num, s )
      if ( s .ne. 0 ) goto 999

      num_vis_corr  = sf_num_vis
      samp_ra       = ra
      samp_dec      = dec
      samp_sid_time = sid_time

      offset = buffer_ptr + data_offset +
     *         (curr_samp-first_samp)*samp_len - 1

      rt_offset = offset + start_rt - 1
      do 10 i = 1, length_rt
          buffer( rt_offset+i )= samp_rt( i )
   10 continue

      update_flag = update_flag + 1

      return

C     Error Handling -

 999  call smp_wrerr( s, 'in subroutine WRITE_RT' )

      end

