C
       integer status
       status=0
       call io_initio
       call io_initlf(.true.)
       call io_setesc(.true.)
       call convert_lsf(status)
       call io_setesc(.false.)
       end



*+
       subroutine CONVERT_LSF (status)
C
C  Converts a logical sample file imported to Unix from ND systems
C
C  DJT, 2 December 93
*-
       integer    status
c
       include '/mrao/post/include/lsf_record.inc'
       include '/mrao/post/include/lsflib_errors.inc'
       include '/mrao/post/include/int_chop_record.inc'
c
       character  file*64
       integer    ifile, irec, i
c
       if (status.ne.0) return

c  Get logical sample file name

       call io_getfil('logical sample file : ', ' ', file, status)

c  Open the logical sample file

       call io_nxtlun(ifile,status)
       open (ifile, file=file, access='DIRECT', status='OLD',
     :                         recl=lsf_len*4, iostat=status)

c  Read each lsf definition, convert floating-point variables to IEEE
c  format and rewrite.

       if (status.eq.0) then
         irec = 1
  1      read(ifile, rec=irec, iostat=status) log_samp_file
         if (status.eq.0) then
           call util_rndr64(ref_ra,ref_ra,1)
           call util_rndr64(ref_dec,ref_dec,1)
           call util_rndr64(ref_date,ref_date,1)

c    Convert preprocessing interference chop parameters
           do i = 1,10
             int_chop_record(i) = pre_int_chop_params(i)
           enddo
           if (pre_int_chop_type .eq. 2) then
             call util_rndr32(max_signal,max_signal,1)
             call util_rndr32(clip_limit,clip_limit,1)
           elseif (pre_int_chop_type .eq. 3) then
             call util_rndr32(max_signal,max_signal,1)
             call util_rndr32(max_noise,max_noise,1)
           elseif (pre_int_chop_type .eq. 4) then
             call util_rndr32(max_int_signal,max_int_signal,1)
           elseif (pre_int_chop_type .eq. 5) then
             call util_rndr32(multi_level,multi_level,10)
           elseif (pre_int_chop_type .eq. 6) then
             call util_rndr32(max_signal,max_signal,1)
             call util_rndr32(max_noise,max_noise,1)
           endif
           do i = 1,10
             pre_int_chop_params(i) = int_chop_record(i)
           enddo

c    Convert postprocessing interference chop parameters
           do i = 1,10
             int_chop_record(i) = post_int_chop_params(i)
           enddo
           if (post_int_chop_type .eq. 2) then
             call util_rndr32(max_signal,max_signal,1)
             call util_rndr32(clip_limit,clip_limit,1)
           elseif (post_int_chop_type .eq. 3) then
             call util_rndr32(max_signal,max_signal,1)
             call util_rndr32(max_noise,max_noise,1)
           elseif (post_int_chop_type .eq. 4) then
             call util_rndr32(max_int_signal,max_int_signal,1)
           elseif (post_int_chop_type .eq. 5) then
             call util_rndr32(multi_level,multi_level,10)
           elseif (post_int_chop_type .eq. 6) then
             call util_rndr32(max_signal,max_signal,1)
             call util_rndr32(max_noise,max_noise,1)
           endif
           do i = 1,10
             post_int_chop_params(i) = int_chop_record(i)
           enddo

           write(ifile, rec=irec, iostat=status) log_samp_file
           irec=irec+1
           goto 1
         endif
         status=0
       endif

       close(ifile)

       if (status.ne.0) then
          call io_wrerr(status, 'in subroutine CONVERT_LSF')
       endif

       end
