C+ cal_gt_undelete

       subroutine cal_gt_undelete(s)
C      ---------------------------
C
C Mark a record in the calibration gain table as deleted
C
C Returned:  error status

       integer       s

C The user is prompted for a record number in the current gains table --
C the record is then marked as deleted by setting the time in
C the gain table index file to -1
C
C GGP 27 June 2001 - new-style gt files
C-

       include '/mrao/post/include/global_constants.inc'
       include '/mrao/post/include/cal_common.inc'
       include '/mrao/post/include/cal_control.inc'
       include '/mrao/post/include/cal_solution.inc'
       include '/mrao/post/include/gt_include.inc'
       include '/mrao/post/include/post_sys_pars.inc'

        integer     i, datim(6)
C   unit number of gains table
       integer      u_index, u_gains
C   block size and print control for io_operan routine
       integer      bsize, iprint
C   word count for io_rdfile routine
       integer      nwd
C   record counter
       integer      num_rec

C check status on entry
       if (s.ne.0) return

C open the gains table file and update control block

       iprint = 0
       nwd = gt_max_entries
       bsize = 4*gt_blocksize
       call io_operan(u_index, RT_gt_index, 'WRITE', bsize, iprint, s)
       call io_rdfile(u_index, 1, gt_index, nwd, s)

* 1st word = count
       num_rec = gt_num_rec     ! default = last one
       if (s.ne.0) goto 999
       call io_geti('Record number : ','*',num_rec,s)
       if (s.eq.0) then
         if (gt_index(num_rec+1) .ne. -1) then
                call io_wrout('This record not deleted?')
                goto 999
         endif
         call io_operan(u_gains, RT_gains, 'READ', bsize, iprint, s)
         nwd = gt_blocksize
         call io_rdfile(u_gains, num_rec, gt_io, nwd, s)
         call io_close (u_gains, s)
         if (s .ne. 0) goto 999
              write (*,*) gt_itim1, gt_idat1
           do i=1,3
             datim(i)   = gt_itim1(i)
             datim(i+3) = gt_idat1(i)
           end do
         call util_datint(datim, gt_index(num_rec+1))
         nwd = gt_max_entries
         call io_wrfile(u_index, 1, gt_index, nwd, s)
         if (s.ne.0) goto 999
       end if

       call io_close (u_index, s)
       return

999     if (s.ne.0) then
           call cal_wrerr(s, 'in subroutine cal_gt_delete')
           close (u_index)
           close (u_gains)
        endif
       end
