C+ cal_gt_delete

       subroutine cal_gt_delete(s)
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
C PA, 8/5/90
C GGP 14 June 2001: new gt-file format
C GGP 25 June 2001: length corrected
C-

       include '/mrao/post/include/global_constants.inc'
       include '/mrao/post/include/cal_common.inc'
       include '/mrao/post/include/cal_control.inc'
       include '/mrao/post/include/cal_solution.inc'
       include '/mrao/post/include/gt_include.inc'
       include '/mrao/post/include/post_sys_pars.inc'

C local variables
C   unit number of gains table
       integer      iunit
C   block size and print control for io_operan routine
       integer      bsize
       integer      iprint
C   word count for io_rdfile routine
       integer      nwd
C   record counter
       integer      num_rec
C   flag to indicate existence of GT file
*       logical      exists

C check status on entry
       if (s.ne.0) return

C open the gains table file and update control block

       iprint = 0
       nwd = gt_max_entries
       bsize = 4*gt_blocksize
       call io_operan(iunit, RT_gt_index, 'WRITE', bsize, iprint, s)
       call io_rdfile(iunit, 1, gt_index, nwd, s)
* 1st word = count
       num_rec = gt_num_rec     ! default = last one
       if (s.ne.0) goto 999
       call io_geti('Record number : ','*',num_rec,s)
       if (s.eq.0) then
         gt_index(num_rec+1) = -1
         nwd = gt_max_entries
         call io_wrfile(iunit, 1, gt_index, nwd, s)
         if (s.ne.0) goto 999
       end if

       call io_close (iunit,s)
       return
999    if (s.ne.0) call cal_wrerr(s, 'in subroutine cal_gt_delete')
       close (iunit)

       end
