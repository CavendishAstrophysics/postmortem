C+ cal_gt_delete

       subroutine cal_gt_delete( s )
C      -----------------------------
C
C Mark a record in the calibration gain table as deleted
C
C Returned:
C   error status
       integer       s
C
C The user is prompted for a record number in the current gains table --
C the record is then marked as deleted by modifiying the information in
C the gain table file header record.
C
C PA, 8/5/90
C
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
       logical      exists

C check status on entry
       if (s.ne.0) return

C open the gains table file and update control block
       iunit = 0
       inquire ( file=RT_gt_file, exist = exists )
       if (.not.exists) then
         call io_wrout(
     *         '*** GT file does not exist -- no record to delete')
         return
       end if
       iprint = 0
       nwd = 1024
       bsize = 4*1024
       call io_operan( iunit, RT_gt_file, 'WRITE', bsize, iprint, s )
       call io_rdfile( iunit, 1, gt_times, nwd, s )
       num_rec = gt_num_rec
       if (s.ne.0) goto 999
       call io_geti('Record number : ','*',num_rec,s)
       if (s.eq.0) then
         gt_times(num_rec+1) = -1
         call io_wrfile( iunit, 1, gt_times, 1024, s )
         if (s.ne.0) goto 999
       end if

       call io_close (iunit,s)
       return
999    if (s.ne.0) call cal_wrerr( s, 'in subroutine cal_gt_delete' )
       close (iunit)

       end
