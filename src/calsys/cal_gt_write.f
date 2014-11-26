C+ cal_gt_write

       subroutine cal_gt_write(csf_name, s)
C      --------------------------------------
C
C Write the current gains table to the permanent gain table file
C
C Given:
C   name of the calibration sample file
       character*(*) csf_name
C Returned:
C   error status
       integer       s
C
C The current gains table is written to the gain table file.  A check
C is made that a current gains table is available.  The user is required
C to supply their initials and comment information to be added to the
C file.
C
C PA, 23/04/90
C GGP 11 Apr 2000 (io_setacc)
C GGP 13 June 2001 - new gain-file format
C GGP 21 June 2001 - corrected handling of index file
C GGP 25 June 2001 - corrected check on no of entries
C-

       include '/mrao/post/include/global_constants.inc'
       include '/mrao/post/include/cal_control.inc'
       include '/mrao/post/include/cal_common.inc'
       include '/mrao/post/include/cal_solution.inc'
       include '/mrao/post/include/gt_include.inc'
       include '/mrao/post/include/post_sys_pars.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_errors.inc'

C local variables
C   sample file unit number
       integer      sf_lun
C   unit number of gains tables
       integer      u_index, u_gains, u_vis
C   loop counters
       integer      i, ich, isb, iae
C   record counter
       integer      num_rec
C   block size and print control for io_operan routine
       integer      bsize
       integer      iprint
C   word count for io_rdfile, io_wrfile routines
       integer      nwd
C   length of string
       integer      ls
C   date and time array
       integer      datim(6)
C   flag to indicate existence of GT file
*       logical      exists
C   string variable for reporting
       character    string*40
C   user name, execution mode, terminal number for io_enqexe call
       character    user*16
       integer      mode, termno

C check status on entry
       if (s.ne.0) return

C check for currently active gains solution
       if (.not.current_solution) then
         call io_wrout('*** no gain table to save')
         return
       end if

C open the gains table file and update control block

       iprint = 0
       nwd    = gt_max_entries
       bsize  = 4*gt_blocksize
       call io_operan(u_index, RT_gt_index, 'WRITE', bsize, iprint, s)
       call io_rdfile(u_index, 1, gt_index, nwd, s)
       num_rec = gt_num_rec + 1
       gt_num_rec = num_rec

        if (gt_num_rec .ge. gt_max_entries) then
            call io_wrout('Gain table index is full:')
            call io_wrout('parameter max_gt_entries must be changed')
            call io_wrout('and the calib system recompiled')
            goto 999
        endif

       call io_operan(u_gains, RT_gains, 'WRITE', bsize, iprint, s)

       if (s.ne.0) goto 999

C open the sample file of the calibration
       call open_sf(sf_lun, csf_name, 'READ', 0, s)
C read the necessary information from the control tables to construct
C the gain table record
       call enq_gt_rec(sf_lun, gt_record, s)
C close the physical sample file
       call close_sf(sf_lun, s)

C copy solution to the output file
       do iae = 1,max_RT_aes
         do isb = 1,max_subb
           do ich = 1,max_channel
             gt_ae_gains(ich, isb, iae) = ae_gains(ich, isb, iae)
           end do
         end do
       end do

C copy calibration record to the output file
       do i=1,cal_length
         gt_cal_record(i) = cal_record(i)
       end do

       call io_enqexe(user, mode, termno)
       gt_operator = user(1:6)
       call io_wrout('Supply up to 8 lines of comment text')
       do ls=1,8
         gt_comment(ls) = ' '
       end do
       ls = 1
       do while (ls.lt.8 .and. s.eq.0)
         call io_getstr('              > ',' ',gt_comment(ls),s)
         if (chr_lenb(gt_comment(ls)).eq.0) s = USR_BREAK
         ls = ls + 1
       end do
       s = 0
C set flag to indicate the visibility-based solution should be saved
       gt_vis_soln = cal_type.eq.3

C read current date and time for this record
       call util_enqdat(gt_date)
       call util_enqtim(gt_time)

C write the new records
       do i=1,3
         datim(i) = gt_itim1(i)
         datim(i+3) = gt_idat1(i)
       end do
       call util_datint(datim, gt_index(num_rec+1))

C test for visibility-based solution
       if (gt_vis_soln) then
C .. output a visibility-gains solution
         iprint = 0
         nwd = 2560
         bsize = 4*2560
         call io_operan(u_vis, RT_v_gains, 'WRITE', bsize, iprint, s)
         call io_rdfile(u_vis, 1, gt_iovis, nwd, s)
         gt_vis_rec = gt_iovis(1)+1
         gt_iovis(1) = gt_vis_rec
         call io_wrfile(u_vis, 1, gt_iovis, nwd, s)
         do i=1,max_vis
           gt_vis_gains(i) = vis_gains(i)
         end do
         call io_wrfile(u_vis, gt_vis_rec+1, gt_iovis, nwd, s)
         call io_close (u_vis, s)
       endif

       call io_wrfile(u_index, 1, gt_index, gt_max_entries, s)
       call io_wrfile(u_gains, num_rec, gt_io, gt_blocksize, s)

C report writing of record and set cmd-language parameter
       if (gtopt_report_write) then
         string = ' '
         write(string,'(A,I8)') '.. new gt-record written = ',num_rec
         call io_wrout(string(1:chr_lenb(string)))
       end if
       string = ' '
       write(string,'(I8)') num_rec
       call cmd_setparam('%GT-RECORD', string(1:chr_lenb(string)), s)


999    if (s.ne.0) call cal_wrerr(s, 'in subroutine cal_gt_write')


       call io_close (u_index, s)
       call io_close (u_gains, s)

       end
