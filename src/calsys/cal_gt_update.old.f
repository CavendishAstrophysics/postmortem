C+ cal_gt_update

       subroutine cal_gt_update( s )
C      -----------------------------
C
C Update the gains table by reading data from the GT file
C
C Returned:
C   error status
       integer       s
C
C The user is prompted for the update option, record number in the
C GT file a telescope list and any other relevant data. The current
C gains table is amended.
C
C Record zero is the null record and consists of gains of (1,0).
C
C PA, 13/11/91
C
C-

       include '/mrao/post/include/global_constants.inc'
       include '/mrao/post/include/cal_common.inc'
       include '/mrao/post/include/cal_control.inc'
       include '/mrao/post/include/cal_solution.inc'
       include '/mrao/post/include/gt_include.inc'
       include '/mrao/post/include/post_sys_pars.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'

C local variables
C   unit number of gains tables
       integer      iunit, iunit_vis
C   loop counters
       integer      i, ich, isb, iae
C   lists
       integer      ae_list(max_RT_aes)
       integer      sb_list(max_subb), ch_list(max_channel)
       integer      array(max_RT_aes*max_subb*max_channel)
C   buffer to hold gains for the reference antenna
       complex      ref_gains(max_channel,max_subb)
C   record counter
       integer      num_rec
C   block size and print control for io_operan routine
       integer      bsize
       integer      iprint
C   word count for io_rfile routine
       integer      nwd
C   length of string
       integer      ls
C   aerial list and general string
       character*80 list, string
C   length of input list and internal pointers
       integer      im, i1, i2, i3, i4
C   flag to indicate how solution is to be applied
       logical      use_phase, use_amp, use_both

C   Options to GT update
       integer      num_opt
       parameter   (num_opt = 3)
       character*80 options(num_opt), current_option
       data options(1) /
     * 'read-correction ...... read amplitude and phase for a GT record'
     *                 /
       data options(2) /
     * 'phase-correction ..... read phase for a GT record'
     *                 /
       data options(3) /
     * 'amplitude-correction . read amplitude for a GT record'
     *                 /



C check status on entry
       if (s.ne.0) return

C prompt for option
       call io_getopt('GT-option (?=list) : ',
     *             'read-correction', options, num_opt,
     *              current_option, s)
       if (s.ne.0) goto 999
       ls = chr_lenb(current_option)
       use_both = .false.
       use_amp = .false.
       use_phase = .false.
       if (chr_cmatch(current_option(1:ls),'read-correction')) then
         use_both = .true.
       elseif (chr_cmatch(current_option(1:ls),'phase-correction')) then
         use_phase = .true.
       elseif (
     *     chr_cmatch(current_option(1:ls),'amplitude-correction')) then
         use_amp = .true.
       end if

C set flag to indicate current gain table is being overwritten
       current_solution = .false.

C open the gains table file
       iprint = 0
       nwd = 1024
       bsize = 4*1024
       call io_operan( iunit, RT_gt_file, 'READ', bsize, iprint, s )
       call io_rdfile( iunit, 1, gt_times, nwd, s )

C get record to read
10     call io_geti('Record number : ', '0', num_rec, s )
       if (s.ne.0) goto 999
       if (num_rec .lt. 0 .or. num_rec.gt.gt_num_rec .or.
     *     gt_times(num_rec+1).eq.-1 ) then
           call io_wrout('*** Illegal record number ' )
           goto 10
       end if

C find aerial / sub-band / channel list
       do iae=1,max_RT_aes
         ae_list(iae) = 1
       end do
       do isb=1,max_subb
         sb_list(isb) = 1
       end do
       do ich=1,max_channel
         ch_list(ich) = 1
       end do
       call io_getstr( 'Aerial list : ', 'ALL', list, s )
       im = chr_lenb(list)
       i1 = 1
       call chr_chucas(list)
       do while (i1.le.im)
         i2 = chr_lend(list(i1:im),';') + i1 - 1
         i1 = chr_intlc(list(i1:i2)) + i1 - 1

C .. decode options
         if (list(i1:i1+1).eq.'SB') then
C ... Sub-Bands
           i4 = chr_intlc(list(i1+2:i2)) + i1 + 1
           if (list(i4:i4+2).ne.'ALL') then
             call chr_chlstc(list(i1+2:i2),string,array,max_subb,isb,s)
             if (s.ne.0) goto 999
             do i=1,max_subb
               sb_list(i) = 0
             end do
             do i=1,isb
               sb_list(array(i)) = 1
             end do
           end if

         else if (list(i1:i1+1).eq.'CH') then
C ... CHannels
           i4 = chr_intlc(list(i1+2:i2)) + i1 + 1
           if (list(i4:i4+2).ne.'ALL') then
             call chr_chlsti(list(i1+2:i2),array,max_channel,ich,s)
             if (s.ne.0) goto 999
             do i=1,max_channel
               ch_list(i) = 0
             end do
             do i=1,ich
               ch_list(array(i)) = 1
             end do
           end if

         else
C .. AErials
           if (list(i1:i1+1).eq.'AE') then
             i3 = 2
           else
             i3 = 0
           end if
           i4 = chr_intlc(list(i1+i3:i2)) + i1 + i3 - 1
           if (list(i4:i4+2).ne.'ALL') then
             call chr_chlsti(list(i4:i2),array,max_RT_aes,iae,s)
             if (s.ne.0) goto 999
             do i=1,max_RT_aes
               ae_list(i) = 0
             end do
             do i=1,iae
               ae_list(array(i)) = 1
             end do
           end if

         end if

C .. increment i1 pointer
         i1 = i2 + 2

       end do

C read record or initialise as required
       if (num_rec.gt.0) then
         nwd = 1024
         call io_rdfile( iunit, num_rec+1, gt_io, nwd, s )
C .. change gain table reference antenna
         if (current_refant.lt.0) then
           current_refant = gt_cal_refant
         else if (current_refant.ne.gt_cal_refant) then
           do isb = 1,max_subb
             do ich = 1,max_channel
               ref_gains(ich,isb) =
     *            cmplx(cabs(gt_ae_gains(ich,isb,current_refant)),0.0)
     *            / gt_ae_gains(ich,isb,current_refant)
             end do
           end do
           do iae = 1,max_RT_aes
             do isb = 1,max_subb
               do ich = 1,max_channel
                 if (gt_ae_gains(ich,isb,iae).ne.(1.0,0.0)) then
                   gt_ae_gains(ich,isb,iae) =
     *                gt_ae_gains(ich,isb,iae) * ref_gains(ich,isb)
                 end if
               end do
             end do
           end do
         end if
         if (gt_vis_soln) then
C ... read GT visibility-based solution
           iprint = 0
           nwd = 2560
           bsize = 4*2560
           call io_operan( iunit_vis, RT_gtvis_file, 'READ',
     *                  bsize, iprint, s )
           call io_rdfile( iunit_vis, gt_vis_rec+1, gt_iovis, nwd, s )
           call io_close( iunit_vis, s )
         else
           do i=1,max_vis
             gt_vis_gains(i) = (1.0,0.0)
           end do
         end if
         if (s.ne.0) goto 999
       else
         do iae = 1,max_RT_aes
           do isb = 1,max_subb
             do ich = 1,max_channel
               gt_ae_gains( ich, isb, iae ) = (1.0,0.0)
             end do
           end do
         end do
         do i=1,max_vis
           gt_vis_gains(i) = (1.0,0.0)
         end do
       end if

C choose amplitude / phase solutions
       if (.not.use_both) then
         if (use_phase) then
           do iae = 1,max_RT_aes
             do isb = 1,max_subb
               do ich = 1,max_channel
                 if (gt_ae_gains(ich,isb,iae).ne.(1.0,0.0)) then
                   if (gt_ae_gains(ich,isb,iae).ne.(0.0,0.0)) then
                       gt_ae_gains(ich,isb,iae) =
     *                    gt_ae_gains(ich,isb,iae) /
     *                    cmplx(cabs(gt_ae_gains(ich,isb,iae)),0.0)
                   end if
                 end if
               end do
             end do
           end do
           do i=1,max_vis
             if (gt_vis_gains(i).ne.(0.0,0.0)) then
               gt_vis_gains(i) = gt_vis_gains(i) /
     *                           cmplx(cabs(gt_vis_gains(i)),0.0)
             end if
           end do
         else
           do iae = 1,max_RT_aes
             do isb = 1,max_subb
               do ich = 1,max_channel
                 if (gt_ae_gains(ich,isb,iae).ne.(1.0,0.0)) then
                   gt_ae_gains(ich,isb,iae) =
     *               cmplx(cabs(gt_ae_gains(ich,isb,iae)),0.0)
                 end if
               end do
             end do
           end do
           do i=1,max_vis
             if (gt_vis_gains(i).ne.(0.0,0.0)) then
               gt_vis_gains(i) = cmplx(cabs(gt_vis_gains(i)),0.0)
             end if
           end do
         end if
       end if

C copy solution to the gain table
       do i = 1,max_vis
         if (use_both) then
           vis_gains(i) = gt_vis_gains(i)
         else
           if (use_phase) then
             vis_gains(i) = gt_vis_gains(i)*
     *                       cmplx(cabs(vis_gains(i)),0.0)
           else
             vis_gains(i) = gt_vis_gains(i)*
     *                      vis_gains(i) / cmplx(cabs(vis_gains(i)),0.0)
           end if
         end if
       end do
       do iae = 1,max_RT_aes
         if (ae_list(iae).eq.1) then
           do isb = 1,max_subb
             if (sb_list(isb).eq.1) then
               do ich = 1,max_channel
                 if (ch_list(ich).eq.1) then
                   if (use_both) then
                     ae_gains(ich,isb,iae) = gt_ae_gains(ich,isb,iae)
                   else
                     if (use_phase) then
                       ae_gains(ich,isb,iae) = gt_ae_gains(ich,isb,iae)*
     *                       cmplx(cabs(ae_gains(ich,isb,iae)),0.0)
                     else
                       ae_gains(ich,isb,iae) = gt_ae_gains(ich,isb,iae)*
     *                       ae_gains(ich,isb,iae) /
     *                         cmplx(cabs(ae_gains(ich,isb,iae)),0.0)
                     end if
                   end if
                 end if
               end do
             end if
           end do
         end if
       end do

       current_gains_table = .true.

C take action on error
999    if (s.ne.0) call cal_wrerr( s, 'in subroutine cal_gt_update' )

C close file
       call io_close (iunit,s)

       end
