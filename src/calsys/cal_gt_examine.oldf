C+ cal_gt_examine

       subroutine cal_gt_examine(psf_name, s)
C      --------------------------------------
C
C Examine records in the gains table file
C
C Given:
C   name of the current physical sample file
       character*(*) psf_name
C Returned:
C   error status
       integer       s
C
C The gain table file is examined.  Records are displayed on the terminal
C under control of a number of user defined options.  The options are:
C
C   Brief      --      brief listing of records
C   Full       --      full listing of records
C   Check      --      check record for use with named physical sample file
C   Number of entries
C   Single-entry
C

C PA, 9/05/90; 2000 fix GP 30/3/2000
C GGP new format for gt file:  14 June 2001; 21 June 2001
C
C-

       include '/mrao/post/include/global_constants.inc'
       include '/mrao/post/include/cal_common.inc'
       include '/mrao/post/include/cal_control.inc'
       include '/mrao/post/include/cal_solution.inc'
       include '/mrao/post/include/gt_include.inc'
       include '/mrao/post/include/post_sys_pars.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/constants.inc'

C   sample file unit number
       integer      sf_lun
C   unit number of gains table
       integer      iunit
C   loop counters
       integer      i, n, n1, n2, ii, rec_num
C   hours minutes seconds, degrees
       integer      im, ih, id
       real         secs
C   length of string
       integer      ls
C   time and date array and codes
       integer      datim(6), date_low, date_high
C   buffer to hold gains table data
       integer*2    save_gt(256)
C   block size and print control for io_operan routine
       integer      bsize
       integer      iprint
C   word count for io_rdfile routine
       integer      nwd
C   output unit number for listing
       integer      iout
C   unit numbers for terminal I/O
       integer      termi, termo
C   string for output
       character    string*80
C   listing options
       integer      mode
       integer      num_opt
       parameter   (num_opt = 5)
       character*80 options(num_opt), current_option
       data options(1) /
     * 'brief ................ brief listing of GT file entries'
     *                 /
       data options(2) /
     * 'detailed ............. detailed listing of GT file entries'
     *                 /
       data options(3) /
     * 'check-mode ........... check entries in GT file for use with SF'
     *                 /
       data options(4) /
     * 'number-of-entries .... return number of entries in the GT file'
     *                 /
       data options(5) /
     * 'single-entry ......... detailed report on a specified entry'
     *                 /

* -------------

       if (s.ne.0) return

       call io_enqtio(termi, termo)

C prompt for listing option
       call io_getopt('GT-examine-option (?=list) : ',
     *             'brief', options, num_opt,
     *             current_option, s )
       if (s.ne.0) goto 999
       ls = chr_lenb(current_option)
       if (chr_cmatch(current_option(1:ls),'brief')) then
         mode = 1
       elseif (chr_cmatch(current_option(1:ls),'detailed')) then
         mode = 2
       elseif (chr_cmatch(current_option(1:ls),'check-mode')) then
         mode = 3
       elseif (
     *    chr_cmatch(current_option(1:ls),'number-of-entries')) then
         mode = 4
       elseif (chr_cmatch(current_option(1:ls),'single-entry')) then
         mode = 5
       else
         call io_wrout('*** Unknown option ')
         goto 999
       end if

C read the index file, then open the gain table

        iprint = 0
        nwd    = gt_max_entries
        bsize  = 4*gt_blocksize
        call io_operan (iunit, RT_gt_index, 'READ', bsize, iprint, s)
        call io_rdfile (iunit, 1, gt_index, nwd, s)
        call io_close  (iunit, s)
        call io_operan (iunit, RT_gains, 'READ', bsize, iprint, s)
        if (s.ne.0) goto 999


C open the sample file of the calibration if check mode is operational
C and save record for checking mode operation
       call util_enqdat(datim(4))
       if (mode.eq.3) then
         iprint = 0
         call open_sf( sf_lun, psf_name, 'READ', iprint, s )
         call enq_gt_rec( sf_lun, gt_record, s )
         call close_sf( sf_lun, s )
         do i=1,gt_length
           save_gt(i) = gt_record(i)
         end do
         do i=1,3
           datim(i+3) = gt_idat1(i)
         end do
       end if
       if (mode.ne.4 .and. mode.ne.5) then
         call io_getdat('List from (date) : ', '*', datim(4), s)
         datim(1) = 0
         datim(2) = 0
         datim(3) = 0
         call util_datint(datim, date_low)
         call util_enqdat(datim(4))
         call io_getdat('List to   (date) : ', '*', datim(4), s)
         datim(1) = 59
         datim(2) = 59
         datim(3) = 23
         call util_datint(datim, date_high)
       else if (mode.eq.4) then
         call io_enqout(iout)
         write(iout,'('' Number-of-records '',I8)') gt_num_rec
         goto 999
       else if (mode.eq.5) then
         rec_num = gt_num_rec
         call io_geti('Entry-number : ','*',rec_num,s)
         rec_num = max(1,rec_num)
         rec_num = min(gt_num_rec,rec_num)
         rec_num = rec_num
       end if
       call io_opeout(iout, s)
       if (s.ne.0) goto 999
       if (mode.eq.1 .or. mode.eq.3) then
         write (iout,100)
 100     format(1x/1x,'Record  ','Source                      ',
     *                'Position (1950.0)            ',
     *                'Time/Date' /
     *             1x,'----------------------------------------',
     *                '----------------------------------------')
       end if

       n1 = 1
       n2 = gt_num_rec
       if (mode.eq.5) then
         n1 = rec_num
         n2 = rec_num
       end if
       do n=n1,n2
         if (mode.eq.5 .or.  (gt_index(n+1).ge.date_low  .and.
     *                        gt_index(n+1).le.date_high .and.
     *                        gt_index(n+1).ne.-1             )) then
           nwd = gt_blocksize
           call io_rdfile(iunit, n, gt_io, nwd, s)
           if (mode.eq.1 .or. mode.eq.3) then
             string = ' '
             string(1:24) = gt_source(1:chr_lenb(gt_source))
             string(26:26) = 'P'
             string(27:27) = 'A'
             if (gt_cal_no_phi) string(26:26) = ' '
             if (gt_cal_no_amp) string(27:27) = ' '
             if (gt_vis_soln)   string(28:28) = 'V'
             call frrads( 'HMS', gt_RA, ih, im, secs )
             write( string(30:41), '(I2.2,I3.2,F7.2)' ) ih, im, secs
             call frrads( 'DMS', gt_DEC, id, im, secs )
             write( string(43:55), '(I3.2,I3.2,F7.2)' ) id, im, secs
             write(string(58:71),
     *           '(I2,''.'',I2.2,'' '',I2,'':'',I2.2,'':'',I2.2)' )
     *             gt_itim1(3),gt_itim1(2),(gt_idat1(ii), ii=1,2),
     *             mod(gt_idat1(3), 100)
             write(iout,'(1X,I5,3X,A)') n,string(1:71)
             if (mode.eq.3) then
               call cal_gt_excheck(save_gt, gt_record, string, s)
               ls = chr_lenb( string )
               write( iout, '(1X,A,A)' ) '-- CHECK: ',string(1:ls)
             end if
           else if (mode.eq.2.or.mode.eq.5) then
             string = ' '
             write(iout,'(1X/1X,''Record   : '',
     *             I4/1X,       ''---------------'')') n
             write(iout,'(1X,''Source     : '',A)')
     *             gt_source(1:chr_lenb(gt_source))
             if (.not.gt_cal_no_phi) string(1:10)  = 'Phase'
             if (.not.gt_cal_no_amp) string(11:20) = 'Amplitude'
             write(iout,'(1X,''Correction : '',A)') string(1:20)
             string = ' '
             call frrads( 'HMS', gt_RA, ih, im, secs )
             write( string(1:13), '(I3.2,I3.2,F7.2)' ) ih, im, secs
             call frrads( 'DMS', gt_DEC, id, im, secs )
             write( string(15:27), '(I3.2,I3.2,F7.2)' ) id, im, secs
             string(30:37) = '(1950.0)'
             write(iout,'(1X,''Position   : '',A)') string(1:40)
             write(iout,'(1X,''Number of  : '',
     *                       ''Aerials = '',I1,'' Sub-bands = '',I1,
     *                       '' Channels = '',I1,'' Samples = '',I4)')
     *                       gt_Naes, gt_Nsubb, gt_Nchannel, gt_Nsamp
             write(iout,'(1X,
     *      ''Start-date : '',I2,''.'',I2.2,2X,I2,'':'',I2.2,'':'',I4)')
     *            gt_itim1(3),gt_itim1(2),(gt_idat1(i), i=1,3)
             write(iout,'(1X,
     *      ''End-date   : '',I2,''.'',I2.2,2X,I2,'':'',I2.2,'':'',I4)')
     *            gt_itim2(3),gt_itim2(2),(gt_idat2(i), i=1,3)
             write(iout,'(1X,''Frequency  : '',F10.3,A)') gt_freq,
     *             ' MHz (nominal central frequency)'
             write(iout,'(1X,''- Sub-bands: '',5F10.3)') gt_subb_freq
             write(iout,'(1X,''- Channels : '',4F10.3)')
     *             (gt_chan_freq(i), i=1,4)
             write(iout,'(1X,''-          : '',4F10.3)')
     *             (gt_chan_freq(i), i=5,8)
             write(iout,'(1X,''F.R. code  : '',I6)')
     *             gt_feed_rotation_code
             write(iout,'(1X,''Comments   : '',A)') gt_comment(1)
             ii = 1
             do i=2,8
               if (gt_comment(i).ne.' ') ii = i
             enddo
             do i=2,ii
               write(iout,'(1X,''           : '',A)') gt_comment(i)
             end do
             if (s.ne.0) goto 999
             if (iout.eq.termo) then
               call io_wrout(' ')
               if (mode .eq. 2) then
                 call io_getwrd('.. press RETURN to continue ', ' ',
     *                     string, ls, s)
               endif
               if (s.ne.0) then
                 s = 0
                 goto 999
               end if
             end if
           end if
         end if
       end do

C take action on error
999    if (s.ne.0) call cal_wrerr(s, 'in subroutine cal_gt_examine')

C close file
       call io_close(iunit, s)
       call io_close(iout, s)
       call io_setout(termo)
       call io_wrout(' ')

       end
