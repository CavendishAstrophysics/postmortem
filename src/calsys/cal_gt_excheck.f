C
C
C+ cal_gt_excheck

       subroutine cal_gt_excheck( gt1, gt2, string, s )
C      ------------------------------------------------
C
C Provide consistency check for gt records GT1 and GT2
C
C      Given:
C         GT records
                 integer*2     gt1(*), gt2(*)
C      Returned
C         Results of check -- character string form
                 character*(*) string
C         Error status
                 integer       s
C
C The records are assumed to be as follows:
C     gt1   --   observation
C     gt2   --   calibration
C
C The following consistency checks are implemented:
C  (i)   Aerials / sub-bands present
C  (ii)  Position of moving aerials
C  (iii) Frequencies of all sub-bands and channels
C Any inconsistencies are reported and returned to the calling (sub)program
C in the STRING array.
C
C PA 26/04/90
C-
       include '/mrao/post/include/global_constants.inc'
       include '/mrao/post/include/gt_record.inc'
       include '/mrao/include/chrlib_functions.inc'

C Local variables
C   internal string to hold cumulative error description
       character*200   error_string
C   saved number of aerials, sub-bands and channels of the observation
       integer         Naes, Nsubb, Nchannel
C   saved aerial positions and aerial/sub-band lists of the observation
       integer*2       save_aepos(4), save_aerial_list(8),
     *                 save_subband_list(5)
C   saved sub-band/channel frequencies of the observation
       real*8          save_freq, save_subb_freq(5), save_chan_freq(8)
C   counters
       integer         i, ii, j, jj, ls
C   logical variable reporting failure of consistency checks
       logical         fail
C   textual list of sub-bands -- for reporting purposes
       character*1     sub_bands(5)
       data            sub_bands / 'A','B','C','D','E' /


C check status on entry
       if (s.ne.0) return

C copy observation record and save values for checking
       do i=1,gt_length
         gt_record(i) = gt1(i)
       end do
       Naes = gt_Naes
       Nsubb = gt_Nsubb
       Nchannel = gt_Nchannel
       do i=1,4
         save_aepos(i) = gt_aepos(i)
       end do
       do i=1,Naes
         save_aerial_list(i) = gt_aerial_list(i)
       end do
       do i=1,Nsubb
         save_subband_list(i) = gt_subband_list(i)
       end do
       save_freq = gt_freq
       do i=1,max_subb
         save_subb_freq(i) = gt_subb_freq(i)
       end do
       do i=1,max_channel
         save_chan_freq(i) = gt_chan_freq(i)
       end do

C copy calibration record
       do i=1,gt_length
         gt_record(i) = gt2(i)
       end do

C check / sub-band aerial list
       error_string = 'AE:'
       ls = chr_lenb(error_string) + 2
       error_string(ls:ls+1) = 'OK'
       do i=1,Naes
         do j=1,Nsubb
           fail = .true.
           do ii=1,gt_Naes
             if (save_aerial_list(i).eq.gt_aerial_list(ii)) then
               do jj=1,gt_Nsubb
                 if (save_subband_list(j).eq.gt_subband_list(jj)) then
                   fail = .false.
                 end if
               end do
             end if
           end do
           if (fail) then
             write(error_string(ls+1:ls+1),'(I1,A1)')i,sub_bands(j)
             ls = chr_lenb(error_string) + 2
           end if
         end do
       end do

C check aerial positions
       ls = chr_lenb(error_string) + 2
       error_string(ls:ls+3) = 'POS:'
       ls = chr_lenb(error_string) + 2
       error_string(ls:ls+1) = 'OK'
       do i=1,4
         if (save_aepos(i).ne.gt_aepos(i)) then
           write (error_string(ls:ls),'(I1)') i
           error_string(ls+1:ls+1) = ' '
           ls = ls+1
         end if
       end do

C check frequencies
       ls = chr_lenb(error_string) + 2
       error_string(ls:ls+4) = 'FREQ:'
       ls = chr_lenb(error_string) + 2
       error_string(ls:ls+1) = 'OK'

       if (save_freq.ne.gt_freq) then
         error_string(ls:ls+2) = 'All'

       else
         do i=1,max_subb
           if (save_subb_freq(i).ne.gt_subb_freq(i)) then
             write (error_string(ls:ls),'(A1)') sub_bands(i)
             ls = ls+1
           end if
         end do
         do i=1,max_channel
           if (save_chan_freq(i).ne.gt_chan_freq(i)) then
             write (error_string(ls:ls),'(I1)') i
             ls = ls+1
           end if
         end do

       end if

C copy results of checking to STRING to be returned to calling program
       ls = chr_lenb(error_string)
       string = error_string(1:ls)

C report any errors
       if (s.ne.0) call cal_wrerr( s, 'in subroutine cal_gt_excheck' )

       end
