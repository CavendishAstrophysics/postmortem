




C$(6b)  Control tables version 2 enquiry support routines.


C+ENQ_V2_GT_REC

       subroutine enq_v2_gt_rec ( record, s )
C
C     Returns information for the gains table record
C
C     Returned:
C         Gains table record
               integer*2  record(*)
C         Status
              integer     s
C
C Control tables version 2 support routine for ENQ_GT_REC
C PA 20/04/90
C
*-
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v2.inc'
      include '/mrao/post/include/gt_record.inc'

      integer  i, ich, isb, iae

      if ( s .ne. 0 ) return

      gt_source = source
      gt_RA = RAref
      gt_DEC = DECref
      gt_Nsp = Nsp
      gt_Naes = Naes
      gt_Nsamp = Nsamp
      gt_Nsubb = Nsubb
      gt_Nchannel = Nchannel
      do iae = 1,max_aes
        gt_aerial_list(iae) = aerial_list(iae)
      end do
      do isb = 1,max_subb
        gt_subband_list(isb) = subband_list(isb)
        gt_subb_freq(isb) = subb_freq(isb)
      end do
      gt_feed_rotation_code = feed_rotation_code
      gt_Npolns = Npolns
      do i=1,10
        gt_ipoln_codes(i) = ipoln_codes(i)
      end do
      do iae=1,4
        gt_aepos(iae) = aepos(iae)
      end do
      gt_freq = freq
      do ich = 1,max_channel
        gt_chan_freq(ich) = chan_freq(ich)
      end do
      do i=1,3
        gt_itim1(i) = itim1(i)
        gt_idat1(i) = idat1(i)
        gt_itim2(i) = itim2(i)
        gt_idat2(i) = idat2(i)
      end do

      do i=1,gt_length
        record(i) = gt_record(i)
      end do

      end
