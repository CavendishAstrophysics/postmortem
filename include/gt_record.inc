C
C Gains Table Record Definition
C -----------------------------
C
C Last updated 23/04/90  P. Alexander
C
C *************************************************************************
C *                                                                       *
C *  6/7/92, DJT, alignment adjusted for Sun f77 compiler                 *
C *                                                                       *
C *************************************************************************
C
C This record together with the full CAL record occupy 1024 bytes.
C
C This record definition is only applicable to the RYLE telescope.
C
       integer      gt_length
       parameter   (gt_length = 176)
       integer*2    gt_record(gt_length)
C
C Information from the control tables of the calibration observation:
C   source title
       character*24 gt_source
       equivalence (gt_source, gt_record(1))
C   number of spacings, aerials, samples, sub-bands and channels
       integer*4    gt_Nsp
       integer*4    gt_Naes
       integer*4    gt_Nsamp
       integer*4    gt_Nsubb
       integer*4    gt_Nchannel
       equivalence (gt_Nsp, gt_record(13))
       equivalence (gt_Naes, gt_record(15))
       equivalence (gt_Nsamp, gt_record(17))
       equivalence (gt_Nsubb, gt_record(19))
       equivalence (gt_Nchannel, gt_record(21))
C   aerial and sub-band lists
       integer*2    gt_aerial_list(8)
       integer*2    gt_subband_list(5)
       equivalence (gt_aerial_list, gt_record(23))
       equivalence (gt_subband_list, gt_record(31))
C   feed rotation cycle, number of polarizations and polarizations
       integer*2    gt_feed_rotation_code
       integer*2    gt_Npolns
       integer*2    gt_ipoln_codes(10)
       equivalence (gt_feed_rotation_code, gt_record(36))
       equivalence (gt_Npolns, gt_record(37))
       equivalence (gt_ipoln_codes, gt_record(38))
C   moving aerial locations in 1/4 spacing coordinates
       integer*2    gt_aepos(4)
       equivalence (gt_aepos, gt_record(48))
C   observing frequency, subband offsets and channel offsets
       real*8       gt_freq
       real*8       gt_subb_freq(5)
       real*8       gt_chan_freq(8)
       equivalence (gt_freq, gt_record(53))
       equivalence (gt_subb_freq, gt_record(57))
       equivalence (gt_chan_freq, gt_record(77))
C   start and stop time / date of calibration run
       integer*4    gt_itim1(3)
       integer*4    gt_idat1(3)
       integer*4    gt_itim2(3)
       integer*4    gt_idat2(3)
       equivalence (gt_itim1, gt_record(109))
       equivalence (gt_idat1, gt_record(115))
       equivalence (gt_itim2, gt_record(121))
       equivalence (gt_idat2, gt_record(127))
C Information added when record written
C   time and date at write
       integer*4    gt_time(3), gt_date(3)
       equivalence (gt_time, gt_record(133))
       equivalence (gt_date, gt_record(139))
C   telescope operator initials
       character*6  gt_operator
       equivalence (gt_operator, gt_record(145))
C   RA and DEC of source
       real*8       gt_RA, gt_DEC
       equivalence (gt_RA, gt_record(149))
       equivalence (gt_DEC, gt_record(153))
C Details of visibility-based calibration
       logical      gt_vis_soln
       equivalence (gt_vis_soln, gt_record(157))
       integer*4    gt_vis_rec
       equivalence (gt_vis_rec, gt_record(159))
C
C Entries 161 --> 176 spare
C
