C
C Include file for gain corrections
C
C Include record definition
       include '/mrao/post/include/gt_record.inc'

C Define IO records
       integer*2      gt_io(2048)
C Define number of records
       integer*4      gt_num_rec
C Define times index list
       integer*4      gt_times(1024)
C Define gain solution
       complex        gt_ae_gains( max_channel, max_subb, max_RT_aes )
C Define cal record
       integer*4      gt_cal_record( cal_length )
       logical        gt_cal_no_amp
       equivalence   (gt_cal_no_amp, gt_cal_record(16))
       logical        gt_cal_no_phi
       equivalence   (gt_cal_no_phi, gt_cal_record(17))
       integer        gt_cal_refant
       equivalence   (gt_cal_refant, gt_cal_record(22))
C Define comment text
       character*64   gt_comment(8)

C equivalence these to the IO record
       equivalence   (gt_num_rec, gt_times(1))
       equivalence   (gt_record, gt_io(1))
       equivalence   (gt_cal_record, gt_io(177))
       equivalence   (gt_comment, gt_io(513))
       equivalence   (gt_ae_gains, gt_io(769))

C Define visibility gain record
       integer*4      gt_iovis(2560)
       complex        gt_vis_gains( max_vis )
       equivalence   (gt_vis_gains( 1 ), gt_iovis(2) )
