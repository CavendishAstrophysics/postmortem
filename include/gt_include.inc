C /mrao/post/include/gt_include.inc

C Include file for RT gain corrections - new version 6 June 2001  GP
C
C record definition
       include '/mrao/post/include/gt_record.inc'

        integer*4        gt_blocksize, gt_max_entries, gt_max_blocks
        parameter       (gt_blocksize   = 1024)
        parameter       (gt_max_blocks  = 10)
        parameter       (gt_max_entries = gt_max_blocks*gt_blocksize)

C IO records
        integer*2       gt_io(2048)

C number of records
        integer*4       gt_num_rec
C index list
        integer*4       gt_index(gt_max_entries)
C gain solution
        complex         gt_ae_gains(max_channel, max_subb, max_RT_aes)
C cal record
        integer*4       gt_cal_record(cal_length)
        logical         gt_cal_no_amp
        equivalence    (gt_cal_no_amp, gt_cal_record(16))
        logical         gt_cal_no_phi
        equivalence    (gt_cal_no_phi, gt_cal_record(17))
        integer         gt_cal_refant
        equivalence    (gt_cal_refant, gt_cal_record(22))
C comment text
        character*64    gt_comment(8)

C equivalence these to the IO record
       equivalence   (gt_num_rec, gt_index(1))
       equivalence   (gt_record, gt_io(1))
       equivalence   (gt_cal_record, gt_io(177))
       equivalence   (gt_comment, gt_io(513))
       equivalence   (gt_ae_gains, gt_io(769))

C visibility gain record
       integer*4      gt_iovis(2560)
       complex        gt_vis_gains(max_vis)
       equivalence   (gt_vis_gains(1), gt_iovis(2))
