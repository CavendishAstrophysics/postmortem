C     *****************************************************************
C
C     Logical Sample File Run Time Common Block.
C     ------------------------------------------
C
C     These are things that only exist for the duration of the current
C     computing session - thus they are control variables or are
C     tabulations of calculated values that are needed for efficiency
C     reasons
C
C     modified by DJT, 8/7/92, common block adjusted to satisfy Sun
C     Fortran compiler alignment restrictions.
C
C     *****************************************************************

C     The maximum number of spacings allowed in these realtime blocks.
      integer*4               max_rt_sp
      parameter             ( max_rt_sp = 1600 )

C     The maximum number of logical sample files allowed open at a time.
      integer*4               max_lsfs
      parameter             ( max_lsfs = 1 )

C     The maximum number of removes allowable by this realtime block
      integer*4               max_rt_rems
      parameter             ( max_rt_rems = 20 )

C     Current logical sample file number
      integer                 curr_lsf_num

C     Current root sample file name.
      character*80            sf_name

C     Current unit numbers for sample file, remove file, gains file
C     and calibration file
      integer                 sf_lun
      integer                 rf_lun
      integer                 gf_lun
      integer                 cf_lun

C     Current sample file type
      integer                 sf_type

C     Integer array of the spacing list.
      integer*4               sp_list_len
      integer*4               sp_list( max_rt_sp )

C     The following two define the baseline and are set up in
C     the routine calc_baseline.
C     Baseline of the telescope in wavelengths
      real*8                  base( 3, max_rt_sp )

C     The angle between the projection of the axis of the telescope on
C     the equatorial plane and the RA=0 plane at sidereal time zero.
      real*8                  baseln_skew

C     Total number of samples in physical sample file
      integer                 num_samp

C     The epoch of the physical sample file as a decimal year
      real*8                  epoch

C     The modified julian date at the zero of sidereal time.
      real*8                  mjd_st0

C     The phase centre of the logical sample file at the sidereal time
C     of the current sample.
      real*8                  epoch_ra, epoch_dec

C     Calibration source number and ionospheric correction number
      integer                 lsf_cal_num, lsf_ion_num

C     Remove information from remove file control tables.
      integer                 lsf_rem_num( max_rt_rems )
      integer                 lsf_rem_type( max_rt_rems )
      integer                 lsf_rem_ion( max_rt_rems )
      integer                 lsf_rem_cal( max_rt_rems )

C     Current sample and current buffer.
      integer                 curr_samp, curr_buff

C     Sample number of the start of the current buffer.
C     (needed for some smoothing purposes - normally equal to curr_samp)
      integer                 start_buff

C     Redtape information for the current sample
      integer                 samp_sid
      real*8                  raw_samp_ra, raw_samp_dec, samp_mjd

C     Flag set if source has a high proper motion.
      logical                 planet_flg

C     Name of source expressed in upper case
      character*16            source_ucase

C     Multi-level interference clipping parameters
      integer*4               sp_index(2,5), npair(2)
      real*4                  amp_level(2,5)

      common       / lsf_runtime /
     *    curr_lsf_num, sf_name, sf_lun, rf_lun, gf_lun, cf_lun,
     *    sf_type, sp_list_len, sp_list, num_samp,
     *    base, baseln_skew, epoch, mjd_st0, epoch_ra, epoch_dec,
     *    lsf_ion_num, lsf_cal_num, lsf_rem_num,
     *    lsf_rem_type, lsf_rem_ion, lsf_rem_cal,
     *    curr_samp, curr_buff, start_buff,
     *    samp_sid, raw_samp_ra, raw_samp_dec, samp_mjd,
     *    planet_flg, source_ucase,
     *    sp_index, npair, amp_level

