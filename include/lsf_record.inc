C     *****************************************************************
C
C     Logical Sample File Definition Record
C     -------------------------------------
C
C     This record defines the layout of the LSF definition record. It is
C     a map of the records in the :LSF files and is held in common for
C     the current LSF at runtime.
C
C     The LSF definition can only refer to one particular physical
C     sample file (PSF). If the the logical sample file is saved then
C     it is saved in the file:
C             (pack-two:<telescope name>)<PSF file name>:lsf
C
C     Because of this convention there is no explicit pointer between
C     the PSF and the LSF, this being done by the PSF file name. Each
C     LSF takes a page in this save file and is uniquely identified
C     by the lsf_key, which is the SINTRAN file date of when the LSF
C     was saved. If the LSF has not been saved, then this key is zero,
C     and the lsf_name is also blank.
C
C
C     *****************************************************************

      include '/mrao/post/include/global_constants.inc'

      integer*4               lsf_len
      parameter             ( lsf_len = 512 )

      integer*4               log_samp_file( lsf_len )
      integer*4               max_sp, max_rems
      parameter             ( max_sp   = 1600,
     *                        max_rems = 20     )


C     Version number, name, and time stamp of time created and last used
      integer*4               lsf_key
      integer*4               lsf_version
      character*60            lsf_name
      character*16            lsf_owner
      integer*4               lsf_time_created
      character*16            lsf_last_user
      integer*4               lsf_last_used

C     Source number of source in physical sample file.
      integer*4               src_num

C     Preprocessing interference chop type and parameters. As with all
C     the _type variables a chop type of zero means no chopping is done
      integer*4               pre_int_chop_type
      integer*4               pre_int_chop_params( 10 )

C     Remove information. The remove flag is an undefined flag and the
C     remove key is a pointer to a particular remove in the remove file.
C     Information about each remove is stored in the remove file.
      integer*4               num_removes
      integer*4               rem_flag( max_rems )
      integer*4               lsf_rem_key( max_rems )

C     Ionospheric correction information. Similar comments to those for
C     removes except ionospheric corrections are stored in the sample
C     file itself. A correction number of zero is no correction.
      integer*4               ion_flag
      integer*4               lsf_ion_key

C     Complex gain information - similar comments as to removes.
      integer*4               cal_flag
      integer*4               lsf_cal_key

C     Flag Table information
C     flag specifying flagging selected
      integer*4               flag_flag
C     key specifying version to flag
      integer*4               flag_key
C     pointer to last used record
      integer*4               flag_record

C     Phase centre of the logical sample file.
      real*8                  ref_ra, ref_dec, ref_date
      character*16            source_text

C     Post interference chop parameters.
      integer*4               post_int_chop_type
      integer*4               post_int_chop_params( 10 )

C     Post processing smoothing information.
      integer*4               smooth_type
      integer*4               smooth_size

C     Sampling rate of LSF, in samples.
      integer*4               samp_rate

C     Bit array indicating which spacings to use.
      integer*4               sp_bit_arr( 50 )

C     Bit array indicating which samples to use - up to 8000 samples.
      integer*4               samp_list( 250 )


      equivalence   ( log_samp_file(  1), lsf_key               )
      equivalence   ( log_samp_file(  2), lsf_version           )
      equivalence   ( log_samp_file(  3), lsf_name              )
      equivalence   ( log_samp_file( 18), lsf_owner             )
      equivalence   ( log_samp_file( 22), lsf_time_created      )
      equivalence   ( log_samp_file( 23), lsf_last_user         )
      equivalence   ( log_samp_file( 27), lsf_last_used         )
      equivalence   ( log_samp_file( 28), src_num               )
      equivalence   ( log_samp_file( 29), pre_int_chop_type     )
      equivalence   ( log_samp_file( 30), pre_int_chop_params   )
      equivalence   ( log_samp_file( 40), num_removes           )
      equivalence   ( log_samp_file( 41), rem_flag              )
      equivalence   ( log_samp_file( 61), lsf_rem_key           )
      equivalence   ( log_samp_file( 81), ion_flag              )
      equivalence   ( log_samp_file( 82), lsf_ion_key           )
      equivalence   ( log_samp_file( 83), cal_flag              )
      equivalence   ( log_samp_file( 84), lsf_cal_key           )
      equivalence   ( log_samp_file( 85), ref_ra                )
      equivalence   ( log_samp_file( 87), ref_dec               )
      equivalence   ( log_samp_file( 89), ref_date              )
      equivalence   ( log_samp_file( 91), source_text           )
      equivalence   ( log_samp_file( 95), post_int_chop_type    )
      equivalence   ( log_samp_file( 96), post_int_chop_params  )
      equivalence   ( log_samp_file(106), smooth_type           )
      equivalence   ( log_samp_file(107), smooth_size           )
      equivalence   ( log_samp_file(108), samp_rate             )
      equivalence   ( log_samp_file(109), sp_bit_arr            )
      equivalence   ( log_samp_file(159), samp_list             )
      equivalence   ( log_samp_file(409), flag_flag             )
      equivalence   ( log_samp_file(410), flag_key              )
      equivalence   ( log_samp_file(411), flag_record           )
