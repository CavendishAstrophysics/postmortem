C
C+rem_make_remove
C
      SUBROUTINE rem_make_remove( file_name, s )

C     Calculates all outstanding removes for a remove file.
C
C     Given:
C         Remove file name.
              character*(*)       file_name
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C
C-
C     ****************************************************************
C
C     Function declarations
C
      include  '/mrao/include/iolib_functions.inc'
      include  '/mrao/include/chrlib_functions.inc'

C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/include/constants.inc'
      include  '/mrao/post/include/remsys_errors.inc'
      include  '/mrao/post/include/remove_common.inc'
      include  '/mrao/post/include/global_constants.inc'

C     ****************************************************************
C
C     Variables, equivalences and commons
C         General purpose loop counters
              integer         i,j
C         String indexes
              integer         i1,i2
C         Current output device
              integer         out
C         List of visibilities returned from get_vis_buffer
              complex         vis_list( max_vis )
C         Spacing list corresponding to this
              integer         sp_list( max_vis )
C         Number of visibilities in the buffer.
              integer         num_vis
C         Sidereal time of the buffer
              integer         sid
C         Remove file unit number
              integer         rf_lun
C         Number of sources in the remove file and current source num.
              integer         num_srcs, src_num
C         Physical sample file name
              character*80    psf_name
C         Logical sample file number
              integer         lsf_num
C         Number of buffers and current buffer of LSF
              integer         num_buff, buff_num
C         LSF phase centre parameters at epoch
              real*8          epoch_ra, epoch_dec, epoch
              character*16    source
C         Flags set if batch mode, to make current remove and if a
C         remove is made successfully
              logical         batch, make_rem, rem_made

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return
      call io_enqout( out )
      call io_enqbch( batch )

      i2 = chr_ilstc( file_name, '/') - 1
      i1 = chr_ilstc( file_name(1:i2), '/') + 1
      psf_name = file_name(i1:i2)
      call open_sf( rf_lun, file_name, 'WRITE', 0, s )
      call enq_numsrc( rf_lun, num_srcs, s )

C     ****************************************************************
C
C         Main Code
C         ---------
C
      rem_made = .false.
      do 1000, src_num = 1, num_srcs
          call enq_src_def( rf_lun, src_num, remove_record, s )
          make_rem = (rem_key.eq.0)
          if (.not.(batch.or.make_rem)) then
              j = chr_lenb(rem_source)
              make_rem=io_yesno(
     *                       'Remake remove on '//rem_source(1:j)//' ?',
     *                       'No', s                               )
          end if

          if ( make_rem .and. rem_type.ge.0 .and. rem_type.le.2) then
              write(out,*) 'Making remove of type',rem_type,
     *                     ' for ', rem_source
C  [Remove file buffer cut down to 8 pages, DJT, 27/11/89]
              call open_source( rf_lun, src_num, 8, s )
              call lsf_open( psf_name, rem_lsf, 'READ', lsf_num, s )
              call lsf_enq_numbuff( lsf_num, num_buff, s )
              call lsf_set_pc( lsf_num, rem_refdat,
     *                         rem_ra(1), rem_dec(1), rem_source, s )
              call lsf_enq_pc_epoch( lsf_num, epoch,
     *                               epoch_ra, epoch_dec, source, s )
              call lsf_set_spacings( lsf_num, num_vis, sp_list, 3, s )

C             Set up model if this is a model source remove.
              if (rem_type.eq.2) then
                  call lsf_set_pbeam( lsf_num, .not.rem_no_pbeam, s )
                  call lsf_set_bandpass( lsf_num, rem_band_type,
     *                                    rem_bandwidth, s )
                  call lsf_set_integt( lsf_num, rem_integt, s )
                  call lsf_set_noise( lsf_num, rem_mod_mean,
     *                                         rem_mod_sigma, s )
                  do i = 1, rem_num_point
                     call lsf_add_source( lsf_num, rem_src_type,
     *                              rem_refdat, rem_ra(i), rem_dec(i),
     *                                 rem_src_flux(i), s)
                  enddo
                  if (rem_src_type.ne.1)
     *                call lsf_set_model( rem_model_file, rem_ap_interp,
     *                                    rem_mod_mean, rem_mod_sigma,
     *                                        s )
              end if

              do 500, buff_num = 1, num_buff
C                  if ( imod((buff_num), 10 ) .eq. 1 ) then
C                     write(out,*) 'Making remove sample ... ', buff_num
C                  end if

                  call lsf_set_buffer(lsf_num, buff_num, s )
                  if (rem_type.ne.2) then
                      call lsf_get_vis(   lsf_num,
     *                                    max_vis,
     *                                    vis_list,
     *                                    num_vis,
     *                                    s          )
                  else
                      call lsf_get_model( lsf_num,
     *                                    max_vis,
     *                                    vis_list,
     *                                    num_vis,
     *                                    s          )
                  end if
                  call lsf_get_sid(   lsf_num, sid, s )

                  call write_rt(  rf_lun, src_num,
     *                            buff_num, epoch_ra, epoch_dec, sid, s)
                  call write_sample(  rf_lun, src_num, buff_num, 1,
     *                                num_vis, sp_list, vis_list, s )
                  if ( s .ne. 0 ) goto 9999
  500         continue

              call lsf_close( lsf_num, s )
              call close_source( rf_lun, src_num, s )
              if ( s .ne. 0 ) goto 9999

              if (rem_key.eq.0) call util_enqnow(rem_key)
              call set_src_def( rf_lun, src_num, remove_record, s )
              if (s.eq.0) rem_made = .true.
          else if ( rem_key .eq. 0 ) then
              s = ILL_REMOVE
              goto 9999
          end if
 1000 continue

      call close_sf( rf_lun, s )
c     if (rem_made) call save_sf( file_name, s )
      if ( s .ne. 0 ) goto 9999


      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call rem_wrerr( s, 'in subroutine REM_MAKE_REMOVE' )
          return
      end
