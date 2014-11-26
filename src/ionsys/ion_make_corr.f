C
C+ion_make_corr
C
      SUBROUTINE ion_make_corr( sf_name,
     *                          s                      )

C
C     Calculates all outstanding ion. corrections for a sample file.
C
C     Given:
C         Physical sample file name.
              character*(*)       sf_name
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
      include    '/mrao/include/chrlib_functions.inc'

C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include    '/mrao/include/constants.inc'
      include    '/mrao/post/include/ionsys_errors.inc'
      include    '/mrao/post/include/ion_runtime.inc'

C     ****************************************************************
C
C     Local variable declarations
C         General purpose loop counters and character string
              integer         i, j
C         1-D Aperture and map array - complex before and real after
              complex         aperture( max_aper_size )
              real            map( max_map_size )
              equivalence   ( aperture, map )
C         Number of corrections in the sample file
              integer         num_corr
C         LSF to use for the current correction.
              integer         lsf_num
C         Number of buffers in the lsf and current buffer
              integer         num_buff, buff_num
C         Flag set if this is the first non-null LSF buffer
              logical         first_buff
C         The current sample number
              integer         samp_num
C         This sample's ra, dec, sidereal time and correction.
              integer         samp_sid
              real*8          ra, dec
              real            ion(4)
C         The current and previous LSF buffer's sid time and correction.
              integer         sid1, sid2
              real            ion1(4), ion2(4)
C         Factors for interpolating from the lsf buffer's to the sample
              real            fact1, fact2
C         The search radius in gridpoints and the search centre
              integer         val_map_width, search_posn
C         Sample file information returned from enquiry routines.
              integer         sf_lun, src_num
C         The integrated amplitude to Jy/beam normalisation factor.
              real            norm_fact
C         General purpose output string and length
              character*48    string
              integer         ls
C         Default output device.
              integer         out
C         Logical flags to control interaction
              logical         batch, make_ion, io_yesno

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return
      call io_enqout( out )
      call io_enqbch( batch )

      call open_sf( sf_lun, sf_name, 'READ', 0, s )
      call open_source( sf_lun, 1, 0, s )
      call enq_numcorr( sf_lun, num_corr, s )
      call enq_epoch( sf_lun, ion_epoch, s )
      call close_source( sf_lun, 1, s )
      call close_sf( sf_lun, s )

C     ****************************************************************
C
C         Main Code
C         ---------
C
      do 800, ion_number = 1, num_corr
          call open_sf( sf_lun, sf_name, 'READ', 0, s )
          call open_source( sf_lun, 1, 0, s )
          call enq_ion_corr(  sf_lun,
     *                        ion_number,
     *                        ion_type, ion_key, ion_lsf,
     *                        ion_first, ion_last,
     *                        ion_numsrcs, ion_source, ion_ra, ion_dec,
     *                        ion_param, s                          )
          call close_source( sf_lun, 1, s )

          make_ion = (ion_key.eq.0)
          if (.not.(batch.or.make_ion.or.ion_type.eq.0)) then
              call enq_ion_name( sf_lun, ion_number, string, s )
              j = chr_lenb(string)
              make_ion=io_yesno('Remake corr. on '//string(1:j)//' ?',
     *                       'No', s                               )
          end if
          call close_sf( sf_lun, s )

          if (make_ion .and. ion_type .ne. 0) then
              call lsf_open( sf_name, ion_lsf, 'WRITE', lsf_num, s )
              call lsf_enq_sf( lsf_num, sf_lun, src_num, s )
              call lsf_enq_numbuff( lsf_num, num_buff, s )
              call lsf_set_pc( lsf_num,
     *                         ion_epoch, ion_ra(1), ion_dec(1),
     *                         ion_source(1), s )
              search_posn = map_size/2+1
              val_map_width = int(search_rad/arcsec_per_gp)+3

C             Calculate beam normalisation factor.
              call ion_transform( lsf_num, -1, aperture,
     *                            search_posn-val_map_width,
     *                            search_posn+val_map_width, s )
              call ion_calc_corr( map, search_posn, ion, s )

C             Patch to get over beam-height problems
              if (ion(3).eq.0.0) ion(3) = 1.0
              norm_fact = 1.0/ion(3)

C             Initialise the sample number to the first sample
              samp_num   = ion_first
              call read_rt( sf_lun, 1, samp_num, ra, dec, samp_sid, s )
              first_buff = .true.
              write(string,'(A,I2,A)') 'Making correction number ',
     *                ion_number, ' >-------------------'
              call io_wrout( string )

              do 700, buff_num = 1, num_buff
                  if (mod(buff_num,max(int(num_buff/20),1)).eq.0) then
                      ls = chr_lend( string, '>' )
                      string(ls+1:ls+2) = '->'
                      write(1,'(''+'',A)') string
                  end if

                  call ion_transform(lsf_num,
     *                               buff_num,
     *                               aperture,
     *                               max0(search_posn-val_map_width,1),
     *                               min0(search_posn+val_map_width,
     *                                    map_size                  ),
     *                               s                               )

                  if (s .eq. 0) then
                      call ion_calc_corr( map, search_posn, ion2, s )
                      ion2(3) = ion2(3)*norm_fact

                      if ( num_buff .eq. 1 ) then
C                         Just write this correction to all samples.
                          do 100, i = ion_first, ion_last
                              call write_ion_corr( sf_lun,
     *                                             ion_number,
     *                                             i,
     *                                             ion2,  s   )
  100                     continue
                      else if (first_buff) then
C                         Just set up previous values for interpolation
                          first_buff = .false.
                          call lsf_get_sid( lsf_num, sid1, s )
                          if ( s .ne. 0 ) goto 9999
                          do 200, i = 1, 4
                              ion1(i) = ion2(i)
  200                     continue
                      else
C                         Interpolate and write the correction away...
                          call lsf_get_sid( lsf_num, sid2, s )

  300                     if ( samp_num .le. ion_last .and.
     *                        (buff_num.eq.num_buff.or.samp_sid.lt.sid2)
     *                                                            ) then
                              fact1=real(sid2-samp_sid)/(sid2-sid1)
                              fact2=real(samp_sid-sid1)/(sid2-sid1)
                              do 400, i = 1, 4
                                  ion(i) = ion1(i)*fact1+ion2(i)*fact2
  400                         continue
                              call write_ion_corr( sf_lun,
     *                                             ion_number,
     *                                             samp_num,
     *                                             ion,  s   )

                              samp_num = samp_num + 1
                              if ( samp_num .lt. ion_last ) then
                                  call read_rt( sf_lun, 1, samp_num,
     *                                          ra, dec, samp_sid, s)
                              end if
                          goto 300
                          end if

C                         Set the previous values.
                          sid1 = sid2
                          do 500, i = 1, 4
                              ion1(i) = ion2(i)
  500                     continue
                      end if
                  else if (s .eq. NO_VISDATA) then
                      s = 0

C                     Clean up if it is the last buffer - just fill
C                     the rest of the file with the last correction
                      if (buff_num .eq. num_buff) then
                          do 600, samp_num = samp_num, ion_last
                              call write_ion_corr( sf_lun,
     *                                             ion_number,
     *                                             samp_num,
     *                                             ion1,  s   )
  600                     continue
                      end if
                  else
                      goto 9999
                  end if
  700         continue

              if (ion_key.eq.0) call util_enqnow( ion_key )
              call set_ion_corr(  sf_lun,
     *                            ion_number,
     *                            ion_type, ion_key, ion_lsf,
     *                            ion_numsrcs,
     *                            ion_source, ion_ra, ion_dec,
     *                            ion_param, s                  )
              call lsf_close( lsf_num, s )

C             Mark the sample file for saving
c             call save_sf( sf_name, s )
          end if
C     BODGE to skip past knotted and unmade correction to make next
      if( s .eq. -2003) then
          write(1,*)'Resetting STATUS and continuing'
          s = 0
      endif
  800 continue

      if ( s .ne. 0 ) goto 9999

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call ion_wrerr( s, ' in subroutine ion_make_corr ' )
          return
      end
