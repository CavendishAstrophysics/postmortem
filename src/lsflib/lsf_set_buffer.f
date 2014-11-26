
C     *****************************************************************
C
C$(2) Routines for accessing and processing data in Logical Sample Files
C
C
C+lsf_set_buffer
C
      SUBROUTINE lsf_set_buffer  ( lsf_num,
     *                             buff_num,
     *                             s                      )

C
C     Sets the current buffer number for a logical sample file.
C
C     Given:
C         Logical sample file number.
              integer*4           lsf_num
C         Buffer number.
              integer*4           buff_num

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Primes all the logical sample file routines get_vis_buffer,
C     get_uv_buffer etc, for a given buffer number - so that their
C     next accesses will be on that buffer number.
C
C-
C     ****************************************************************
C
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/constants.inc'
      include  '/mrao/post/include/lsflib_errors.inc'
      include  '/mrao/post/include/lsf_definition.inc'
      include  '/mrao/post/include/lsf_runtime.inc'

C     ****************************************************************
C
C     Local constant and variable declarations
C         The sample number and start of buffer corresponding to
C         the input parameter buff_num.
              integer     sample, start_buffer

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

      if ( lsf_num .ne. curr_lsf_num ) then
          call get_lsf( lsf_num, s )
          if ( s .ne. 0 ) goto 9999
      end if

C     ****************************************************************
C
C         Main Code
C         ---------
C
      call sample_num( buff_num, sample, start_buffer, s )

      if ( s .ne. 0 ) then
          curr_buff = 0
          curr_samp = 0
          start_buff= 0
      else
          curr_buff  = buff_num
          curr_samp  = sample
          start_buff = start_buffer

          call read_rt(   sf_lun,
     *                    src_num,
     *                    curr_samp,
     *                    raw_samp_ra, raw_samp_dec,
     *                    samp_sid,
     *                    s               )
          samp_mjd = mjd_st0+dble(samp_sid)/(864000.0D+0*const_sut2sst)
          if (planet_flg) then
              call planet_topo( source_ucase, samp_mjd,
     *                          epoch_ra, epoch_dec, s    )
          end if
      end if

      if ((s .ne. 0) .and. (s .ne. ILL_BUFFER)) goto 9999

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine LSF_SET_BUFFER' )
          return
      end
