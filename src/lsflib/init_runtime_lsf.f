C
C+init_runtime_lsf
C
      SUBROUTINE init_runtime_lsf (   phys_sf_lun,
     *                                lsf_num,
     *                                s                      )

C
C     Opens files and sets up the lsf-runtime common blocks.
C
C     Given:
C         Physical sample file fortran logical unit number.
              integer         phys_sf_lun

C     Returned:
C         Logical sample file runtime number
              integer         lsf_num
C         Status variable - must be zero on entry - otherwise error
              integer         s
C
C     Initialises the lsf-runtime common blocks for the current lsf
C     definition. Also opens the remove file, calibration file and all
C     sources that are needed by the definition, excluding the LSF
C     source, since this is assumed to be already open.
C
C     NPR     9 July 1987
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
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/post/include/lsflib_errors.inc'
      include  '/mrao/post/include/lsf_definition.inc'
      include  '/mrao/post/include/lsf_runtime.inc'

C     ****************************************************************
C
C     Local constant and variable declarations
C
C     Constants
C         <description>

C     Variables, equivalences and commons
C         Loop counter and record number counter
              integer         i
              character*80    file_name
              integer         ls
C         Keys for calibration and remove.
              integer         cal_key, ion_key

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

C     ****************************************************************
C
C         Main Code
C         ---------
C
      curr_lsf_num = 1
      sf_lun  = phys_sf_lun
      inquire( phys_sf_lun, name = file_name )
      ls = chr_lend( file_name, ';' )
      sf_name = file_name( 1:ls )
      rf_lun  = 0
      cf_lun  = 0
      call enq_sftype( sf_lun, sf_type, s )

C     The following sets up the spacing list and baselines.
      call lsf_set_spacings( curr_lsf_num, sp_list_len, sp_list, 3, s )

      call enq_numsamp( sf_lun, src_num, num_samp, s )
      call enq_epoch ( sf_lun, epoch, s )
      call enq_mjd_st0( sf_lun, mjd_st0, s )
      call precrd2(1, ref_date,ref_ra,ref_dec, epoch,epoch_ra,epoch_dec)
      if ( s .ne. 0 ) goto 9999

C     Open calibration file
C     (Use enquiry routine so LSF is patched if necessary.)
      call lsf_enq_cal( phys_sf_lun, lsf_key, ion_key, cal_key, s )
      if (cal_key.ne.0) then
          call enq_namfil( phys_sf_lun, 'CAL', file_name, s )
          call open_sf( cf_lun, file_name, 'READ', 0, s )
C  [Buffer size reduced to 8 pages, DJT, 29/11/89]
          call open_srckey( cf_lun, lsf_cal_key, 8, lsf_cal_num, s )
      else
          lsf_cal_num = 0
      end if

C     Find ionospheric correction number
      if (ion_key.ne.0) then
          call enq_ion_num( phys_sf_lun, ion_key, lsf_ion_num, s )
      else
          lsf_ion_num = 0
      end if
      if (s.ne.0) goto 9999

C     Open remove file
      if ( num_removes .ne. 0 ) then
          call enq_namfil( phys_sf_lun, 'REM', file_name, s )
          call open_sf( rf_lun, file_name, 'READ', 0, s )
          if ( s .ne. 0 ) goto 9999

          do 100, i = 1, num_removes
C  [Buffer size reduced to 8 pages, DJT, 29/11/89]
              call open_srckey( rf_lun, lsf_rem_key(i), 8,
     *                          lsf_rem_num(i), s            )
  100     continue

C         Open the remove calibration sources and other runtime info.
          call open_remcal( s )
      end if

      if (s.ne.0) then
          i = 0
          call close_sf( rf_lun, i )
          rf_lun = 0
          goto 9999
      end if

C     Setup the variables for sources of high proper motion (planets)
      source_ucase = source_text
      call chr_chucas( source_ucase )
      call planet_index( source_ucase, i, s )
      planet_flg = ( s .eq. 0 )
      s = 0

      curr_samp = 0
      curr_buff = 0
      start_buff= 0
      lsf_num   = curr_lsf_num

      if (s .ne. 0) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine INIT_RUNTIME_LSF' )
          return
      end
