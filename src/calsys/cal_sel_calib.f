C+cal_sel_calib
C
      SUBROUTINE cal_sel_calib( psf_name, s )

C     Asks the user to select the sample file for the calibrator
C
C     Given:
C         Current physical sample file name
              character*64        psf_name
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     For multi-centre observations, an attempt is made to offer the
C     calibrator, or the list of possible calibrators, from the list
C     of pointing centres.
C-
C
C     Function declarations
C
      include  '/mrao/include/chrlib_functions.inc'
      include  '/mrao/include/iolib_functions.inc'

C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/post/include/global_constants.inc'
      include  '/mrao/post/include/cal_common.inc'
      include  '/mrao/post/include/merge_types.inc'
      include  '/mrao/post/include/lsflib_errors.inc'

C
C     Variables, equivalences and commons
C         New sample file name and lsf key.
              character*64        string
              character*64        new_sf_name
              integer             new_lsf_key
C         Lsf number and the number of buffers in the lsf
              integer             lsf_num, num_buff
C         Sample file logical unit number
              integer             sf_lun
C         Number of pointing centres, pointing centre index
              integer             ncentre, icentre
C         Source name and sample file name for each pointing centre
              character*24        centre(max_centre)
              character*40        sfile(max_centre)
C         Samples at each pointing centre
              integer             nsamp(max_centre)
C         RA, Dec for each pointing centre
              real*8              ra_ref(max_centre)
              real*8              dec_ref(max_centre)
C         Index array for sample file selection
              integer             ic(max_centre)
C         Telescope name, telescope code
              character*16        tscope_name
              integer             tscope_code
C         String indexes
              integer             i, i1, i2, ii, j, jj
C         Ambiguity flag for enq_sfname
              integer             iamb

C     ==================================================================
C
C     Subroutine initialisation
C     -------------------------
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

C
C     Main Code
C     ---------
C
      call open_sf(sf_lun, psf_name, 'READ', 0, s )
      call enq_centres(sf_lun, ncentre, centre, sfile, nsamp,
     :                               ra_ref, dec_ref, icentre, s )
      call enq_tscope(sf_lun, tscope_name, tscope_code, s )
      call close_sf(sf_lun, s )

      new_sf_name = ' '
      new_lsf_key = 0

C  Offer calibrators in order of increasing nsamp
      if ( ncentre .gt. 1 ) then
        do i = 1, ncentre
          ic(i) = i
        end do
        do i = 1, ncentre-1
          do j = i+1, ncentre
             if (nsamp(ic(j)).lt.nsamp(ic(i))) then
               ii = ic(j)
               do jj = j, i+1, -1
                 ic(jj) = ic(jj-1)
               end do
               ic(i) = ii
             end if
          end do
        end do
        do i = 1, ncentre
           call enq_sfname(sfile(ic(i)), string, iamb, s )
           if (io_yesno( string(1:chr_lenb(string))//'?', 'no', s ))then
             new_sf_name = string
             goto 10
           end if
        end do
      end if

 10   call lsf_open( new_sf_name, new_lsf_key, 'READ', lsf_num, s )
      if ( s .ne. 0 ) goto 9999

      if ( tscope_name.eq.'T151' .or. tscope_name.eq.'38MHZ' ) then
        if ( .not.chr_fmatch( psf_name, new_sf_name ) ) then
          call io_wrout('*** Warning sample file observations differ')
          if ( .not.io_yesno('Do you wish to continue ? ','yes',s)) then
            call lsf_close( lsf_num, s )
            new_sf_name = cal_sf
            call lsf_open( new_sf_name, cal_lsf, 'RC', lsf_num, s )
            goto 100
          end if
        end if
      end if

      i2 = chr_ilstc( new_sf_name, '/') - 1
      i1 = chr_ilstc( new_sf_name(1:i2), '/') + 1
      cal_sf = new_sf_name(i1:i2)
      cal_lsf = new_lsf_key
      call lsf_enq_numbuff( lsf_num, num_buff, s )
      call lsf_enq_pc_rdate( lsf_num, cal_refdat,
     *                       cal_ra(1), cal_dec(1), cal_source, s )
100   call lsf_close( lsf_num, s )

      if ( s .ne. 0 ) goto 9999
      return

C
C     Error Handling
C     --------------
C
 9999 continue
          if ( .not. ( s.eq.NO_LSFSAVED .or.
     *                 s.eq.USR_BREAK        ) ) then
              call cal_wrerr( s, 'in subroutine cal_sel_calib ' )
          end if
          return
      end
C
C
C     *****************************************************************
C

