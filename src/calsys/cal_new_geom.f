
C
C+cal_new_geom
C
      subroutine cal_new_geom(sf_name, s)

* corrections to phases resulting from error in adopted geometry
* GP 9 March 1999


c       given:  sample-file name (sf_name)

        character*(*)   sf_name

C     Returned:
C         Status variable - must be zero on entry - otherwise error

        integer         s

C-
C
C     Function declarations, constants, common, errors
C
      include  '/mrao/include/chrlib_functions.inc'
      include  '/mrao/include/iolib_functions.inc'
      include  '/mrao/include/constants.inc'
      include  '/mrao/include/iolib_errors.inc'

      include  '/mrao/post/include/src_pack.inc'
      include  '/mrao/post/include/calib_errors.inc'
      include  '/mrao/post/include/global_constants.inc'
      include  '/mrao/post/include/cal_common.inc'


C
C     Local variables, equivalences and commons

                integer         lun, iunit, ounit, cf_lun
                integer         i, cal_samp
                integer         src_num, old_cal_num, new_cal_num
                integer         lsf_key
                integer         prsec/3/
                integer         ra_len, dec_len
                integer         samples, cal_buffers, visibilities
                integer         new_cal_num
                integer         sid             ! from sample-redtape

                integer         subband,channel,spacing,iae1,iae2

                real*8          ch_freq

                real*8          e_ref, e_date
                real*8          ra, dec
                real*8          ra_source, dec_source
                real*8          ra_cal, dec_cal
                real*8          dx, dy, dz
                real*8          px, py, pz

                integer         Ixyz(3, 8)
                integer         Jxyz(3, 8)

* this should be fetched from the redtape, but ...

                real*4          Azlong  /.044389541/

                real*4          cos_hs, sin_hs, cos_hc, sin_hc
                real*4          cos_ds, sin_ds, cos_dc, sin_dc
                real*4          h_s, h_c, s_time
                real*4          dphi


                character*3     system
                character*24    src_name
                character*80    geom_file, cal_file_name
                character*32    ra_string, dec_string



* Ixyz(3, 8): original values via enquiry routine

* Jxyz(3, 8): x(1), y(1), z(1), x(2), ... unit: 0.1 mm [changes]


                integer         num_centre
                character*24    s_name(max_centre)
                character*40    f_name(max_centre)
                integer         n_samp(max_centre)
                real*8          ra_ref(max_centre)
                real*8          de_ref(max_centre)
                integer         centre_no



C         Logical sample file spacing list
              integer         sp_list(max_vis)
C         Spacing list for previous calibration
              integer         cal_list(max_vis)
C         List of visibilities returned from get_vis_buffer
              complex         vis_list(max_vis)
C         List of model visibilities returned from cal_def_model
              complex         mod_vis(max_vis)
C         List of derived spacing calibrations
              complex         gains(max_vis)
C         List of previous spacing calibrations
              complex         prev_gains(max_vis)
C         Incremental spacing calibration buffer
              complex         gains_buff(max_vis)


C         Workspace
              common /post/ sp_list, cal_list, vis_list,
     *                      mod_vis, gains, prev_gains, gains_buff

C     ==================================================================

C initialisation


      if (s .ne. 0) return

        call io_enqout(ounit)
        call open_sf (lun, sf_name, 'READ', 0, s)
        if (s .ne. 0) return
        call open_source (lun, 1, 0, s)
        call enq_numsamp (lun, 1, samples, s)
        call enq_numvis  (lun, visibilities, s)

        if (s .ne. 0) goto 9999
        write (ounit,*) samples,      ' samples in sample file'
        write (ounit,*) visibilities, ' visibilities/sample'

        call enq_namfil(lun, 'CAL', cal_file_name, s)
        if (s .eq. NO_FILE) then
          s = 0
          call io_wrout(
     *             'No calibrations have been saved for this file.')
          call close_sf(lun, s)
          return
        endif

        src_num = 1

        call enq_2_epochs (lun, e_ref, e_date, s)
        write (*, '(1x, a, f8.3,3x, a, f8.3)')
     *                          'ref epoch ', e_ref,
     *                          'obs epoch ', e_date

* get ra, dec (source, at ref date)

        call enq_centres(lun, num_centre, s_name, f_name,
     *                   n_samp, ra_ref, de_ref, centre_no, s)

        ra_source  = ra_ref(centre_no)
        dec_source = de_ref(centre_no)
        src_name   = s_name(centre_no)

        if (s .ne. 0) goto 9999


* select a calibration ...

        call cal_open (sf_name, lsf_key, s)
        if (s .ne. 0) goto 9999

        old_cal_num = cal_number
*        write (*, '(1x, a, i3)') 'cal_number: ', cal_number

* get ra, dec (cal-source:  at ref. date)

        ra_cal  = cal_ra(1)
        dec_cal = cal_dec(1)

        call chr_chdtos (ra_source/const_h2r, prsec, ra_string, ra_len)
        call chr_chdtos (dec_source/const_d2r,prsec,dec_string,dec_len)

        write (*, '(1x,a,1x,a,a,a)') 'source: ', src_name,
     *                                ra_string(1:ra_len),
     *                               dec_string(1:dec_len)

        call chr_chdtos (ra_cal/const_h2r, prsec, ra_string, ra_len)
        call chr_chdtos (dec_cal/const_d2r,prsec,dec_string,dec_len)

        write (*, '(1x,a,a,9x,a,a)') 'cal   : ', cal_source,
     *                          ra_string(1:ra_len),
     *                         dec_string(1:dec_len)

* precession from e_ref to e_date ...

        system = '???'
        if (e_ref .eq. 1950.0D0) system = 'FK4'
        if (e_ref .eq. 2000.0D0) system = 'FK5'

        write (*, *) 'precessed from ',system,' to date of observation'

        call sla_preces(system, e_ref, e_date, ra_source, dec_source)
        call sla_preces(system, e_ref, e_date, ra_cal, dec_cal)

        call chr_chdtos (ra_source/const_h2r, prsec, ra_string, ra_len)
        call chr_chdtos (dec_source/const_d2r,prsec,dec_string,dec_len)

        write (*, '(1x,a,1x,a,a,a)') 'source: ', src_name,
     *                                ra_string(1:ra_len),
     *                               dec_string(1:dec_len)

        call chr_chdtos (ra_cal/const_h2r, prsec, ra_string, ra_len)
        call chr_chdtos (dec_cal/const_d2r,prsec,dec_string,dec_len)

        write (*, '(1x,a,a,9x,a,a)') 'cal   : ', cal_source,
     *                          ra_string(1:ra_len),
     *                         dec_string(1:dec_len)


* read the old set of values:

        call enq_xyz(lun, Ixyz, s)

        if (s .ne. 0) goto 9999
        write (ounit, *) 'Original geometry values/0.1mm'
        write (ounit, '(3(2x,i4))') Ixyz


        call close_sf (lun, s)


        cos_ds = cos(dec_source)
        sin_ds = sin(dec_source)
        cos_dc = cos(dec_cal)
        sin_dc = sin(dec_cal)



* get table of 8*xyz changes (from a file)


        geom_file = '/mrao/post/util/geom_file.data'
        call io_getfil('geometry corrections file :','*',geom_file,s)

        call io_opefil(iunit, geom_file, 'READ', 1, s)
        if (s .ne. 0) return
        read (iunit, *, err = 9999, iostat = s) Jxyz
        close (iunit)

        write (ounit, *)            'changes'
        write (ounit, '(3(2x,i4))') Jxyz


* for each sample:

*       find ST, calculate phase change for each baseline,
*       modify old cal and write new one


C Initialise buffers

      do i = 1, max_vis                         ! was max_spac
         gains(i)      = cmplx(1.0,0.0)
         prev_gains(i) = cmplx(1.0,0.0)
         gains_buff(i) = cmplx(1.0,0.0)
      end do


        do i = 1, visibilities
          cal_list(i) = i
        enddo


        call open_sf (cf_lun, cal_file_name, 'WRITE', 0, s)
        if (s .ne. 0) goto 9999

        call open_source (cf_lun, old_cal_num, 0, s)

        call enq_numsamp (cf_lun, old_cal_num, cal_buffers, s)
        if (s .ne. 0) goto 9999

* save a new calibration:

        cal_number = 0          ! force cal_save to save a new cal
        cal_source = cal_source(1:chr_lenb(cal_source))//'_g'

        call cal_save(sf_name, s)

        new_cal_num = cal_number
        if (s .ne. 0) goto 9999

        call open_source (cf_lun, new_cal_num, 0, s)
        if (s .ne. 0) goto 9999


C Loop through and for each sample in the calibration LSF determine
C the new gains, writing the results to the calibration file

      do cal_samp = 1, cal_buffers

C .. set up buffer containing previous calibration gains

  100 call read_rt    (cf_lun, old_cal_num, cal_samp, ra, dec, sid, s)
      call read_sample(cf_lun, old_cal_num, cal_samp, 1,
     *                          visibilities, cal_list, prev_gains, s)





C .. combine new gains with gains from any previous calibration

         do i = 1, visibilities
                call enq_vis_desig(cf_lun,i,spacing,subband,channel,s)
                call enq_chfreq (cf_lun,subband,channel,ch_freq,s)
                call enq_ae_vis ( cf_lun, i, iae1, iae2, s)

* E - W, 0.1mm
                dx = Jxyz(1,iae1)-Jxyz(1,iae2)
                dy = Jxyz(2,iae1)-Jxyz(2,iae2)
                dz = Jxyz(3,iae1)-Jxyz(3,iae2)

* c in m/s; ch_freq in Hz; xyz in 0.1mm

                px = const_pi*dx*2.0D-4*ch_freq/const_c
                py = const_pi*dy*2.0D-4*ch_freq/const_c
                pz = const_pi*dz*2.0D-4*ch_freq/const_c

* sid unit is 0.1s (!)

                s_time = sid*const_pi/432000.0
                h_s    = s_time - ra_source - Azlong
                h_c    = s_time - ra_cal    - Azlong
                cos_hs = cos(h_s)
                sin_hs = sin(h_s)
                cos_hc = cos(h_c)
                sin_hc = sin(h_c)

                dphi = px*(cos_ds*sin_hs - cos_dc*sin_hc)
     *               + py*(cos_ds*cos_hs - cos_dc*cos_hc)
     *               + pz*(sin_ds        - sin_dc)


             gains_buff(i) = prev_gains(i)*cmplx(cos(dphi),sin(dphi))

         end do

C .. write sample redtape and data to the calibration file
         call write_rt(cf_lun, new_cal_num, cal_samp, ra, dec, sid, s)
         call write_sample(cf_lun, new_cal_num, cal_samp, 1,
     *                          visibilities, cal_list, gains_buff, s)

         if (s .ne. 0) goto 9999

      enddo


C Update and close the calibration file
      call enq_src_pack(cf_lun, new_cal_num, src_pack, s)
      src_num_samp = cal_buffers
      if (src_interp_type.eq.1) then
         call write_rt(cf_lun, src_num, src_num_samp,
     *                  cal_ra(1), cal_dec(1), src_stop_time, s)
      end if
      call set_src_pack(cf_lun, new_cal_num, src_pack, s)

      call close_source(cf_lun, old_cal_num, s)
      call close_source(cf_lun, new_cal_num, s)
      if (s .ne. 0) goto 9999

      call util_enqnow(cal_key)
      call set_src_def(cf_lun, new_cal_num, cal_record, s)

      if (s .ne. 0) goto 9999
        call close_sf (cf_lun)
        write (ounit, '(1x, a, i2, a)')
     *            'New cal no ', new_cal_num, ' written'
      return


C     Errors

 9999   call cal_wrerr(s, 'in subroutine cal_new_geom')
        return
      end
