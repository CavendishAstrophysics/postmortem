*+
      subroutine shadow_flag(file, shadow_beg, shadow_end,
     &          ha_beg_int, ha_end_int,run_beg_int,run_end_int,
     &          lsf_key,status)

c Subroutine implements flagging of the shadowing found by calc_shad
c
c Keith Grainge 21/11/95; output unit corrected 28/10/98 (GP)
c
*-
      include '/mrao/include/iolib_errors.inc'
      include '/mrao/include/iolib_functions.inc'
      include '/mrao/include/chrlib_functions.inc'
      include '/mrao/post/include/global_constants.inc'
      include '/mrao/post/include/flag_errors.inc'

      integer mrec
      parameter (mrec=64)
      logical shadow_beg(5), shadow_end(5)
      integer ha_beg_int(5,3), ha_end_int(5,3)
      integer run_beg_int(3), run_end_int(3)
      integer i,j,status
      integer*2 iflag(max_samp,max_aes)
      character  flag_spac(mrec)*16
      character file*(*)
      character  flag_samp(mrec)*168
      common /post/ flag_spac,flag_samp,iflag
      integer lspac,lsamp
      integer lsf_key,lsf_num,sf_lun,src_num
      character  ffile*64
      character*4 cae,cbeghr,cbegmi,cbegse
      character*4 crunbeghr,crunbegmi,crunbegse
      character*4 cendhr,cendmi,cendse
      character*4 crunendhr,crunendmi,crunendse
      integer flag_id, flag_version, irec,nrec
      integer l1,l2,l3,l4,l5,l6,l7
      integer iout

      nrec = 0

      call io_enqout(iout)

c open file and then close it to get correct sf_lun

      call open_sf(sf_lun,file,'read',0,status)
      call close_sf(sf_lun,status)

      write (iout, *) 'start'
      call chr_chitoc(run_beg_int(3),crunbeghr,l5)
      call chr_chitoc(run_beg_int(2),crunbegmi,l6)
      call chr_chitoc(run_beg_int(1),crunbegse,l7)
      do i = 1, 5
         if (.not. shadow_beg(i)) then
            write (iout, 9000) 'ae', i, ' not shadowed'
         else
            write (iout, 9001) 'ae', i,
     &           ha_beg_int(i,3),ha_beg_int(i,2),ha_beg_int(i,1)
            nrec = nrec + 1
            call chr_chitoc(i,cae,l1)
            call chr_chitoc(ha_beg_int(i,3),cbeghr,l2)
            call chr_chitoc(ha_beg_int(i,2),cbegmi,l3)
            call chr_chitoc(ha_beg_int(i,1),cbegse,l4)
            flag_spac(nrec) = 'ae ' // cae(1:l1)
            flag_samp(nrec) = 'ha ' // crunbeghr(1:l5) // ' ' //
     &       crunbegmi(1:l6) // ' ' // crunbegse(1:l7) // ' ' //
     &       cbeghr(1:l2) // ' ' // cbegmi(1:l3) // ' ' // cbegse(1:l4)
         endif
      enddo
      write (iout, *) 'end'
      call chr_chitoc(run_end_int(3),crunendhr,l5)
      call chr_chitoc(run_end_int(2),crunendmi,l6)
      call chr_chitoc(run_end_int(1),crunendse,l7)
      do i = 1, 5
         if (.not. shadow_end(i)) then
            write (iout, 9000) 'ae', i, ' not shadowed'
         else
            write (iout, 9001) 'ae', i, (ha_end_int(i,j), j = 3,1,-1)
            nrec = nrec + 1
            call chr_chitoc(i,cae,l1)
            call chr_chitoc(ha_end_int(i,3),cendhr,l2)
            call chr_chitoc(ha_end_int(i,2),cendmi,l3)
            call chr_chitoc(ha_end_int(i,1),cendse,l4)
            flag_spac(nrec) = 'ae ' // cae(1:l1)
            flag_samp(nrec) = 'ha ' //
     &       cendhr(1:l2) // ' ' // cendmi(1:l3) // ' ' // cendse(1:l4)
     &    // ' ' // crunendhr(1:l5) // ' ' // crunendmi(1:l6) // ' ' //
     &       crunendse(1:l7)
         endif
      enddo
c
c    Apply flag table entries to the flag file
c
      if (nrec.gt.0) then
         call lsf_open(file,lsf_key,'READ',lsf_num,status)
         call lsf_enq_sf(lsf_num,sf_lun,src_num,status)
         if (io_yesno(
     &         'Do you want to add these entries to the flag file?',
     &                                           'no',status)) then
            call enq_namfil(sf_lun,'FLAG',ffile,status)
            call flag_open(ffile,flag_id,status)
            if (status.eq.0) then
               flag_version=5
               do irec=1,nrec
                  lspac=chr_lenb(flag_spac(irec))
                  lsamp=chr_lenb(flag_samp(irec))
                  call flag_write_entry(flag_id,lsf_num,sf_lun,
     :                               flag_version,'POSTMORTEM',
     :                               flag_spac(irec)(1:lspac),
     :                               flag_samp(irec)(1:lsamp),
     :                               'set','shadowing',status)
c                  write(*,*) flag_spac(irec)(1:lspac),
c     :                               flag_samp(irec)(1:lsamp)
               enddo
               call flag_close(flag_id,status)
            endif
         endif
         call lsf_close(lsf_num,status)
      endif

 9000 format (1x, a, i2, a)
 9001 format (1x, a, i2, 2x, 3i3.2)

      end
