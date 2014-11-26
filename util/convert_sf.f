C
       integer status
       status=0
       call io_initio
       call io_initlf(.true.)
       call io_setesc(.true.)
       call convert_sf(status)
       call io_setesc(.false.)
       end



*+
       subroutine CONVERT_SF (status)
C
C  Converts a sample file imported to Unix from ND systems
C
C  DJT, 8 December 93
*-
       integer    status
c
       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/samplib_errors.inc'
       include '/mrao/post/include/samp_rt.inc'
c
       character  file*64
c      real*8     buffer(2048)
       integer    sf_type, num_src, num_samp
       integer    ifile, isrc, isamp
c
       if (status.ne.0) return

c  Get sample file name

       call io_getfil('sample file : ', ' ', file, status)

c  Open the sample file

       call open_sf(ifile, file, 'WRITE', 0, status)

c  Convert control tables

       call read_ct(ifile, status)
       if (ct_vers .eq. 0) then
          call convert_v0_ct(status)
       elseif (ct_vers .eq. 1) then
          call convert_v1_ct(status)
       elseif (ct_vers .eq. 2) then
          call convert_v2_ct(status)
       else
          status=ILL_CONTTAB
       endif
       call write_ct(ifile, status)

c  Convert RA,Dec within redtape of each sample

       call enq_sftype(ifile,sf_type,status)
       call enq_numsrc(ifile,num_src,status)
       if (status.eq.0) then
         do isrc = 1, num_src
           call open_source(ifile,isrc,0,status)
           call enq_numsamp(ifile,isrc,num_samp,status)
           do isamp = 1, num_samp
             call convert_rt(ifile,isrc,isamp,status)
             if (ct_vers .eq. 2 .and. sf_type .eq. 1) then
                call convert_mon(ifile,isrc,isamp,status)
             endif
           enddo
           call close_source(ifile,isrc,status)
         enddo
       endif

c  Convert zero corrections for version 0 control tables

c      if (ct_vers.eq.0 .and. status.eq.0) then
c        block_no = block_no + samp_pages
c        no_words = no_words*2
c        call io_rdfile(ifile,block_no,buffer,no_words,status)
c        if (status.eq.0) then
c          call util_rndr32(buffer,buffer,no_words)
c          call io_wrfile(ifile,block_no,buffer,no_words,status)
c        endif
c      endif

       call close_sf(ifile,status)

       if (status.ne.0) then
          call smp_wrerr(status, 'in subroutine CONVERT_SF')
       endif

       end






*+
       subroutine CONVERT_V0_CT (status)
C
C  Convert control tables to Unix format
C
C  Converts version 0 control tables imported to Unix from ND systems,
C  translating floating-point values to IEEE format, and moving some
C  control table items to satisfy alignment restrictions.
C
C  DJT, 1 July 92.
C
*-
       integer  ct_pages, samp_pages, no_samp
       integer  status
c
       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v0.inc'
c
       integer  i
c
       if (status.ne.0) return

C  Reset save flag

       isaved = 0

C  Adjust the alignment of real*8 variables in Section 7

       do i = 72,17,-1
          ctv0s7(i) = ctv0s7(i-2)
       enddo

C  Translate floating-point values to IEEE format

       call util_rndr32(X,X,187)
       call util_rndr32(COLL,COLL,120)
       call util_rndr32(AMPSCL,AMPSCL,2)
       call util_rndr64(PI,PI,10)
       call util_rndr64(RAMC,RAMC,7)
       call util_rndr32(HOFFSET,HOFFSET,120)
       call util_rndr64(RAAE,RAAE,2)
       call util_rndr64(RAPC,RAPC,2)
       call util_rndr64(CAL,CAL,12)
       call util_rndr32(FION,FION,1)
       call util_rndr64(REM,REM,12)
       call util_rndr32(AFAC,AFAC,1)
       call util_rndr32(SECARC,SECARC,1)

       ct_pages = ICTAB
       samp_pages = (LRECB*2)/(page*4)
       no_samp = NSAMP

       end






*+
       subroutine CONVERT_V1_CT (status)
C
C  Convert control tables to Unix format
C
C  Converts version 1 control tables imported to Unix from ND systems,
C  translating floating-point values to IEEE format, and moving some
C  control table items to satisfy alignment restrictions on floating-
C  point variables.
C
C  Use this routine for sample files, remove (REM) files and calibration
C  (CAL) files.  Remove definitions are expanded to increase the maximum
C  number of source definitions per remove to 16.
C
C  DJT, 4 July 92.
C
*-
       integer  ct_pages, samp_pages, no_samp
       integer  status
c
       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v1.inc'
       include '/mrao/post/include/remove_record.inc'
       include '/mrao/post/include/cal_record.inc'
       include '/mrao/post/include/src_pack.inc'
c
       integer  sf_type, max_src, num_src
       integer  src_ptr, len_src, len_pack
       integer  i, ioff, isrc
c
       if (status.ne.0) return

C  Adjust the alignment of real*8 variables in Section 7

       do i = 72,17,-1
          ctv1s7(i) = ctv1s7(i-2)
       enddo

C  Translate floating-point values to IEEE format, Sections 2-7

       call util_rndr32(X,X,187)
       call util_rndr32(COLL,COLL,120)
       call util_rndr32(AMPSCL,AMPSCL,2)
       call util_rndr64(PI,PI,10)
       call util_rndr64(RAMC,RAMC,7)

       no_samp = 0
       samp_pages = 0

       ct_pages = ctv1_pack(2)
       sf_type  = ctv1_pack(4)
       num_src  = ctv1_pack(6)
       src_ptr  = ctv1_pack(8)
       len_src  = ctv1_pack(9)
       len_pack = ctv1_pack(10)

C  Convert remove source definitions

       if (sf_type .eq. 2) then

          do i = 1, rem_length
             remove_record(i) = 0
          enddo

          max_src = (ct_pages*page-src_ptr)/(len_src+rem_length)
          num_src = min0(num_src,max_src)

          do isrc = num_src, 1, -1

C    Read source definition from control tables, realigning
C    floating-point variables to word boundaries.

             ioff = src_ptr + (isrc-1)*len_src
             do i = 1, len_pack
                src_pack(i) = ct_all(ioff+i)
             enddo
             ioff = ioff + len_pack
             do i = 1, 19
                remove_record(i) = ct_all(ioff+i)
             enddo
             ioff = ioff + 19
             do i = 1, 12
                remove_record(20+i) = ct_all(ioff+i)
             enddo
             ioff = ioff + 12
             do i = 1, 12
                remove_record(52+i) = ct_all(ioff+i)
             enddo
             ioff = ioff + 12
             do i = 1, 6
                remove_record(84+i) = ct_all(ioff+i)
             enddo
             ioff = ioff + 6
             do i = 1, 19
                remove_record(100+i) = ct_all(ioff+i)
             enddo

C    Translate floating-point values to IEEE format

             call util_rndr32(src_amp_factor,src_amp_factor,1)
             call util_rndr64(rem_refdat,rem_refdat,1)
             call util_rndr32(rem_bandwidth,rem_bandwidth,1)
             call util_rndr32(rem_integt,rem_integt,1)
             call util_rndr32(rem_model_max,rem_model_max,1)
             call util_rndr64(rem_ra,rem_ra,6)
             call util_rndr64(rem_dec,rem_dec,6)
             call util_rndr32(rem_src_flux,rem_src_flux,6)
             call util_rndr32(rem_mod_mean,rem_mod_mean,1)
             call util_rndr32(rem_mod_sigma,rem_mod_sigma,1)

C    Write source definition back to control tables

             ioff = src_ptr + (isrc-1)*(len_pack+rem_length)
             do i = 1, len_pack
                ct_all(ioff+i) = src_pack(i)
             enddo
             ioff = ioff + len_pack
             do i = 1, rem_length
                ct_all(ioff+i) = remove_record(i)
             enddo

             no_samp = no_samp + src_num_samp

          enddo

          samp_pages = src_samp_len/page

          ctv1_pack(6) = num_src
          ctv1_pack(7) = max_src
          ctv1_pack(9) = len_pack + rem_length

C  Convert calibration source definitions

       elseif (sf_type .eq. 3) then

          do isrc = 1, num_src

             do i = 1, cal_length
                cal_record(i) = 0
             enddo

C    Read source definition from control tables, realigning
C    floating-point variables to word boundaries

             ioff = src_ptr + (isrc-1)*len_src
             do i = 1, len_pack
                src_pack(i) = ct_all(ioff+i)
             enddo
             ioff = ioff + len_pack
             do i = 1, 11
                cal_record(i) = ct_all(ioff+i)
             enddo
             do i = 12, 74
                cal_record(i+1) = ct_all(ioff+i)
             enddo

C    Translate floating-point values to word boundaries

             call util_rndr32(src_amp_factor,src_amp_factor,1)
             call util_rndr64(cal_refdat,cal_refdat,1)
             call util_rndr32(cal_bandwidth,cal_bandwidth,1)
             call util_rndr32(cal_integt,cal_integt,1)
             call util_rndr64(cal_ra,cal_ra,6)
             call util_rndr64(cal_dec,cal_dec,6)
             call util_rndr32(cal_src_flux,cal_src_flux,6)
             call util_rndr32(cal_mod_mean,cal_mod_mean,1)
             call util_rndr32(cal_mod_sigma,cal_mod_sigma,1)

C    Write source definition back to control tables

             ioff = src_ptr + (isrc-1)*len_src
             do i = 1, len_pack
                ct_all(ioff+i) = src_pack(i)
             enddo
             ioff = ioff + len_pack
             do i = 1, cal_length
                ct_all(ioff+i) = cal_record(i)
             enddo

             no_samp = no_samp + src_num_samp

          enddo

          samp_pages = src_samp_len/page

       endif

       end




*+
       subroutine CONVERT_V2_CT (status)
C
C  Convert control tables to Unix format
C
C  Converts version 2 control tables imported to Unix from ND systems,
C  translating floating-point values to IEEE format, and moving some
C  control table items to satisfy alignment restrictions on floating-
C  point variables.
C
C  Use this routine for sample files, remove (REM) files and calibration
C  (CAL) files.  Remove definitions are expanded to increase the maximum
C  number of source definitions per remove to 16.
C
C  DJT, 6 July 92.
C
*-
       integer  ct_pages, samp_pages, no_samp
       integer  status
c
       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v2.inc'
       include '/mrao/post/include/remove_record.inc'
       include '/mrao/post/include/cal_record.inc'
       include '/mrao/post/include/src_pack.inc'
       include '/mrao/include/chrlib_functions.inc'
c
       integer  sf_type, max_src, num_src
       integer  src_ptr, len_src, len_pack
       integer  i, i1, i2, ioff, isrc
       character string*40
c
       if (status.ne.0) return

       save_flag = 0

C  Translate floating-point values to IEEE format, Sections 4,7,8

       call util_rndr64(DATREF,DATREF,2)
       call util_rndr64(X,X,3*MAX_AES*MAX_SUBB*MAX_CHANNEL)
       call util_rndr32(AMPSCL,AMPSCL,2)

C  Convert track queue item, Section 3

       do i = 89, 86, -1
         Qitem(Qp0+i) = Qitem(Qp0+i-1)
       enddo
       Qitem(Qp0+85) = Qitem(Qp0+81)
       do i = 81, 76, -1
         Qitem(Qp0+i) = Qitem(Qp0+i-1)
       enddo
       call util_rndr64(RAref,RAref,5)
       call util_rndr32(offset_W,offset_W,3)
       call util_rndr32(offset_arcmin,offset_arcmin,1)
       call util_rndr64(RAref_list,RAref_list,2*max_centre)
       do i = 1, Ncentre
         i1 = index(file_list(i),')')
         string = file_list(i)(i1+1:)
         i1 = index(string,':')
         i2 = chr_lenb(string)
         call chr_chlcas(string(i1+1:i2))
         string(i1:i1) = '/'
         file_list(i) = string
       enddo

C  Convert telescope parameters, Section 6

       do i = 135, 65, -1
         tpa(Pp5+i) = tpa(Pp5+i-3)
       enddo
       do i = 63, 0, -1
         tpa(Pp5+i) = tpa(Pp5+i-2)
       enddo
       do i = 7, 0, -1
         tpa(Pp4+i) = tpa(Pp4+i-1)
       enddo
       call util_rndr64(freq,freq,max_subb+max_channel+35)
       call util_rndr32(X4,X4,32)
       call util_rndr32(Natmos,Natmos,3)
       call util_rndr32(Tsys,Tsys,10)
       call util_rndr32(cos_lat,cos_lat,2)
       call util_rndr64(az_tangent,az_tangent,2)
       call util_rndr32(ha_stow,ha_stow,8)
       call util_rndr32(ha_7point,ha_7point,14)


       no_samp = 0
       samp_pages = 0

       ct_pages = ctv2_pack(2)
       sf_type  = ctv2_pack(4)
       num_src  = ctv2_pack(6)
       src_ptr  = ctv2_pack(8)
       len_src  = ctv2_pack(9)
       len_pack = ctv2_pack(10)

C  Convert source definition parameters, Section 9A

       if (sf_type .eq. 1) then

          ioff = src_ptr
          do i = 1, len_pack
             src_pack(i) = ct_all(ioff+i)
          enddo
          call util_rndr32(src_amp_factor,src_amp_factor,1)
          do i = 1, len_pack
             ct_all(ioff+10) = src_pack(10)
          enddo

          no_samp = src_num_samp
          samp_pages = src_samp_len/page

C  Convert remove source definitions, Section 9B

       elseif (sf_type .eq. 2) then

          do i = 1, rem_length
             remove_record(i) = 0
          enddo

          max_src = (ct_pages*page-src_ptr)/(len_src+rem_length)
          num_src = min0(num_src,max_src)

C    Read source definition from control tables, realigning
C    floating-point variables with word boundaries

          do isrc = num_src, 1, -1

             ioff = src_ptr + (isrc-1)*len_src
             do i = 1, len_pack
                src_pack(i) = ct_all(ioff+i)
             enddo
             ioff = ioff + len_pack
             do i = 1, 19
                remove_record(i) = ct_all(ioff+i)
             enddo
             ioff = ioff + 19
             do i = 1, 12
                remove_record(20+i) = ct_all(ioff+i)
             enddo
             ioff = ioff + 12
             do i = 1, 12
                remove_record(52+i) = ct_all(ioff+i)
             enddo
             ioff = ioff + 12
             do i = 1, 6
                remove_record(84+i) = ct_all(ioff+i)
             enddo
             ioff = ioff + 6
             do i = 1, 19
                remove_record(100+i) = ct_all(ioff+i)
             enddo

C    Translate floating-point values to IEEE format

             call util_rndr32(src_amp_factor,src_amp_factor,1)
             call util_rndr64(rem_refdat,rem_refdat,1)
             call util_rndr32(rem_bandwidth,rem_bandwidth,1)
             call util_rndr32(rem_integt,rem_integt,1)
             call util_rndr32(rem_model_max,rem_model_max,1)
             call util_rndr64(rem_ra,rem_ra,6)
             call util_rndr64(rem_dec,rem_dec,6)
             call util_rndr32(rem_src_flux,rem_src_flux,6)
             call util_rndr32(rem_mod_mean,rem_mod_mean,1)
             call util_rndr32(rem_mod_sigma,rem_mod_sigma,1)

C    Write source definition back to control tables

             ioff = src_ptr + (isrc-1)*(len_pack+rem_length)
             do i = 1, len_pack
                ct_all(ioff+i) = src_pack(i)
             enddo
             ioff = ioff + len_pack
             do i = 1, rem_length
                ct_all(ioff+i) = remove_record(i)
             enddo

             no_samp = no_samp + src_num_samp

          enddo

          samp_pages = src_samp_len/page

          ctv2_pack(6) = num_src
          ctv2_pack(7) = max_src
          ctv2_pack(9) = len_pack + rem_length

C  Convert calibration source definitions, Section 9C

       elseif (sf_type .eq. 3) then

          do isrc = 1, num_src

             do i = 1, cal_length
                cal_record(i) = 0
             enddo

C    Read source definition from control tables, realigning
C    floating-point variables to word boundaries

             ioff = src_ptr + (isrc-1)*len_src
             do i = 1, len_pack
                src_pack(i) = ct_all(ioff+i)
             enddo
             ioff = ioff + len_pack
             do i = 1, 11
                cal_record(i) = ct_all(ioff+i)
             enddo
             do i = 12, 74
                cal_record(i+1) = ct_all(ioff+i)
             enddo

C    Translate floating-point values to IEEE format

             call util_rndr32(src_amp_factor,src_amp_factor,1)
             call util_rndr64(cal_refdat,cal_refdat,1)
             call util_rndr32(cal_bandwidth,cal_bandwidth,1)
             call util_rndr32(cal_integt,cal_integt,1)
             call util_rndr64(cal_ra,cal_ra,6)
             call util_rndr64(cal_dec,cal_dec,6)
             call util_rndr32(cal_src_flux,cal_src_flux,6)
             call util_rndr32(cal_mod_mean,cal_mod_mean,1)
             call util_rndr32(cal_mod_sigma,cal_mod_sigma,1)

C    Write source definition back to control tables

             ioff = src_ptr + (isrc-1)*len_src
             do i = 1, len_pack
                ct_all(ioff+i) = src_pack(i)
             enddo
             ioff = ioff + len_pack
             do i = 1, cal_length
                ct_all(ioff+i) = cal_record(i)
             enddo

             no_samp = no_samp + src_num_samp

          enddo

          samp_pages = src_samp_len/page

       endif

       end


*+
      subroutine CONVERT_RT ( lun, src_num, samp_num, status )
C
C  Convert sample redtape to Unix format
C
C  Reads sample redtape for the given sample and converts the
C  floating point values (RA and Dec) to IEEE format.
C
C  DJT, 8 December 93.
C
*-
      integer  lun, src_num, samp_num, status
c
      include '/mrao/post/include/samp_rt.inc'
      include '/mrao/post/include/sf_pack.inc'
      include '/mrao/post/include/sf_buffer.inc'
c
      integer  offset, rt_offset
      integer  i

      if (status.ne.0) return

      call read_buffer ( lun, src_num, samp_num, status )
      if ( status .ne. 0 ) goto 999

      offset = buffer_ptr + data_offset +
     *         (curr_samp-first_samp)*samp_len - 1

      rt_offset = offset + start_rt - 1
      do 10 i = 1, length_rt
          samp_rt( i )= buffer( rt_offset+i )
   10 continue

      call util_rndr64(samp_ra,samp_ra,2)

      do 11 i = 1, length_rt
          buffer( rt_offset+i ) = samp_rt( i )
   11 continue

      update_flag = update_flag + 1

      return

 999  call smp_wrerr( status, 'in subroutine CONVERT_RT' )

      end





*+
      subroutine CONVERT_MON ( lun, src_num, samp_num, status)
C
C  Convert floating point data in the monitor block to Unix format
C
C  For Ryle Telescope, converts derived rain correction factors.
C
C  DJT, 26 November 98.
C
*-
      integer   lun, src_num, samp_num, status
c
      include  '/mrao/post/include/samp_rt.inc'
      include  '/mrao/post/include/sf_pack.inc'
      include  '/mrao/post/include/sf_buffer.inc'
      include  '/mrao/post/include/mon_v2_block.inc'
      include  '/mrao/post/include/samplib_errors.inc'
c
      integer   offset, mon_offset
      integer   i

      if ( status .ne. 0 ) return

      call read_buffer ( lun, src_num, samp_num, status )
      if ( status .ne. 0 ) goto 999

      offset = buffer_ptr + data_offset +
     *         (curr_samp-first_samp)*samp_len - 1

      mon_offset = offset + start_mon - 1
      mon_offset = 2*mon_offset

      if ( data_type .eq. 1 ) then

          do i = 1, mon_length
              mon_block(i) = ibuff(mon_offset+i)
          enddo
          call util_rndr32(mon_rfac,mon_rfac,num_aerials)
          do i = 1, mon_length
              ibuff(mon_offset+i) = mon_block(i)
          enddo

          update_flag = update_flag + 1

      endif

      return

 999  call smp_wrerr( status, 'in subroutine CONVERT_MON')

      end
