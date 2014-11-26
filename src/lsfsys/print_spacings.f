C+PRINT_SPACINGS

       subroutine print_spacings (lsf_num, status)
C      -------------------------------------------
C
C  Executes the PRINT-SPACINGS command.
C
C  Given:
C      LSF_NUM       integer       logical sample file number
C      STATUS        integer       status value
C
C  Scans the current logical sample file over a specified range and
C  prints out cos, sin, amplitude and phase for merged spacings.
C
C  The STATUS value should be zero on entry.
C
C  [DJT, 23/8/93]
C  last mod 13 Apr 2000 GP [cdate*11]
*-
       integer    lsf_num, status

       include '/mrao/post/include/global_constants.inc'
       include '/mrao/post/include/merge_types.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/include/constants.inc'

       character  title(4)*80
       character  list*80, file*64
       character  string*14, cdate*11, ctime*8, cstime*8
       integer    naes, num_spac, group_num, no_group, merge_type
       integer    idate(3), itime(3), sf_lun, src_num, sid, tscope
       integer    ibuff, ibuff1, ibuff2, num_buff, vis_ptr
       integer    ia, iae, nae, nsp, nsb, nch
       integer    istat, stime, utime, utime1
       integer    i, iold, iout, ls, lt
       integer    termi, termo
       real       rcos, rsin, ramp, rphi
       real*8     mjd_st0, mjd_samp1
       real*8     mjd_ut0, ut_st0
       real*8     frac
       logical    print

       integer    max_buff
       parameter (max_buff = max_aes*max_samp/2)

       integer    ae_list(max_aes)
       integer    sp_list(max_vis)
       integer    merge_list(2*max_vis)
       integer    group_size(max_vis)
       integer    sid_time(max_samp)
       complex    vis_list(max_vis)
       complex    vis_merge(max_buff)

C  Place work arrays in the work space common block
       common  /post/  ae_list, sp_list, merge_list,
     :                 sid_time, group_size,
     :                 vis_list, vis_merge

C  Check status on entry
       if (status.ne.0) return

C  Enquire sample file info and output unit
       call lsf_enq_sf( lsf_num, sf_lun, src_num, status )
       call io_enqout(iold)
       call io_enqtio(termi,termo)
       iout=iold

C  Get telescope type
       call enq_phys_tscope( sf_lun, tscope, status )

C  Get number of aerials, spacings, subbands, channels
       call enq_obsdef( sf_lun, nae, nsp, nsb, nch, status )

C  Prompt for merge type
       call get_merge( sf_lun, 'Specify merge type:', 'none',
     :                                               merge_type, status)

C  Prompt for spacing list and list of spacings to scan
  1    call get_spacings( sf_lun, 'Select spacings : ', 'all',
     :                      list, sp_list, max_vis, num_spac, status )
       call lsf_set_spacings( lsf_num, num_spac, sp_list, 2, status )
       if (status.eq.0) call chr_chucas(list)

C  Define merge list using the specified merge-type
       call set_merge( sf_lun, sp_list, num_spac, merge_type,
     :                     merge_list, no_group, group_size, status )

C  Prompt for list of aerials to display
       if (merge_type .eq. aerial_merge .or.
     :     merge_type .eq. fr_aerial_merge) then
         call get_aerials( sf_lun, 'Display aerials:', 'all',
     :                       string, ae_list, max_aes, naes, status )
       endif

C  Get range of sample buffers to scan
       call lsf_get_range(lsf_num, ibuff1, ibuff2, status)

C  Check available workspace buffer size
       if (status.eq.0) then
         num_buff = ibuff2 - ibuff1 + 1
         if (num_buff*no_group .gt. max_buff) then
            write(iout,*)'*** insufficient workspace for this selection'
            goto 1
         endif
       endif

C  Prompt for output file
       call io_opeout(iout,status)
       if (status.eq.0) then
         if (iout.ne.termo) write(iout,*)'PRINT-SPACINGS'
         write(iout,*)
         inquire (unit=sf_lun, name=file)
         call io_lstfil(iout,file,status)
       endif


       if (status.eq.0) then

C  Read the data

         vis_ptr = 1
         num_buff = 0
         do ibuff = ibuff1, ibuff2

           call lsf_set_buffer( lsf_num, ibuff, status)
           call lsf_get_vis( lsf_num, max_vis, vis_list, num_spac,
     :                                                          status )
           call merge_vis_buffer( vis_list, num_spac,
     :                            merge_list, no_group, group_size,
     :                            vis_merge(vis_ptr), status )
           call lsf_get_sid( lsf_num, sid, status )
           if (status.ne.0) goto 5

           num_buff = num_buff + 1
           sid_time(num_buff) = sid
           vis_ptr = vis_ptr + no_group

         enddo

C  Initialise header text
         call lsf_title(lsf_num,list,ibuff1,ibuff2,title,status)

C  Find Modified Julian Date for 0h ST
         call enq_mjd_st0( sf_lun, mjd_st0, status )
         ut_st0  = (mjd_st0-int(mjd_st0))*24.0D+0

C  Find Modified Julian Date for 0h UT on date of first sample
         mjd_samp1 = mjd_st0+
     :              dble(sid_time(1))/(864000.0d0*const_sut2sst)
         mjd_ut0 = int(mjd_samp1)
         call sla_djcl(mjd_samp1,idate(3),idate(2),idate(1),frac,istat)
         call chr_chdate(idate,1,cdate,ls)

C  Print out data for each merge group in turn

         do group_num = 1, no_group

           if (group_size(group_num) .ne. 0) then

              print = .true.
              if (merge_type.eq.aerial_merge) then
                print = .false.
                do i = 1, naes
                  if (group_num.eq.ae_list(i)) print = .true.
                enddo
              elseif (merge_type.eq.fr_aerial_merge) then
                print = .false.
                ia = (group_num-1)/(nch*nsb)+1
                call enq_iae_code(sf_lun, ia, iae, status)
                do i = 1, naes
                  if (iae.eq.ae_list(i)) print = .true.
                enddo
              endif

              if (print) then

C    Write title for this group
               call enq_grp_desc( sf_lun, merge_type, sp_list, num_spac,
     :                            merge_list, no_group, group_size,
     :                            group_num, title(2), status )
                lt = chr_lenb(title(2))+1
                title(2)(lt:) = ' from '//list
                write(iout,*)
                write(iout,'(x,a)')title
                write(iout,2) mjd_ut0, cdate
                write(iout,3)

C    Write data for this group

                utime1 = 0
                vis_ptr = group_num
                do ibuff = 1, num_buff
                  rphi = 0.0
                  rcos = real(vis_merge(vis_ptr))
                  rsin = aimag(vis_merge(vis_ptr))
                  ramp = cabs(vis_merge(vis_ptr))
                  if (ramp.gt.0.0) then
                    rphi = atan2(rsin,rcos)/const_d2r
                  endif
                  stime = sid_time(ibuff)/10
                  call util_stohms(stime,itime)
                  call chr_chtime(itime,cstime,ls)
                  utime = ut_st0*3600 + stime/const_sut2sst
                  utime = mod(utime, 86400)
                  if (utime.lt.utime1) utime = utime + 86400
                  call util_stohms(utime,itime)
                  call chr_chtime(itime,ctime,ls)
                  write(iout,4) cstime, ctime, rcos, rsin, ramp, rphi
                  vis_ptr = vis_ptr + no_group
                  if (io_attn(status)) goto 5
                  utime1 = utime
                enddo
             endif
           endif

    2      format(/2X,'Modified Julian date',F9.2,'  at 0h on ',A)
    3      format(/7X,'ST',9X,'UT',8X, 'cos',12X,'sin',12X,
     :                                                'amp',8X, 'phase')
    4      format(X,A,3X,A,3(3X,G12.5),3X,F6.1)

         enddo

       endif

C  Tidy up

    5  call io_close(iout,status)
       call io_setout(iold)
       write(iold,*)

       if (status.ne.0 .and. status.ne.usr_break) then
         call smp_wrerr(status,'in routine PRINT-SPACINGS')
       endif

       end
C
C
