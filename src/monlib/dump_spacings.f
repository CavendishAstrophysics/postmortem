*$(1)  POSTMORTEM monitor system commands


*+DUMP_SPACINGS

       subroutine dump_spacings (file, status)
C      ---------------------------------------
C
C  Executes the DUMP-SPACINGS command.
C
C  Given:
C      FILE          char*(*)      current sample file name
C      STATUS        integer       status value
C
C  Routine to dump spacings from the current sample file, writing
C  the sample redtape and visibility cos and sine for a specified
C  range of samples.
C
*-
       character  file*(*)
       integer    status
c
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/post/include/global_constants.inc'
       include '/mrao/post/include/samplib_errors.inc'
c
       character  list*80, tscope*6
       integer    ilist(max_vis)
       integer    ifile, iout, iold, termi, termo
       integer    i, ii, is, isamp, isamp1, isamp2, num_samp
       integer    iae1, iae2, isp, iba, ich
       integer    n, ns, bad_samp
       integer    samp_status, samp_sid_time, samp_wt
       real*8     samp_ra, samp_dec
       real*4     vis(2)
       character  sub_bands(5)*1
       data       sub_bands /'A','B','C','D','E'/
c
       if (status.ne.0) return
c
c  Open the sample file
c
       call open_sf(ifile,file,'read',0,status)
       call open_source(ifile,1,0,status)
c
c  Prompt for further parameters
c
       call get_spacings( ifile, 'spacing list :', ' ', list, ilist,
     :                    max_vis, ns, status                        )
       if (ns.gt.0) then
         call enq_numsamp ( ifile, 1, num_samp, status )
         isamp1=1
         isamp2=num_samp
         call io_geti('first sample:','*',isamp1,status)
         call io_geti('last sample :','*',isamp2,status)
         isamp1 = max(1,isamp1)
         isamp2 = min(num_samp, isamp2)
c
         call io_enqout(iold)
         call io_opeout(iout,status)
         call io_enqtio(termi,termo)
         if (status.eq.0) then
           if (iout.ne.termo) write(iout,*)'DUMP-SPACINGS'
           write(iout,*)
           call io_lstfil(iout,file,status)
         endif
       endif
c
       if (ns.gt.0 .and. status.eq.0) then
c
c  Dump each of the specified spacings in turn to the output file
c
         is=0
         do while (is.lt.ns .and. status.eq.0)
           is=is+1
c
           n=0
           bad_samp=0
           ii=ilist(is)
           call enq_tscope(ifile,tscope,i,status)
           call enq_vis_desig(ifile,ilist(is),isp,iba,ich,status)
           call enq_ae_vis(ifile,ilist(is),iae1,iae2,status)
           if (tscope.eq.'T151' .or. tscope.eq.'38MHZ') then
             write(iout,2)isp,iae1,iae2
             write(iout,4)
           elseif (tscope.eq.'RYLE') then
             write(iout,3)isp,sub_bands(iba),ich,iae1,iae2
             write(iout,4)
           endif
           do isamp=isamp1,isamp2
             call read_sample(ifile,1,isamp,1,1,ii,vis,status)
             if (status.eq.BAD_SAMPLE) then
               bad_samp=bad_samp+1
               vis(1)=0.0
               vis(2)=0.0
               status=0
             endif
             if (status.eq.0) then
               n=n+1
               call enq_samp_rt(ifile,samp_status,samp_ra,samp_dec,
     :                                samp_sid_time,samp_wt,status)
               write(iout,5)isamp,samp_status,samp_ra,samp_dec,
     :                                samp_sid_time,samp_wt,vis
               if (io_attn(status)) goto 1
             endif
           enddo
    1      if (n.gt.0) write(iout,6)n,isamp1,isamp1+n-1,bad_samp
c
         enddo
c
    2    format(' Spacing',I4,', aerials',I3,',',I2/)
    3    format(' Spacing',I4,' sub-band ',a1,' channel ',I1,
     :                                        ' aerials',I3,',',I2/)
    4    format(8X,'status',6X,'RA',6X,'dec',4X,'s-time',4X,'wt',
     :                                            6X,'cos',9X,'sin')
    5    format(I6,X,O8.8,X,2F8.4,X,I8,X,I5,X,1P,2G12.4)
    6    format('0',I5,' samples read,',I5,' to',I5,',',3X,
     :                               'including',I4,' bad samples'/)
c
       endif
c
       if (status.eq.USR_BREAK) then
         status=0
       else if (status.ne.0) then
         call mon_wrerr(status,'in routine DUMP_SPACINGS')
       endif
c
       call close_source(ifile,1,status)
       call close_sf(ifile,status)
c
       call io_close(iout,status)
       call io_setout(iold)
       write(iold,*)
c
       end
