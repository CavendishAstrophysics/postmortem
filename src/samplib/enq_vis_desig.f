

C+ enq_vis_desig

       subroutine enq_vis_desig ( lun, ivis, isp, iba, ich, s )
C
C returns the spacing, sub-band and channels for a given visibility
C
C Given
C   LUN            -           I4        -    SF logical unit number
C   IVIS           -           I4        -    visibility index number
C
C Returned
C   ISP            -           I4        -    spacing code
C   IBA            -           I4        -    sub-band code
C   ICH            -           I4        -    channel code
C   S              -           I4        -    error return code
C
C Returns the spacing, sub-band and channel codes for a given visibility
C
C [PA, 8/11/88]
C-

       integer     lun, s
       integer     index_sp, index_ba, index_ch
       integer     ivis, isp, iba, ich
       integer     nae, nsp, nba, nch

C check status on entry
       if (s .ne. 0) return

C find total numbers of spacings, bands and channels present
       call enq_obsdef(lun,nae,nsp,nba,nch,s)

C determine index numbers for spacings, sub-bands and channels
       index_sp = (ivis - 1)/(nba*nch) + 1
       index_ba = (ivis - (index_sp-1)*nba*nch - 1)/nch + 1
       index_ch = (ivis - (index_sp-1)*nba*nch - (index_ba-1)*nch)

C convert to designations
       call enq_isp_code(lun,index_sp,isp,s)
       call enq_iba_code(lun,index_ba,iba,s)
       call enq_ich_code(lun,index_ch,ich,s)

C check status on exit to provide a trace-back
       if (s.ne.0) then
         call smp_wrerr(s,'in routine ENQ_VIS_DESIG')
       end if

       return
       end
