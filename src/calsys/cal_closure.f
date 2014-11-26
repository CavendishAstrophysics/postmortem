C
C
C
C+cal_closure

       subroutine cal_closure( nae, num_vis, vis_phase, mat, iprint,
     *                         ae_list, num_closure, closure, s    )
C      -------------------------------------------------------------
C
C Calculate the closure quantities for the supplied visibility phases
C
C Given:
C  number of aerials
       integer        nae
C  number of visibilities
       integer        num_vis
C  visibility phases
       real*8         vis_phase(num_vis)
C  aerial matrix (aerial pairs for each visibility)
       integer*2      mat(2,num_vis)
C  print control parameter (=0 no output printed)
       integer        iprint
C  aerial list
       integer        ae_list(nae)
C
C Returned
C  number of closure quantities found
       integer        num_closure
C  closure phases ordered by incresing aerial numbers
       real*8         closure(*)
C  status return
       integer        s
C
C The closure quantities are calculated and returned to the calling
C routine.  Optionally a formatted printing of the closure quantities
C may be obtained on the current output device if iprint is >0.  If
C iprint is <0 the routine will only report those values of closure
C phase larger (in magnitude) than the current gate setting (degrees).
C
C PA, 7/9/89
C PA, 18/9/89:  Support for checking and reporting of large closure
C               quantities added
C-
C local variables
       integer      iout
       integer      n1, n2, n3, n, i, nnn(3), isb, ich
       character*1  sub_bands(5)
       character*80 string
       integer      len_string
       data  sub_bands / 'A', 'B', 'C', 'D', 'E' /

C include standard constants for radians-->degreed conversion
       include '/mrao/include/constants.inc'
C include function
       include '/mrao/include/chrlib_functions.inc'
C include control information
       include '/mrao/post/include/cal_control.inc'

C check status on entry
       if ( s.ne.0 ) return

C initialise for printing results
       isb = current_subband
       ich = current_channel
       if (iprint.gt.0) then
         call io_enqout(iout)
         if (current_channel.ne.0) then
           string = ' '
           write(string,'('': Sub-band '',a1,''  Channel '',i1)')
     *     sub_bands(isb),ich
         else if (current_subband.ne.0) then
           string = ' '
           write(string,'('': Sub-band '',a1)')
     *     sub_bands(isb)
         else
           string = ' '
         end if
         len_string = chr_lenb(string)
         write(iout,5)string(1:len_string)
  5      format(1X/1X,'Closure Quantities Listed',a/
     *             1X,'-------------------------'/1X)
       else if (iprint.lt.0) then
         call io_enqout(iout)
       end if

C loop through all the possible aerial triplets
       num_closure = 0
       do n1=1,nae
         do n2=n1+1,nae
           do n3=n2+1,nae
             do n=1,3
               nnn(n) = 0
             end do
C .. find visibilities for each aerial triplet
             do i=1,num_vis
               if ( (mat(1,i).eq.n1 .and. mat(2,i).eq.n2) ) then
                 nnn(1) = i
               elseif ( (mat(2,i).eq.n1 .and. mat(1,i).eq.n2) ) then
                 nnn(1) = -i
               end if
               if ( (mat(1,i).eq.n2 .and. mat(2,i).eq.n3) ) then
                 nnn(2) = i
               elseif ( (mat(2,i).eq.n2 .and. mat(1,i).eq.n3) ) then
                 nnn(2) = -i
               end if
               if ( (mat(1,i).eq.n3 .and. mat(2,i).eq.n1) ) then
                 nnn(3) = i
               elseif ( (mat(2,i).eq.n3 .and. mat(1,i).eq.n1) ) then
                 nnn(3) = -i
               end if
             end do
C .. calculate the closure quantity for this aerial triplet
             if (nnn(1).ne.0 .and. nnn(2).ne.0 .and. nnn(3).ne.0) then
               num_closure = num_closure+1
               closure(num_closure) = 0.0D+0
               do n=1,3
                 if (nnn(n).gt.0) then
                   closure(num_closure)=closure(num_closure)+
     *                                  vis_phase(nnn(n))
                 else
                   closure(num_closure)=closure(num_closure)-
     *                                  vis_phase(-nnn(n))
                 end if
               end do
C .. if required report results on the current output device
               if (iprint.gt.0) then
                 write(iout,10)num_closure,
     *                         ae_list(n1),ae_list(n2),ae_list(n3),
     *                         closure(num_closure)/const_d2r
               else if (iprint.lt.0) then
                 if (abs(closure(num_closure)/const_d2r)
     *               .gt.closure_gate) then
                   if (current_channel.ne.0) then
                     string = ' '
                     write(string,'(''Closure-Error: Aes: '',3i3,
     *                              '' SB: '',a1,'' CH: '',i1,
     *                              f10.3,'' degrees'')')
     *                              ae_list(n1),ae_list(n2),
     *                              ae_list(n3),
     *                              sub_bands(isb),ich,
     *                              closure(num_closure)/const_d2r
                   else if (current_subband.ne.0) then
                     string = ' '
                     write(string,'(''Closure-Error: Aes: '',3i3,
     *                              '' SB: '',a1,
     *                              f10.3,'' degrees'')')
     *                              ae_list(n1),ae_list(n2),
     *                              ae_list(n3),
     *                              sub_bands(isb),
     *                              closure(num_closure)/const_d2r
                   else
                     string = ' '
                     write(string,'(''Closure-Error: Aes: '',3i3,
     *                              f10.3,'' degrees'')')
     *                              ae_list(n1),ae_list(n2),
     *                              ae_list(n3),
     *                              closure(num_closure)/const_d2r
                   end if
                   len_string = chr_lenb(string)
                   write(iout,'(1X,A)') string(1:len_string)
                 end if
               end if
             end if
           end do
         end do
       end do

10     format(1X,'Number: ',i3,'  Aerials:',3i3,'  Closure-Phase: ',
     *        F9.3,' degrees')

C report any error occuring in this routine
       if (s.ne.0) call cal_wrerr( s, 'in subroutine cal_closure' )
       end
