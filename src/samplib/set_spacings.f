

C+ SET_SPACINGS

       subroutine set_spacings ( lun, list, ilist, mvis, nvis, status )
C
C Resolves a spacing list into index numbers.
C
C  Given:
C      LUN       integer     sample file logical unit number
C      LIST      char*(*)    string containing input spacing list
C      MVIS      integer     maximum number of visibilities in the list
C
C  Returned:
C      ILIST     integer(*)  array containing spacing index numbers
C      NVIS      integer     number of visibilities in the list
C      STATUS    integer     status value
C
C  Routine to resolve a spacing list, presented as a character string,
C  into an array containing valid visibility index numbers, arranged in
C  increasing order.  Note that all occurrences of repeated spacings
C  are included in the output array.
C
c  NB this version does not expect semicolons as separators.
C  The input line may contain a number of syntactic elements separated
C  by key"words". Each element is scanned and the appropriate list
C  constructed. Each element should begin with a key as follows:
C    SP   -   SPacings:  List in format i,j,k or i-k or i(j)k or ALL
C    AE   -   AErials:   List in format i,j,k / k,l,m or i-j / k,l etc.
C                                       or i,j,l or i-k or ALL
C    HUT  -   HUTs:      List in format similar to Aerials
C    SB   -   Sub-Bands: List in format A,B,C or A-E or ALL
C    CH   -   CHannels:  List in format 1,2,3 or 1-3 or ALL
C
C  HUT option only applies to the low frequency telescopes while
C  BA and CH are only meaningful for the Ryle telescope. A list
C  without a key is treated as a spacing list. Syntactic elements are
C  scanned left to right, those on the right take precedence over those
C  to the left.
C
C  Examples:
C     SP  1-500
C     AE  1-20 / ALL
C     AE  1-20
C     SP  1-10  SB ALL  CH 1,2,3,7-8
C
C  The status value should be zero on entry.  The returned status value
C  is zero unless the input string does not contain a valid spacing list
C  (ILL_SPLIST).
C
C  [PA, 6/12/88]
C  [DJT, 27/9/89]
C  [GGP 31 May 1999]
*-
       include '/mrao/post/include/global_constants.inc'
       include '/mrao/post/include/phys_tscopes.inc'
       include '/mrao/post/include/samplib_errors.inc'

       character list*(*)
       integer   lun, mvis, ilist(*), nvis, status

C number of spacings, sub-bands and channels present & max numbers
       integer   nae, nsp, nba, nch
       integer   nae_max, nsp_max, nba_max, nch_max

C cumulative lists of spacing, sub-bands and channels to include
       integer   sp_ok(max_spac), ba_ok(max_subb), ch_ok(max_channel)

C telescope identifier code
       integer   itscope

C work spaces used for decoding lists
       integer   array(max_spac)
       character string*120

C list of aerials used in decoding AE and HUTS options
       integer   ieast(max_aes), iwest(max_aes), neast, nwest, is1, is2

C counters
       integer   i0, i1, i2, i3, i4, im, is, ix, i, k
       integer   nok, min_sp, max_sp
       integer   n, n1, n2, ne, nw
       integer   iba, ich, isp

C integer string pointer functions
       integer   chr_intlc, chr_lenb

C check status on entry
       if (status.ne.0) return

C enquire telescope type and number of spacing etc. present
       call enq_phys_tscope(lun,itscope,status)
       if (itscope.ne.clfst .and. itscope.ne.five_km) then
         status = ill_tscope
         goto 999
       end if
       call enq_teldef(lun,nae_max,nsp_max,nba_max,nch_max,status)
       call enq_obsdef(lun,nae,nsp,nba,nch,status)

C prepare input string
       call chr_chucas(list)

C define defaults as including all spacings, sub-bands and channels
       do n = 1,nsp
         sp_ok(n) = 1
       end do
       do n = 1,nba
         ba_ok(n) = 1
       end do
       do n = 1,nch
         ch_ok(n) = 1
       end do

C parse the input line for syntactic elements and process each in turn
       im = chr_lenb(list)
       i1 = 1
       do while (i1.le.im)
        i1 = chr_intlc(list(i1:im)) + i1 - 1

* find the next occurrence of a key (or ';')

        i0 = 10000
        ix = index(list(i1+1:im), 'SB')
        if (ix .ne. 0) i0 = min(i0, ix)
        ix = index(list(i1+1:im), 'CH')
        if (ix .ne. 0) i0 = min(i0, ix)
        ix = index(list(i1+1:im), 'AE')
        if (ix .ne. 0) i0 = min(i0, ix)
        ix = index(list(i1+1:im), 'HUT')
        if (ix .ne. 0) i0 = min(i0, ix)
        ix = index(list(i1+1:im), 'SP')
        if (ix .ne. 0) i0 = min(i0, ix)
        ix = index(list(i1+1:im), ';')
        if (ix .ne. 0) i0 = min(i0, ix)

        if (i0 .eq. 10000) then
                i2 = im
        else
                i2 = i0 + i1 - 1
        endif

C .. decode options
         if      (list(i1:i1) .eq. ';') then ! ignore ';'
                i1 = i1 + 1

         else if (list(i1:i1+1).eq.'SB') then
C ... Sub-Bands
           i4 = chr_intlc(list(i1+2:i2)) + i1 + 1
           if (list(i4:i4+2).ne.'ALL') then
         call chr_chlstc(list(i1+2:i2),string,array,nba_max,nok,status)
             if (status.ne.0) goto 998
             do n1=1,nba
               ba_ok(n1) = 0
               call enq_iba_code(lun,n1,iba,status)
               do n=1,nok
                 if (iba.eq.array(n)) ba_ok(n1) = 1
               end do
             end do
           end if

         else if (list(i1:i1+1).eq.'CH') then
C ... CHannels
           i4 = chr_intlc(list(i1+2:i2)) + i1 + 1
           if (list(i4:i4+2).ne.'ALL') then
             call chr_chlsti(list(i1+2:i2),array,nch_max,nok,status)
             if (status.ne.0) goto 998
             do n1=1,nch
               ch_ok(n1) = 0
               call enq_ich_code(lun,n1,ich,status)
               do n=1,nok
                 if (ich.eq.array(n)) ch_ok(n1) = 1
               end do
             end do
           end if

         else if (list(i1:i1+1).eq.'AE' .or. list(i1:i1+2).eq.'HUT')
     *     then
C ... aerials or huts
           is = index(list(i1:i2),'/') + i1 - 1
           if (is.gt.i1) then
             i3 = chr_intlc(list(is+1:i2))
             call set_aerials(lun,list(i1:is-1),ieast,nae_max,
     *                                                     neast,status)
             call set_aerials(lun,list(is+i3:i2),iwest,nae_max,
     *                                                     nwest,status)
           else
             call set_aerials(lun,list(i1:i2),ieast,nae_max,
     *                                                     neast,status)
             call set_aerials(lun,'ALL',iwest,nae_max,nwest,status)
           end if
           if (status.ne.0) goto 999
           do n = 1,nsp
             call enq_ae_spac(lun,n,is1,is2,status)
             sp_ok(n) = 0
             do ne = 1,neast
               do nw = 1,nwest
                 if ( (is1.eq.ieast(ne).and.is2.eq.iwest(nw)) .or.
     *                (is2.eq.ieast(ne).and.is1.eq.iwest(nw)) ) then
                   sp_ok(n) = 1
                 end if
               end do
             end do
           end do

         else
C .. SPacings
           if (list(i1:i1+1).eq.'SP') then
             i3 = 2
           else
             i3 = 0
           end if
           i4 = chr_intlc(list(i1+i3:i2)) + i1 + i3 - 1
           if (list(i4:i2).ne.'ALL') then
             do n=1,nsp
               sp_ok(n) = 0
             end do

             call enq_isp_code(lun,1,min_sp,status)
             call enq_isp_code(lun,nsp,max_sp,status)

             call chr_chlsti(list(i1+i3:i2),array,max_sp,nok,status)
             if (status.ne.0) goto 998
             call util_sorti(array,nok)
             if (status.eq.0) then
C
               i=1
               do k=1,nok

C ... reject spacing numbers out of range
                 if (array(k).ge.min_sp .and. array(k).le.max_sp) then

C ... convert spacing number to index number, add duplicate spacings
C ... to the end of the list.
  100              call enq_isp_code(lun,i,isp,status)
                   if (isp.le.array(k)) then
                     if (isp.eq.array(k)) sp_ok(i) = 1
                     if (i.lt.nsp) then
                       i=i+1
                       goto 100
                     endif
                   endif
C
                 end if
               end do
C
             end if
           end if

         end if

C .. increment i1 pointer
         i1 = i2 + 1

       end do

C Sort through the lists we have and construct the index array
C which can then be sorted
       nvis = 0
       do n=1,nsp
         if (sp_ok(n).eq.1) then
           do n1 = 1,nba
             if (ba_ok(n1).eq.1) then
               do n2 = 1,nch
                 if (ch_ok(n2).eq.1) then
                   nvis = nvis + 1
                   if (nvis.le.mvis) then
                     ilist(nvis) = (n-1)*nba*nch + (n1-1)*nch + n2
                   end if
                 end if
               end do
             end if
           end do
         end if
       end do

 998   continue
       if (status.eq.0) then
         call util_sorti(ilist,nvis)
       else
         status = ILL_SPLIST
       end if
C
 999   if (status.ne.0) then
         call smp_wrerr(status,'in subroutine SET_SPACINGS')
       end if

       end
