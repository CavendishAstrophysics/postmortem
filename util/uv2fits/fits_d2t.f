*+FITS_D2T - main pixel data to buffer, and hence to output device
*
*
       subroutine fits_d2t(key,data,leni2,status)
*
*
*  Key = 'OPEN'  - open tape; rewind
*        'BEGIN' - begin; set buffer pointers to zero etc
*        'WRITE  - write; transfer data(1) to data(leni2), where data
*                  is an integer*2 array, to next bit of free space in
*                  the buffer, transferring full blocks to tape as
*                  required
*        'FILL'  - fill; unless current buffer is empty, transfer it,
*                  padded with repeated data(1) to completion, to tape
*        'END'   - end; fill as above and write tape mark
*        'CLOSE' - close; write second tape mark, close tape, rewind
*
*
*  S.Kenderdine   6 July 1988
*                 20 July 1988 - tape calls, revised 'C'
*                 21 July 1988 - direct, fill, general tidy
*                 22 July 1988 - excise direct
*                 3 August 1988 - mtlib calls
*  DJT            29 August 1990 - revised 'O','B','E','C'
*                 17 March 1992 - revised for output to disc
*  DJT            16 February 1998 - Unix version, no tape option
*
*-

       include '/mrao/include/iolib_functions.inc'
       include 'uv2fits_common.inc'

       character key*(*)
       integer*2 data(*)
       integer leni2,status

       character k1*1

       integer i, n
       integer point
       integer i2rec, lim_i2rec
       integer i4rec, lim_i4rec
       integer nrecords

       real*8  records

       logical ntap
       save

* Entry

       if(status.ne.0) return

       k1=key(1:1)
       call chr_chucas(k1)

*
* Open
*
       if (k1.eq.'O') then

*  Select disc or tape output

         medium = 'DISC'

         ntap=.false.
         ytap=.not.ntap
         if(ytap) then
           stap=0
         else
           stap=-1
         endif

*
* Begin .. position tape
*
       elseif (k1.eq.'B') then

         call io_operan(idev,ffile,'WRITE',block,0,status)
         point=1

         n=1
         do i=2,naxis
           n=n*naxisn(i)
         enddo
         n=bitpix*gcount*(pcount+n)
         records=n/(8.0*block)
         nrecords=records
         if(nrecords.ne.records) nrecords=nrecords+1
         write(iodev,1800) nrecords
 1800    format(/i6,' FITS data records, plus header & tables')
         n=1

*
* Write
*
       elseif (k1.eq.'W') then

         do i=1,leni2
           i2buffer(point)=data(i)
           point=point+1
           if(point.gt.lim_i2buff) then
             call io_wrfile(idev,n,i2buffer,lim_i4buff,stap)
             if(ytap.and.stap.ne.0) call io_wrerr(stap,' on write')
             n=n+nblock
c            if(vdu) write(iodev,1000) n-1
c            if(vdu .and. mod(n,10).eq.0) write(iodev,1000) n-1
 1000        format('+',i5,' records written')
             point=1
           endif
         enddo

*
* Fill
*
       elseif (k1.eq.'F') then

*  .. pad rest of current record with i*2 from data(1)

         i2rec=block/2
         if(mod(point,i2rec).ne.1) then
           lim_i2rec=i2rec*((point-2)/i2rec+1)
           do i=point,lim_i2rec
             i2buffer(i)=data(1)
           enddo
           point=lim_i2rec+1
           if(point.gt.lim_i2buff) then
             call io_wrfile(idev,n,i2buffer,lim_i4buff,stap)
             if(ytap.and.stap.ne.0) call io_wrerr(stap,' on write')
             n=n+nblock
             point=1
           endif
         endif

*
* End
*
       elseif (k1.eq.'E') then

*  .. write last incomplete block if any, padding rest of buffer with
*      i*2 from data(1)

         if(point.ne.1) then
           do i=point,lim_i2buff
             i2buffer(i)=data(1)
           enddo
           i2rec=block/2
           i4rec=block/4
           lim_i4rec=i4rec*((point-2)/i2rec+1)
           call io_wrfile(idev,n,i2buffer,lim_i4rec,stap)
           n=n+lim_i4rec/i4rec
           if(ytap.and.stap.ne.0) call io_wrerr(stap,' on write')
           point=1
         endif

         write(iodev,1000) n-1
         close(idev)
         n=0

*
* Close .. final tape mark(s), close mag tape unit
*
       elseif (k1.eq.'C') then

       endif

       return

       end
