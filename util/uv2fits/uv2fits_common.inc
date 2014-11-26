**
*
*  UV2FITS include file
*
*
*  S.Kenderdine   21 June 1988
*                 20 July 1988 - tape and disc buffers
*                 22 July 1988 - spmax, for partial transfers
*                 25 July 1988 - lim_i4buff, tdev renamed tfile
*                 27 July 1988 - linem i2line etc
*                 28 July 1988 - mjd
*                 2 August 1988 - nth
*                 3 August 1988 - revised tape calls (mtlib)
*                 4 August 1988 - ytap
*  DJT            August 1991 - revised for Ryle Telescope
*                 March 1992 - revised for output to disc
*
**


**
* FITS header items

       integer bitpix
       integer blank
       integer gcount
       integer ijd0,ijda
       integer naxis,naxisn(8)
       integer pcount

       integer idate1(3), idate2(3)
       integer itime1(3), itime2(3)

       real*4 aclip

       real*8 uscal,uzero
       real*8 vscal,vzero
       real*8 wscal,wzero

       real*8 bscale,bzero
       real*8 jd0,jdascal,jdbscal,jdazero
       real*8 mjd2jd

       common/co_h/bscale,bzero,naxis,naxisn,bitpix,blank,
     :  aclip,gcount,pcount,idate1,idate2,itime1,itime2,
     :  jdazero,jdascal,jdbscal,uscal,uzero,vscal,vzero,
     :  wscal,wzero,jd0,mjd2jd,ijd0,ijda

**
* Items relating to control

       character medium*4
       character ffile*64

       common/co_medium/medium,ffile

       integer idev
       integer stap

       logical ytap

       common/co_tape/idev,stap,ytap

       character uvfile*64

       integer ddev
       integer iodev
       integer tfile
       logical vdu

       common/co_cont/uvfile,iodev,ddev,tfile,vdu

**
* Items relating to the sample file

       integer lsf_num,lun
       integer src_num
       integer nsb, nch
       integer isb(5), ich(8)

       common/co_lsf/lsf_num,lun,src_num,isb,ich,nsb,nch

**
* Items relating to tape and scratch file buffers

       integer nblock,block
       parameter (nblock=8,block=2880)

       integer lim_buff,lim_i2buff,lim_i4buff
       parameter (lim_buff=nblock*block)
       parameter (lim_i2buff=lim_buff/2)
       parameter (lim_i4buff=lim_buff/4)
       character buffer*(lim_buff)
       integer*2 i2buffer(lim_i2buff)
       equivalence (buffer,i2buffer)

       common/co_buffer/buffer

       character line*80
       integer*2 i2line(40)
       equivalence (line,i2line)

       common/co_line/line
